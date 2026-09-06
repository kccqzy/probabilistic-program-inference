{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Preparing a program for inference. Three things happen here, in this
-- order: the statements of each block are reordered ("Scheduling"), the
-- statements nobody reads are dropped ("Slicing and interference"), and the
-- variables are numbered so that ones never simultaneously needed share a
-- number ("Coloring").
--
-- The numbering is register allocation, done by Chaitin's algorithm: variables
-- are split into webs, webs that are never live at the same time are allowed to
-- collide, and the resulting interference graph is colored greedily. Slicing
-- and interference are two clients of the same backward liveness analysis, so
-- one walk serves both, and the interference graph is built for the sliced
-- program.
--
-- A word on what all of this is for, because it is easy to optimise the wrong
-- thing. What we are trying to make small is the work 'Prob.Den' does, which is
-- the number of (statement, state) pairs it handles. The number of variables
-- that come out of the coloring is /not/ that quantity and is not a reliable
-- proxy. Few variables are worth having because states are 'Prob.CoreAST.Sigma'
-- values used as map keys, and because narrower states merge more often, but
-- that makes the variable count a secondary constraint (i.e. the time
-- complexity of Sigma operations).
module Prob.Alloc
  ( allocIntProg,
  )
where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Array (accum, assocs, (!))
import Data.Bifunctor
import Data.Foldable
import Data.Graph (buildG, components, indegree)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (sort)
import qualified Data.Map.Merge.Set.Strict as MMS
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord (Down (..), comparing)
import Data.Semigroup (Semigroup (..), stimesIdempotentMonoid)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tree (flatten)
import Data.Tuple (swap)
import Prob.CoreAST
import Prob.Liveness (liveIn, liveIns)

-- | Slice a program and number its variables, letting variables share a
-- number when their values are never wanted at the same time.
allocIntProg :: (Ord vt) => Prog vt -> Prog Int
allocIntProg p = relabelLoops ((colors M.!) <$> parted)
  where
    (sliced, g) = sliceInterfere (scheduleProg (webProg p))
    (parted, g') = partitionLoops sliced g
    colors = color g'

-- | Give the loops labels that are equal iff when the loops are identical.
-- "Prob.Den" caches solved loop kernels by label, and a kernel is a property of
-- the guard and body alone, so identical loops (the two copies a do-while
-- desugars into, typically) may share one and different loops must not. The
-- parser's labels only say which loops were identical before desugaring; after
-- optimization (especially constant propagation and copy propagation) and color
-- allocation, identical source-level loops need not remain identical.
relabelLoops :: Prog Int -> Prog Int
relabelLoops (ss `ReturnMult` es) = evalState (traverse go ss) Seq.empty `ReturnMult` es
  where
    go :: Stmt Int -> State (Seq.Seq (Expr Int, [Stmt Int])) (Stmt Int)
    go (If e s1 s2) = If e <$> traverse go s1 <*> traverse go s2
    go (Par o bs) = Par o <$> traverse (traverse go) bs
    go (While _ e body) = do
      -- Inner loops first, so the key of the outer loop contains their new
      -- labels.
      body' <- traverse go body
      let content = (e, body')
      known <- gets (content `Seq.elemIndexL`)
      l <- case known of
        Just l -> pure l
        Nothing -> state (\m -> (Seq.length m, m Seq.|> content))
      pure (While l e body')
    go s = pure s

--------------------------------------------------------------------------------
-- Webs
--------------------------------------------------------------------------------

-- | A web: a source variable paired with the identity of the group of
-- definitions that reach it. Two occurrences of one source variable in
-- different webs hold unrelated values so they are free to be numbered apart.
-- Example: if we have @a:= 1; ... use a ...; a := 2;@ then the two uses of @a@
-- are distinct and belong to different webs.
type Web vt = (vt, Int)

-- | Union-find over web identifiers. The root of a class is its smallest
-- member, which keeps the numbering independent of the order the merges
-- happened in.
type UF = IM.IntMap Int

data WebState vt = WebState
  { wsNext :: Int,
    wsUF :: UF,
    -- | The definition currently reaching each variable.
    wsReach :: M.Map vt Int
  }

type W vt = State (WebState vt)

ufFind :: Int -> W vt Int
ufFind x = do
  r <- gets (IM.lookup x . wsUF)
  case r of
    Just p | p /= x -> do
      root <- ufFind p
      when (root /= p) $ modify' (\s -> s {wsUF = IM.insert x root (wsUF s)})
      pure root
    _ -> pure x

ufUnion :: Int -> Int -> W vt ()
ufUnion a b = do
  ra <- ufFind a
  rb <- ufFind b
  when (ra /= rb) $ modify' (\s -> s {wsUF = IM.insert (max ra rb) (min ra rb) (wsUF s)})

-- | Rename every occurrence of every variable to the web it belongs to. A
-- definition starts a fresh web; control flow joins merge the webs arriving
-- along each edge, so a use ends up in one web with every definition that can
-- reach it.
webProg :: (Ord vt) => Prog vt -> Prog (Web vt)
webProg p@(ReturnMult ss es) = evalState go initial
  where
    varsSet = Set.fromList (toList p)
    vars = Set.toAscList varsSet
    -- Execution starts with every variable false, so entry is a definition of
    -- all of them.
    initial = WebState (Set.size varsSet) IM.empty (M.fromDistinctAscList (zip vars [0 ..]))
    go = do
      p' <- ReturnMult <$> webStmts (Set.fromList (foldMap toList es)) ss <*> traverse webExpr es
      traverse (\(v, i) -> (v,) <$> ufFind i) p'

-- | Give a variable a brand new web, as it is being redefined.
defWeb :: (Ord vt) => vt -> W vt (Web vt)
defWeb v = do
  i <- state (\s -> (wsNext s, s {wsNext = wsNext s + 1}))
  modify' (\s -> s {wsReach = M.insert v i (wsReach s)})
  pure (v, i)

webExpr :: (Ord vt) => Expr vt -> W vt (Expr (Web vt))
webExpr expr = do
  reach <- gets wsReach
  -- Every variable is in 'wsReach' from the start, so the 'M.!' is safe.
  pure $ (\v -> (v,) $! reach M.! v) <$> expr

-- | Merge two reaching-definition maps, as happens where control flow joins.
--
-- Only variables live at the join are merged. A variable dead will not be read,
-- so the definitions arriving are unrelated values and may stay separate webs.
-- Merging them anyway is not wrong, but it ties statements together that have
-- nothing to do with each other. This often happens since the desugarer reuses
-- temporaries. TODO: re-evaluate the decision of the desugarer reusing
-- temporaries.
joinReach :: (Ord vt) => Set.Set vt -> M.Map vt Int -> M.Map vt Int -> W vt ()
joinReach live a b = do
  for_ (M.intersectionWith (,) (M.restrictKeys a live) b) $ uncurry ufUnion
  newReach <- traverse ufFind (M.union a b)
  modify' (\s -> s {wsReach = newReach})

-- | Walk a block whose live-out set is given.
webStmts :: (Ord vt) => Set.Set vt -> [Stmt vt] -> W vt [Stmt (Web vt)]
webStmts out ss = zipWithM webStmt (drop 1 (scanr liveIn out ss)) ss

-- | Walk a statement whose live-out set is given.
webStmt :: (Ord vt) => Set.Set vt -> Stmt vt -> W vt (Stmt (Web vt))
webStmt _ (v := e) = do
  e' <- webExpr e
  v' <- defWeb v
  pure (v' := e')
webStmt _ (v :~ d) = (:~ d) <$> defWeb v
webStmt _ (Observe e) = Observe <$> webExpr e
webStmt out (If e s1 s2) = do
  e' <- webExpr e
  before <- gets wsReach
  s1' <- webStmts out s1
  after1 <- gets wsReach
  modify' (\s -> s {wsReach = before})
  s2' <- webStmts out s2
  after2 <- gets wsReach
  joinReach out after1 after2
  pure (If e' s1' s2')
-- The loop header is a join of the entry edge and the back edge, and the back
-- edge is not known until the body has been walked, so we walk it until the
-- header stops changing. Restoring 'wsNext' before each walk makes every walk
-- hand out the same web identifiers, so the merges accumulate instead of the
-- identifiers running away; the last walk is the one we keep.
webStmt out loop@(While o e body) = do
  savedNext <- gets wsNext
  before <- gets wsReach
  let atHeader = liveIn loop out
      go = do
        header <- gets wsReach
        modify' (\s -> s {wsNext = savedNext})
        e' <- webExpr e
        body' <- webStmts atHeader body
        after <- gets wsReach
        joinReach atHeader before after
        header' <- gets wsReach
        header'' <- traverse ufFind header
        if header' == header''
          then pure (While o e' body')
          else go
  go
-- The blocks commute, so walking them in sequence is fine.
webStmt out (Par o blocks) =
  Par o <$> zipWithM webStmts (drop 1 (scanr liveIns out blocks)) blocks

--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

-- | Reorder the statements of every block so that inference has less work to
-- do.
--
-- Statements touching disjoint webs always commute, samples and loops included.
-- The legal orders of a block are therefore the linear extensions of the
-- partial order where two statements are ordered in the original written order
-- if they share a web, and unordered otherwise. Sharing any web at all is
-- treated as a dependence, which is a bit coarser than necessary but is safe.
-- (I have difficulty formulating a more fine-grained treatment.)
--
-- We use a greedy algorithm to pick statements within a block, because doing it
-- exactly is NP-hard. The greedy algorithm essentially picks the most highly
-- ranked statement with the following goal of minimizing the amount of state,
-- roughly speaking. This is roughly measured by the number of program states
-- (Distr) handled each time a statement is handled. It must be noted that this
-- is not the total number of variables; the total number of variables only
-- affect the cost of IntSet lookups, inserts, and compares, and it is a tiny
-- factor. This is how each statement is ranked:
--
-- * @x :~ Bernoulli p@ splits every state in two, so in the worst case it
--   doubles the number of states and is the /only/ statement kind that can grow
--   it.
--
-- * @x := e@ applies a function to each state. It can never grow the number of
--   states, and shrinks it whenever two states are mapped together which is
--   common.
--
-- * @observe e@ deletes states, so in the best case it shrinks states, and in
--   the worst case it just keeps things the same.
--
-- * A statement that uses a web lets that web die. Two states differing only in
--   a dead web become one after coloring, so deaths shrink the support too.
--   This is what the cut is tracking.
--
-- Therefore we do this:
--
-- 1. __Observes first.__ An observe costs one pass over the distribution it is
--    handed and deletes states from it.
--
-- 2. __Then anything that does not introduce randomness.__ Deferring a :~
--    means every deterministic statement that does not depend on it runs
--    before the split rather than after, at half the cost.
--
-- 3. __Then the smallest cut.__ Among statements that are alike in the two
--    respects above, prefer the one leaving fewest webs live, which is the one
--    most likely to have merged states by killing something.
--
-- 4. __Then the most webs already live.__ Finish a chain that has been started
--    before beginning an unrelated one, rather than leaving several half-built.
--
-- 5. __Finally whichever was written first.__ When we can't decide, let the
--    user decide. The pass is deterministic.
scheduleProg :: (Ord w) => Prog w -> Prog w
scheduleProg (ReturnMult ss es) = ReturnMult (snd (scheduleStmts (Set.fromList (foldMap toList es)) ss)) es

-- | @scheduleStmts out ss@ schedules a block whose live-out set is @out@,
-- returning its live-in set alongside.
scheduleStmts :: (Ord w) => Set.Set w -> [Stmt w] -> (Set.Set w, [Stmt w])
scheduleStmts out ss = (live, greedy)
  where
    (live, nested) = foldr (\s (l, acc) -> (: acc) <$> scheduleStmt l s) (out, []) ss
    -- Scheduling is just reordering the statements, and so it will not change
    -- the live set. So it is safe to do the liveIn computation first, and
    -- reordering the statements won't change the live set.

    stmts = IM.fromDistinctAscList (zip [0 ..] nested)
    webs = Set.fromList . toList <$> stmts
    -- The dependence order graph.
    deps =
      buildG (0, IM.size webs - 1) $
        [ (i, j)
        | (j, wj) <- IM.toList webs,
          (i, wi) <- IM.toList (fst (IM.split j webs)),
          not (Set.disjoint wi wj)
        ]
    indeg = indegree deps
    uses = M.unionsWith (+) (M.fromSet (const (1 :: Int)) <$> IM.elems webs)
    orderingPreferences = orderingPreference <$> stmts

    greedy = (stmts IM.!) <$> go (IS.fromList [i | (i, 0) <- assocs indeg]) indeg (Set.filter stillLive live) uses
      where
        stillLive w = w `Set.member` out || w `M.member` uses
    go ready waiting open remaining
      | IS.null ready = []
      | otherwise = pick : go ready' waiting' open' remaining'
      where
        commit i = (Set.filter stillLive (open `Set.union` wi), left, Set.size (Set.intersection wi open))
          where
            wi = webs IM.! i
            left = MMS.merge MMS.preserveMissing MMS.dropMissingSet (MMS.mapMaybeMatched (\_ c -> if c == 1 then Nothing else Just (c - 1))) remaining wi
            stillLive w = w `Set.member` out || w `M.member` left
        (pick, (open', remaining', _)) =
          minimumBy (comparing (\(i, (o, _, shared)) -> (orderingPreferences IM.! i, Set.size o, Down shared, i))) [(i, commit i) | i <- IS.toList ready]
        released = deps ! pick
        waiting' = accum (+) waiting [(j, -1) | j <- released]
        ready' = IS.delete pick ready `IS.union` IS.fromList (filter ((== 0) . (waiting' !)) released)

-- | The order we pick a statement based on what a statement can do to the
-- number of states in the distribution. The 'Ord' instance is the order of
-- preference.
data OrderingPreference = PreferEarly | Neutral | PreferLate deriving (Eq, Ord)

instance Semigroup OrderingPreference where
  (<>) = max
  stimes = stimesIdempotentMonoid

instance Monoid OrderingPreference where
  mconcat = go PreferEarly
    where
      -- This is basically 'Data.Foldable.maximum' but safe on empty lists and has
      -- early exit.
      go acc [] = acc
      go PreferLate _ = PreferLate
      go acc (x : xs) = go (max acc x) xs

-- | Classify a statement.
orderingPreference :: Stmt w -> OrderingPreference
orderingPreference s = case s of
  Observe _ -> PreferEarly
  _ :~ _ -> PreferLate
  _ := _ -> Neutral
  If _ s1 s2 ->
    -- Written this way to avoid ++.
    mconcat (mconcat (map orderingPreference s1) : map orderingPreference s2)
  While _ _ ss -> mconcat (map orderingPreference ss)
  Par _ blocks -> mconcat (map (mconcat . map orderingPreference) blocks)

-- | Schedule the blocks nested in a statement, and report the statement's
-- live-in set given its live-out set. Scheduling never changes a live set,
-- so the set is just 'liveIn'.
scheduleStmt :: (Ord w) => Set.Set w -> Stmt w -> (Set.Set w, Stmt w)
scheduleStmt out s = (liveIn s out, reordered)
  where
    reordered =
      case s of
        If e s1 s2 -> If e (snd (scheduleStmts out s1)) (snd (scheduleStmts out s2))
        While o e body -> While o e (snd (scheduleStmts (liveIn s out) body))
        -- Each block is scheduled on its own, with the live-out it would have
        -- in sequence.
        Par o blocks -> Par o (zipWith (\l b -> snd (scheduleStmts l b)) (drop 1 (scanr liveIns out blocks)) blocks)
        _ -> s

--------------------------------------------------------------------------------
-- Slicing and interference
--------------------------------------------------------------------------------

type Live w = Set.Set w

-- | Which webs may not share a number, as an adjacency map that has an entry
-- for every web in the program.
type Graph w = M.Map w (Set.Set w)

type I w = State (Live w, Graph w)

-- | One backward walk that performs liveness to simultaneously calculate which
-- statements are dead and which statements will interfere with each other. For
-- the dead statements, they are removed; for the interference, the interference
-- will be recorded for accurate coloring.
sliceInterfere :: (Ord w) => Prog w -> (Prog w, Graph w)
sliceInterfere p@(ReturnMult ss es) = (ReturnMult ss' es, clique live g)
  where
    (ss', (live, g)) = runState (sliceStmts ss) (Set.fromList (foldMap toList es), M.fromList [(w, Set.empty) | w <- toList p])

edge :: (Ord w) => w -> w -> Graph w -> Graph w
edge a b = M.insertWith Set.union a (Set.singleton b) . M.insertWith Set.union b (Set.singleton a)

clique :: (Ord w) => Set.Set w -> Graph w -> Graph w
clique ws g = foldl' (\g' (a, b) -> edge a b g') g [(a, b) | a <- l, b <- l, a < b]
  where
    l = Set.toList ws

-- | Insert all variables in an expression into the Live state.
gen :: (Ord w) => Expr w -> I w ()
gen = modify' . first . Set.union . Set.fromList . toList

-- | Two webs interfere when one is defined while the other is live.
def :: (Ord w) => w -> I w ()
def x = modify' (\(l, g) -> let out = Set.delete x l in (out, foldl' (flip (edge x)) g (Set.toList out)))

sliceStmts :: (Ord w) => [Stmt w] -> I w [Stmt w]
sliceStmts = foldrM step [] -- Liveness is backwards, so we use foldrM.
  where
    step s kept = (++ kept) <$> sliceStmt s

sliceStmt :: (Ord w) => Stmt w -> I w [Stmt w]
-- An observe changes the normalization of the whole program, so it is always
-- kept.
sliceStmt s@(Observe e) = gen e >> pure [s]
sliceStmt s@(x := e) = do
  wanted <- gets ((x `Set.member`) . fst)
  if wanted then def x >> gen e >> pure [s] else pure []
sliceStmt s@(x :~ _) = do
  wanted <- gets ((x `Set.member`) . fst)
  if wanted then def x >> pure [s] else pure []
sliceStmt (If e s1 s2) = do
  (out, _) <- get
  k1 <- sliceStmts s1
  (l1, _) <- get
  modify' (\(_, g) -> (out, g))
  k2 <- sliceStmts s2
  if null k1 && null k2
    then pure []
    else do
      modify' (\(l2, g) -> (Set.unions [Set.fromList (toList e), l1, l2], g))
      pure [If e k1 k2]
-- Every loop is kept. A loop that might diverge removes probability mass
-- conditioned on the state it was entered in, so deleting it changes the
-- answer even when nothing reads what it writes: @x ~ bernoulli 0.5; while
-- x do {}; return x@ answers "false with probability 1", but would answer
-- uniform were the loop sliced away.
sliceStmt (While o e body) = do
  (out, _) <- get
  let live0 = out `Set.union` Set.fromList (toList e)
      go = do
        (prev, _) <- get
        r <- sliceStmts body
        modify' (\(l, g) -> (l `Set.union` live0, g))
        (next, _) <- get
        if prev == next then pure r else go
  modify' (\(_, g) -> (live0, g))
  slicedBody <- go
  -- Historically, we used to reset what's written in the loop but dead at its
  -- head here, at the end of the body and again before the loop, so that states
  -- differing only in these dead states would be recognized as the same state
  -- at the loop head. 'Prob.Den' now performs the carried analysis: it projects
  -- each arriving state onto the loop's carried set, which excludes precisely
  -- those the body overwrites before reading.
  pure [While o e slicedBody]
-- Backwards through the blocks as if in sequence. Slicing can only remove
-- statements, so it keeps the invariant; a node left with one block is
-- unwrapped.
sliceStmt (Par o blocks) = do
  blocks' <- foldrM (\b acc -> (: acc) <$> sliceStmts b) [] blocks
  pure $ case filter (not . null) blocks' of
    [] -> []
    [b] -> b
    bs -> [Par o bs]

--------------------------------------------------------------------------------
-- Partitioning loop bodies
--------------------------------------------------------------------------------

-- | Split the body of every loop into a 'Par' of independent blocks followed
-- by the statements that combine them, and add the interference edges that
-- keep the blocks' webs apart through coloring, so the 'Par' invariant holds
-- of the numbered program too.
--
-- The body of a loop often has independent parts that meet only in the guard:
-- say we have the actual stochastic procedure being modeled and the iteration
-- counter, where the guard reads both. Inference unrolls the loop once per
-- distinct loop-head state, so without this it computes the stochastic
-- procedure once per counter value. With the body split, "Prob.Den" computes
-- each block's step once per distinct state of that block's own variables and
-- multiplies.
--
-- The split has three steps, all on the sliced program, so dead statements
-- do not tie parts together:
--
-- 1. __The join.__ A statement is in the /cone/ of the guard when every body
--    statement that reads what it writes is in the cone too — so its results
--    reach the next iteration only through the guard, if at all. The join is
--    the longest run of cone statements that ends the body. Taking a suffix
--    rather than the whole cone keeps every statement in its place relative
--    to whatever it depends on.
--
-- 2. __The blocks.__ The rest of the body is split into connected components
--    under "mention a common web that the body writes". Webs the body never
--    writes are loop invariants, and reading one from two blocks is harmless.
--
-- 3. __Sinking.__ A join statement that reads from a single block, such as
--    "the cars are all parked", is appended to that block instead, where it
--    will be computed once per block state rather than once per product
--    state. It is moved only past join statements it does not depend on.
--
-- A body that does not split into at least two blocks is left exactly as it
-- was. Inner loops are split before the loop around them.
partitionLoops :: (Ord w) => Prog w -> Graph w -> (Prog w, Graph w)
partitionLoops (ReturnMult ss es) g = (ReturnMult ss' es, g')
  where
    (ss', (_, g')) = runState (traverse partStmt ss) (0, g)

partStmt :: (Ord w) => Stmt w -> State (Int, Graph w) (Stmt w)
partStmt (If e s1 s2) = If e <$> traverse partStmt s1 <*> traverse partStmt s2
partStmt (Par o blocks) = Par o <$> traverse (traverse partStmt) blocks
partStmt (While o e body) = do
  body' <- traverse partStmt body
  case partitionBody body' of
    Nothing -> pure (While o e body')
    Just (blocks, joinStmts) -> do
      (n, g) <- get
      let cross =
            [ (a, b)
            | (i, bi) <- zip [0 :: Int ..] blocks,
              (j, bj) <- zip [0 ..] blocks,
              i /= j,
              a <- concatMap stmtWrites bi,
              b <- concatMap toList bj,
              a /= b
            ]
      put (n + 1, foldl' (\g' (a, b) -> edge a b g') g cross)
      pure (While o e (Par n blocks : joinStmts))
partStmt s = pure s

-- | Where a web's value comes from during sinking: a block, or a statement
-- that stays in the join.
data Owner = Block Int | Join deriving (Eq)

-- | Split one loop body into blocks and join, or 'Nothing' if it has fewer
-- than two blocks.
partitionBody :: (Ord w) => [Stmt w] -> Maybe ([[Stmt w]], [Stmt w])
partitionBody body
  | length comps < 2 = Nothing
  | otherwise = Just (blocks, map (stmts IM.!) (reverse retained))
  where
    n = length body
    stmts = IM.fromDistinctAscList (zip [0 ..] body)
    readsOf = Set.fromList . stmtReads <$> stmts
    writesOf = Set.fromList . stmtWrites <$> stmts
    mentionsOf = Set.fromList . toList <$> stmts
    bodyWrites = Set.unions (IM.elems writesOf)
    readers = M.unionsWith IS.union [M.fromSet (const (IS.singleton i)) rs | (i, rs) <- IM.toList readsOf]
    readersOf w = M.findWithDefault IS.empty w readers

    -- Step 1. The cone is a least fixed point: joining it only ever makes it
    -- easier for others to join.
    cone = grow IS.empty
      where
        grow c
          | c' == c = c
          | otherwise = grow c'
          where
            c' = IM.keysSet (IM.filter (all (\w -> readersOf w `IS.isSubsetOf` c)) writesOf)
    suffixStart = n - length (takeWhile (`IS.member` cone) [n - 1, n - 2 .. 0])
    prefixIdx = [0 .. suffixStart - 1]

    -- Step 2. Statements mentioning a common body-written web are chained.
    prefixWrites = Set.unions [writesOf IM.! i | i <- prefixIdx]
    chains =
      [ (a, b)
      | w <- Set.toList prefixWrites,
        let is = [i | i <- prefixIdx, w `Set.member` (mentionsOf IM.! i)],
        (a, b) <- zip is (drop 1 is)
      ]
    comps
      | null prefixIdx = []
      | otherwise = map (sort . flatten) (components (buildG (0, suffixStart - 1) (chains ++ map swap chains)))
    owner0 = M.fromList [(w, Block k) | (k, is) <- zip [0 ..] comps, i <- is, w <- Set.toList (writesOf IM.! i)]

    -- Step 3. Walk the join in order. A statement sinks into block k when
    -- every body-written web it reads is owned by k, nothing it writes is
    -- owned elsewhere, and no retained statement reads what it writes; it
    -- then comes after everything it depends on and before nothing that
    -- depends on it. Otherwise it stays, and owns what it writes.
    (_, _, sunk, retained) = foldl' place (owner0, Set.empty, IM.empty, []) [suffixStart .. n - 1]
    place (owner, retainedReads, sunkAcc, retainedAcc) i =
      case target of
        Just k ->
          ( M.fromSet (const (Block k)) ws `M.union` owner,
            retainedReads,
            IM.insertWith (flip (++)) k [i] sunkAcc,
            retainedAcc
          )
        Nothing ->
          ( M.fromSet (const Join) ws `M.union` owner,
            retainedReads `Set.union` (readsOf IM.! i),
            sunkAcc,
            i : retainedAcc
          )
      where
        rs = Set.toList (Set.intersection (readsOf IM.! i) bodyWrites)
        ws = writesOf IM.! i
        readOwners = map (`M.lookup` owner) rs
        writeOwners = mapMaybe (`M.lookup` owner) (Set.toList ws)
        target =
          case readOwners of
            Just (Block k) : _
              | all (== Just (Block k)) readOwners,
                all (== Block k) writeOwners,
                not (any (`Set.member` retainedReads) ws) ->
                  Just k
            _ -> Nothing
    blocks = [map (stmts IM.!) (is ++ IM.findWithDefault [] k sunk) | (k, is) <- zip [0 ..] comps]

--------------------------------------------------------------------------------
-- Coloring
--------------------------------------------------------------------------------

-- | Color greedily in smallest-last order: strip the least constrained web
-- off the graph repeatedly, then put them back in the reverse order, each
-- taking the smallest number none of its neighbors has.
color :: (Ord w) => Graph w -> M.Map w Int
color g = foldl' assign M.empty (smallestLast g)
  where
    assign m w =
      let neighbors = toList (M.findWithDefault Set.empty w g)
          used = IS.fromList (mapMaybe (`M.lookup` m) neighbors)
          -- The neighbors use at most 'IS.size used' distinct colors, so among the
          -- first size+1 colors there must be one that is unused.
          lowestFree = IS.findMin (IS.fromRange (0, IS.size used) `IS.difference` used)
       in M.insert w lowestFree m

-- | Repeatedly strip the least constrained web (i.e. the one with the fewest
-- neighbors) and order them in reverse. This is not just indegree/outdegree:
-- after we remove each vertex we re-calculate the number of neighbors.
smallestLast :: (Ord w) => Graph w -> [w]
smallestLast = go []
  where
    go acc g
      | M.null g = acc
      | otherwise = go (v : acc) (M.map (Set.delete v) (M.delete v g))
      where
        (v, _) = minimumBy (comparing (Set.size . snd)) (M.toAscList g)
