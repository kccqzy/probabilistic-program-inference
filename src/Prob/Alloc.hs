{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Numbering the variables of a program so that variables whose values are
-- never simultaneously needed share a number. This is register allocation. We
-- use Chaitin's algorithm to do it: variables are split into webs, webs that
-- are never live at the same time are allowed to collide, and the resulting
-- interference graph is colored greedily.
--
-- Slicing lives here too: dropping statements that write values no one reads,
-- and planting @:= False@ resets around loops to keep the number of program
-- states small. Slicing and interference are two clients of the same backward
-- liveness analysis, so one walk serves both, and the interference graph is
-- built for the sliced program — dead code does not manufacture edges.
module Prob.Alloc
  ( allocIntProg,
  )
where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Array (accum, assocs, (!))
import Data.Bifunctor
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord (Down (..), comparing)
import Data.Semigroup (Semigroup (..), stimesIdempotentMonoid)
import qualified Data.Set as Set
import Prob.CoreAST

-- | Slice a program and number its variables, letting variables share a
-- number when their values are never wanted at the same time.
allocIntProg :: (Ord vt) => Prog vt -> Prog Int
allocIntProg p = (colors M.!) <$> sliced
  where
    (sliced, g) = sliceInterfere (webProg p)
    colors = color g

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
      p' <- ReturnMult <$> webStmts ss <*> traverse webExpr es
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
joinReach :: (Ord vt) => M.Map vt Int -> M.Map vt Int -> W vt ()
joinReach a b = do
  forM_ (M.intersectionWith (,) a b) $ uncurry ufUnion
  newReach <- traverse ufFind (M.union a b)
  modify' (\s -> s {wsReach = newReach})

webStmts :: (Ord vt) => [Stmt vt] -> W vt [Stmt (Web vt)]
webStmts = traverse webStmt

webStmt :: (Ord vt) => Stmt vt -> W vt (Stmt (Web vt))
webStmt (v := e) = do
  e' <- webExpr e
  v' <- defWeb v
  pure (v' := e')
webStmt (v :~ d) = (:~ d) <$> defWeb v
webStmt (Observe e) = Observe <$> webExpr e
webStmt (If e s1 s2) = do
  e' <- webExpr e
  before <- gets wsReach
  s1' <- webStmts s1
  after1 <- gets wsReach
  modify' (\s -> s {wsReach = before})
  s2' <- webStmts s2
  after2 <- gets wsReach
  joinReach after1 after2
  pure (If e' s1' s2')
-- The loop header is a join of the entry edge and the back edge, and the back
-- edge is not known until the body has been walked, so we walk it until the
-- header stops changing. Restoring 'wsNext' before each walk makes every walk
-- hand out the same web identifiers, so the merges accumulate instead of the
-- identifiers running away; the last walk is the one we keep.
webStmt (While o e body) = do
  savedNext <- gets wsNext
  before <- gets wsReach
  let go = do
        header <- gets wsReach
        modify' (\s -> s {wsNext = savedNext})
        e' <- webExpr e
        body' <- webStmts body
        after <- gets wsReach
        joinReach before after
        header' <- gets wsReach
        header'' <- traverse ufFind header
        if header' == header''
          then pure (While o e' body')
          else go
  go

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
sliceInterfere p@(ReturnMult ss es) = (ReturnMult ss' es, clique liveIn g)
  where
    (ss', (liveIn, g)) = runState (sliceStmts ss) (Set.fromList (foldMap toList es), M.fromList [(w, Set.empty) | w <- toList p])

edge :: (Ord w) => w -> w -> Graph w -> Graph w
edge a b = M.insertWith Set.union a (Set.singleton b) . M.insertWith Set.union b (Set.singleton a)

clique :: (Ord w) => Set.Set w -> Graph w -> Graph w
clique ws g = foldl' (\g' (a, b) -> edge a b g') g [(a, b) | a <- l, b <- l, a < b]
  where
    l = Set.toList ws

-- | Insert all variables in an expression into the Live state.
gen :: (Ord w) => Expr w -> I w ()
gen = modify' . first . Set.union . Set.fromList . toList

-- | Two webs interfere when one is defined while the other is live. We do not
-- use overlapping live ranges. Intentional dead stores have an empty live range
-- and would otherwise be free to land on top of a live web.
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
  -- A web written in the loop but not live causes a large increase in program
  -- states, so it is reset to False at the end of the body and before the
  -- loop for the states entering the first iteration.
  liveEntry <- gets fst
  let loopVars = Set.unions (Set.fromList (toList e) : map (Set.fromList . toList) slicedBody)
      resetWebs = loopVars `Set.difference` liveEntry
      resetStmts = [w := Constant False | w <- Set.toList resetWebs]
  -- The reset is an extra definition of the dead web itself: after coloring it
  -- overwrites exactly the number the junk sits in. 'def' gives it edges to
  -- everything live around the loop so the reset cannot clobber a number whose
  -- value is still wanted.
  for_ resetWebs def
  pure (resetStmts ++ [While o e (slicedBody ++ resetStmts)])

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
