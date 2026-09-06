{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Denotational semantics using traditional normalized/unnormalized semantics.
module Prob.Den
  ( denProg
  , denProgStats
  , InferStats(..)
  , InferLoopStats(..)
  , InferParStats(..)
  ) where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Prob.CoreAST
import qualified Prob.LinearEq as L
import Prob.Liveness (liveIn, liveIns)

--------------------------------------------------------------------------------
-- Denotational Semantics
--------------------------------------------------------------------------------

-- | A (sparse, unnormalized) distribution over states, supported on the
-- reachable states. This is what 'denStmts' both consumes and produces: the
-- whole distribution is pushed through the program in one pass.
type Distr s = M.Map s Rational

denExpr :: Expr Int -> Sigma -> Bool
denExpr (Var x) sigma = IS.member x sigma
denExpr (Constant d) _ = d
denExpr (Or a b) sigma = denExpr a sigma || denExpr b sigma
denExpr (And a b) sigma = denExpr a sigma && denExpr b sigma
denExpr (Xor a b) sigma = denExpr a sigma /= denExpr b sigma
denExpr (Not a) sigma = not (denExpr a sigma)

-- | The loop currently being unrolled, identified by its label. Labels are
-- equal exactly when the 'While' nodes are structurally identical: that is
-- how "Prob.Alloc" assigns them, and on an unoptimized program the parser's
-- labels (the source offset of the @while@ keyword) have the property too,
-- since the only copies are the two a do-while desugars into. Sharing kernels
-- between identical loops is sound because the kernel is solved
-- independently of what follows the loop.
--
-- The states here specifically the seen set is not full loop-head states but
-- their /carried/ parts: states that satisfy the guard that can additionally be
-- observed in the next iteration. This means if a loop writes to a variable but
-- does not read from it, it is not present.
data CurrentLoop = CurrentLoop
  { clLabel :: Int
  , clSeenSigma :: Set.Set Sigma
  , clEqns :: [(Sigma, Ret)]
  }

-- | What statements do to the state, conservatively: which variables it may
-- read before it has written them, and which it is certain to have written by
-- the end. These two might not be disjoint sets (See 'findCarried'.)
data Flow = Flow
  { flowMayReadBeforeWrite :: IS.IntSet
  , flowMustWrite :: IS.IntSet
  }

-- | Running @a@ and then @b@: a read in @b@ may only be read if @a@ had not
-- already written that variable. Clearly not commutative.
instance Semigroup Flow where
  Flow e1 w1 <> Flow e2 w2 = Flow (e1 `IS.union` (e2 IS.\\ w1)) (w1 `IS.union` w2)

instance Monoid Flow where
  mempty = Flow IS.empty IS.empty

-- | Construct the Flow for a single statement.
flowStmt :: Stmt Int -> Flow
flowStmt (x := e) = Flow (IS.fromList (toList e)) (IS.singleton x)
flowStmt (x :~ _) = Flow IS.empty (IS.singleton x)
flowStmt (Observe e) = Flow (IS.fromList (toList e)) IS.empty
flowStmt (If e s1 s2) =
  Flow
    (IS.unions [IS.fromList (toList e), flowMayReadBeforeWrite f1, flowMayReadBeforeWrite f2])
    -- Only what both arms are certain to write is certain to be written.
    (flowMustWrite f1 `IS.intersection` flowMustWrite f2)
  where
    f1 = foldMap flowStmt s1
    f2 = foldMap flowStmt s2
-- A loop may run zero times, so it is certain to write nothing. The
-- read-before-write set is the guard plus one pass of the body: a variable the
-- body writes in an earlier iteration is not relevant here.
flowStmt (While _ e body) =
  Flow (IS.fromList (toList e) `IS.union` flowMayReadBeforeWrite (foldMap flowStmt body)) IS.empty
-- The blocks commute, so their sequential flow is fine.
flowStmt (Par _ blocks) = foldMap (foldMap flowStmt) blocks

-- | Find the carried set of a loop. The carried set is defined as the subset of
-- the footprint of the loop such that any variables that are always written
-- before read are excluded.
findCarried :: IS.IntSet -> [Stmt Int] -> IS.IntSet
findCarried footprint stmts = footprint IS.\\ (mustWrite IS.\\ mayRead)
  where Flow mayRead mustWrite = foldMap flowStmt stmts

-- | Find the counter of a loop. The counter is defined as the set of carried
-- variables written by the sample-free blocks of the body's 'Par'. Intuitively,
-- this is a generalization of the typical counter used in a three-statement
-- C-style loop @for (i = 0; i < 5; ++i)@ where the loop body does not write to
-- @i@ at all. Here, the counter is the part of the loop that does not interact
-- with other stochastic variables. This actually allows more sophisticated
-- counters as long as they are fully deterministic.
findCounter :: IS.IntSet -> [Stmt Int] -> IS.IntSet
findCounter carried stmts = IS.intersection carried (IS.fromList (concatMap (concatMap stmtWrites) (filter (not . any stmtSamples) blocks)))
  where
    blocks =
      case stmts of
        Par _ bs : remainder ->
          -- Anything after the 'Par' is the join, which must not write anything carried.
          if all (IS.disjoint carried . IS.fromList . stmtWrites) remainder
          then bs
          else error "internal error: post-Par block join must not write carried"
        _ -> [stmts]
          -- A block that is not split counts as one block. The entire block may qualify as a counter.

-- | What inference actually cost, as opposed to what a static estimate of it
-- predicted. 'denStmts' pushes a whole distribution through one statement at a
-- time, so the time it takes is largely proportional to 'isStatesPushed': the
-- number of (statement, state) pairs it worked on.
data InferStats = InferStats
  { -- | Statements handled, counting each one once per time it is run:
    -- a loop unrolled many times is counted many times.
    isStmtsRun :: Int
    -- | The number of states handled. Each time denStmt runs a statement, it
    -- may handle multiple states at once.
  , isStatesPushed :: Int
    -- | The largest distribution any single statement saw. Related to the peak
    -- live set, which bounds it above by two to the power of the live count.
  , isLargestDistr :: Int
  , isLoopStats :: IM.IntMap InferLoopStats
  }

data InferLoopStats = InferLoopStats
  { ilsFootprintSize, ilsCarriedSize, ilsKernels, ilsDeterministicSteps :: Int
    -- | The 'Par' nodes in this loop's body, by label. A node nested in an
    -- inner loop belongs to that inner loop, not to this one.
  , ilsPars :: IM.IntMap InferParStats
  }

-- | What one 'Par' node cost. The three per-block lists are in block order and
-- all have 'ipsBlocks' entries.
data InferParStats = InferParStats
  { -- | How many blocks the loop body was split into.
    ipsBlocks :: Int
    -- | How many times the node ran. One per iteration it took part in.
  , ipsRuns :: Int
    -- | States arriving at the node, summed over its runs, and states leaving
    -- it. The ratio is how much the node fans the distribution out.
  , ipsStatesIn, ipsStatesOut :: Int
    -- | Per block: how many variables it mentions.
  , ipsFootprints :: [Int]
    -- | Per block: how many distinct projections of the state it was solved
    -- for. This is the block's cache size, and the analogue of
    -- 'ilsKernels' — the whole point of the split is that these stay small
    -- where their product would not.
  , ipsSolved :: [Int]
    -- | Per block: the most states any one of those solves produced. The
    -- product of these bounds how far one arriving state can fan out.
  , ipsFanOut :: [Int]
  }

data LoopInfo = LoopInfo
  { liFootprint :: IS.IntSet
    -- ^ The variables mentioned by the loop.
  , liCarried :: IS.IntSet
    -- ^ The variables inside the loop body that need to be persisted from one
    -- iteration to the next. The loop's own guard is deliberately not part of
    -- the carried set. It is allowed for a loop guard to read some variables
    -- that are not read again in the loop itself. Such variables may safely be
    -- disregarded.
  , liCounter :: IS.IntSet
    -- ^ The carried variables the body updates deterministically and on their
    -- own: those written by the sample-free blocks of the body's 'Par' (or
    -- by the whole body, if it is sample-free). Their next value is a
    -- function of their current value, so every state alive after @t@
    -- iterations has the counter value @f^t@ of where it started, and a state
    -- can only recur once a counter value has. Empty when there is no such
    -- part.
  , liKernels :: M.Map Sigma (Distr Sigma)
  , liDeterministicSteps :: Int
  }

-- | The state threaded through the denotation: the loop currently being
-- unrolled (if any), the footprint and carried set of every loop in the
-- program, the cache of already-solved loop kernels, and the running cost
-- counters. Only 'dsCurrentLoop' is saved and restored around a loop solve;
-- the footprints, the carried sets, the kernel cache and the counters only
-- ever grow.
data DenState = DenState
  { dsCurrentLoop :: Maybe CurrentLoop
  , dsLoopInfo :: IM.IntMap LoopInfo
  , dsParInfo :: IM.IntMap ParInfo
  , dsParLive :: IM.IntMap IS.IntSet
    -- ^ The variables live after each 'Par' node, from 'parLiveOuts'.
  , dsStmtsRun :: Int
  , dsStatesPushed :: Int
  , dsLargestDistr :: Int
  }

-- | What is known about a 'Par' node, by its label: for each block, the
-- variables it mentions, the variables it writes, and those of its writes
-- that are still live after the node; and the block's cache of transitions. A
-- cache entry maps a state projected onto the block's footprint to what the
-- block does to it, restricted to those live written variables; the variables
-- it only reads are supplied by the caller. The writes that are not live are
-- wanted too: they leave the state at the node even though they are no part
-- of the block's answer.
data ParInfo = ParInfo
  { piBlocks :: [(IS.IntSet, IS.IntSet, IS.IntSet)]
  , piCaches :: IM.IntMap (M.Map Sigma (Distr Sigma))
    -- | Cost counters, for 'InferParStats'.
  , piRuns, piStatesIn, piStatesOut :: Int
  }

type Den = State DenState

-- | The denotation of a statement list: an (unnormalized) distribution over
-- ending states, plus — while inside a loop — symbolic references to loop states
-- (the 'L.Term's) that 'L.solveRows' later resolves. At the top level the
-- reference list is empty and a 'Ret' is just a 'Distr'.
data Ret = Ret (Distr Sigma) [L.Term Sigma]

mergeDistr :: (Sigma -> Sigma) -> Distr Sigma -> Distr Sigma
mergeDistr = M.mapKeysWith (+)

-- | The denotation of a statement list, as a transformer of whole
-- distributions.
--
-- Historically this function worked one state at a time. But this is too
-- costly. Walking a single state costs a traversal of the remaining program per
-- execution /path/, and two paths that have arrived at the same state are only
-- noticed once both of their suffixes have been walked in full and their
-- distributions added together on the way back up.
denStmts :: [Stmt Int] -> Distr Sigma -> Den Ret
denStmts _ d | M.null d =
               -- Nothing left to push. This is caused by a failed observe.
               pure (Ret M.empty [])
denStmts [] d = pure (Ret d [])
denStmts (s:next) d = updateStats (M.size d) >> denStmt s next d

updateStats :: Int -> Den ()
updateStats n =
  modify'
    (\ds ->
       ds
         { dsStmtsRun = dsStmtsRun ds + 1
         , dsStatesPushed = dsStatesPushed ds + n
         , dsLargestDistr = max (dsLargestDistr ds) n
         })

denStmt :: Stmt Int -> [Stmt Int] -> Distr Sigma -> Den Ret
denStmt (x := e) next d =
  denStmts next (mergeDistr (\sigma -> sigmaInsert x (denExpr e sigma) sigma) d)
denStmt (x :~ Bernoulli theta) next d =
  -- Here, we ensure that no matter which branch is taken, the rest of the
  -- program is handled once, not twice.
  denStmts next (M.union (branch theta True) (branch (1 - theta) False))
  where
    branch w v = mergeDistr (sigmaInsert x v) ((*w) <$> d)
denStmt (Observe e) next d = -- requires renormalization at the end
  denStmts next (M.filterWithKey (\sigma _ -> denExpr e sigma) d)
denStmt (If e s1 s2) next d = do
  let (dThen, dElse) = M.partitionWithKey (\sigma _ -> denExpr e sigma) d
  rThen <- denStmts s1 dThen
  rElse <- denStmts s2 dElse
  case rThen `plusRet` rElse of
    Ret newD [] -> denStmts next newD
    _ -> error "internal error: loop terms escaped; loops should not be desugared via If"
  where
    plusRet (Ret d1 t1) (Ret d2 t2) = Ret (M.unionWith (+) d1 d2) (t1 ++ t2)
denStmt loop@(While lbl e s) next d = do
  cl <- gets dsCurrentLoop
  case cl of
    Just current
      | clLabel current == lbl -> do
        -- Arriving back at the loop being unrolled. States that fail the
        -- guard exit the loop right here: they become constants of the
        -- equation being built. States that satisfy it are about to run the
        -- body, whose behavior and results are independent of their
        -- memoryless part, so they are lumped by their carried part.
        car <- liCarried <$> loopInfo
        let (dTrue, dFalse) = M.partitionWithKey (\sigma _ -> denExpr e sigma) d
            lumped = mergeDistr (IS.intersection car) dTrue
        for_ (M.keys lumped) unrollOnce
        if null next
          then pure (Ret dFalse [L.Term w sigma | (sigma, w) <- M.toList lumped])
          else error "internal error: expected empty after unrolling loop"
    _ -> do
      -- The loop's dynamics depend only on the variables its guard and body
      -- mention (the footprint), and in fact only on those variables read by
      -- its body. Each arriving state is handled accordingly: states failing
      -- the guard never run the body and exit; for the rest, we compute the
      -- carried part and the non-carried part. The carried part is the kernel's
      -- cache key so states differing anywhere else share one kernel.
      --
      -- Before any kernel is solved, the whole arriving distribution is run
      -- deterministically; the counter tells us the variables that are written
      -- deterministically. If it exists, we run the body iteration by iteration
      -- for as long as the counter variables do not recur and the counter does
      -- not cause the loop to exit. This guarantees that no loop-head state can
      -- have been seen before. Within that horizon the loop is just
      -- straight-line code, and pushing the distribution through it costs one
      -- body pass per iteration rather than one per loop-head state, with no
      -- equations to solve. What is still alive when a counter value recurs
      -- goes through the kernels.
      (fp, car, counter) <- ((,,) <$> liFootprint <*> liCarried <*> liCounter) <$> loopInfo
      let (dTrue, dFalse) = M.partitionWithKey (\sigma _ -> denExpr e sigma) d
          -- What a state carries into the next iteration: the carried part,
          -- and whatever the loop does not touch at all.
          lump sigma = IS.intersection car sigma `IS.union` (sigma IS.\\ fp)
      (deterministicExits, alive) <- deterministicTransition counter lump (mergeDistr lump dTrue)
      let entries = Set.map (IS.intersection car) (M.keysSet alive)
      ensureKernels entries
      kernels <- gets (liKernels . (IM.! lbl) . dsLoopInfo)
      let transform (sigma, w) =
            let kernel = kernels M.! IS.intersection car sigma
            in mergeDistr (IS.union (sigma IS.\\ fp)) ((*w) <$> kernel)
          out = M.unionsWith (+) (dFalse : deterministicExits : map transform (M.toList alive))
      denStmts next out
  where
    loopInfo = do
      cached <- gets (IM.lookup lbl . dsLoopInfo)
      case cached of
        Just li -> pure li
        Nothing -> do
          let fp = IS.fromList (toList e ++ concatMap toList s)
              car = findCarried fp s
              li = LoopInfo { liFootprint = fp, liCarried = car, liCounter = findCounter car s, liKernels = M.empty, liDeterministicSteps = 0 }
          modify' (\ds -> ds {dsLoopInfo = IM.insert lbl li (dsLoopInfo ds)})
          pure li
    -- Push the alive distribution through iterations of the body until a
    -- counter value recurs, or nothing is alive. Returns the mass that exited
    -- and the distribution still alive. Alive means the state satisfies
    -- the guard. A loop without a counter is not handled at all; nothing exits.
    deterministicTransition counter lump alive0
      | IS.null counter = pure (M.empty, alive0)
      | otherwise = go Set.empty M.empty alive0
      where
        go seen exits alive
          | M.null alive = pure (exits, alive)
          | counterState `Set.isSubsetOf` seen = pure (exits, alive)
          | otherwise = do
              modify' (\ds -> ds {dsLoopInfo = IM.adjust (\li -> li {liDeterministicSteps = liDeterministicSteps li + 1}) lbl (dsLoopInfo ds)})
              r <- denStmts s alive
              let after = case r of
                    Ret dist [] -> dist
                    _ -> error "internal error: loop terms escaped"
                  (aTrue, aFalse) = M.partitionWithKey (\sigma _ -> denExpr e sigma) after
              go (seen `Set.union` counterState) (M.unionWith (+) exits aFalse) (mergeDistr lump aTrue)
          where
            counterState = Set.map (IS.intersection counter) (M.keysSet alive)
    -- Solve, in one batch, the kernels of every lumped entry state not
    -- already cached. The missing lumps are all unrolled under a single
    -- 'CurrentLoop', so a lump reachable from two entry states is
    -- unrolled once rather than once per entry, and the equations accumulate
    -- into one system x=Ax+b. We do not have to solve the entire system: we
    -- only need the rows corresponding to the entry states, and 'L.solveRows'
    -- computes exactly those rows — for all the entry states at once, so the
    -- recurrent blocks are asterated once per batch rather than once per
    -- entry — and combines them with the per-state exit distributions.
    ensureKernels entries = do
      known <- gets (M.keysSet . liKernels . (IM.! lbl) . dsLoopInfo)
      let missing = entries Set.\\ known
      unless (null missing) $ do
        outer <- gets dsCurrentLoop
        modify' (\ds -> ds {dsCurrentLoop = Just (CurrentLoop lbl Set.empty [])})
        for_ missing unrollOnce
        newEqns <- gets (clEqns . fromJust . dsCurrentLoop)
        -- Safe fromJust: 'L.solveRows' returns Nothing only if a divergent
        -- (recurrent) state carries exit mass, which mass conservation rules
        -- out (see TODOs/TODO-proof.txt).
        let coeffs = [L.Row st tms | (st, Ret _ tms) <- newEqns]
            kernelExits = [(st, dist) | (st, Ret dist _) <- newEqns]
            solved = fromJust (L.solveRows coeffs missing kernelExits)
            updateLoopInfo li = li { liKernels = M.union solved (liKernels li) }
        modify'
          (\ds ->
             ds
               { dsCurrentLoop = outer
               , dsLoopInfo = IM.adjust updateLoopInfo lbl (dsLoopInfo ds)
               })
    unrollOnce loopSigma = do
      seen <- gets (clSeenSigma . fromJust . dsCurrentLoop)
      when (loopSigma `Set.notMember` seen) $ do
        modify' (overCurrentLoop (\c -> c {clSeenSigma = Set.insert loopSigma (clSeenSigma c)}))
        solvedEarlier <- gets (M.lookup loopSigma . liKernels . (IM.! lbl) . dsLoopInfo)
        r <-
          case solvedEarlier of
            -- An earlier batch already worked out where this ends up, so it
            -- becomes a constant. No need to re-unroll.
            Just k -> pure (Ret k [])
            Nothing -> denStmts (s ++ [loop]) (M.singleton loopSigma 1)
        modify' (overCurrentLoop (\c -> c {clEqns = (loopSigma, r) : clEqns c}))
    overCurrentLoop f ds = ds {dsCurrentLoop = f <$> dsCurrentLoop ds}
denStmt (Par lbl blocks) next d = do
  info <- parInfo
  -- Each state goes through every block, and the block's answer is looked up
  -- by the part of the state the block can see, so a block is pushed once
  -- per distinct such part, however many states share it. What no block
  -- writes is carried over from the state unchanged. A written variable that
  -- is dead after the node is dropped, so that states differing only in a
  -- spent temporary merge here rather than at the next loop head.
  -- The blocks commute and no block mentions what another writes, so a state's
  -- projection onto one block's footprint is the same whether or not the other
  -- blocks have run. We may therefore push the whole distribution through one
  -- block at a time instead of building each state's product on its own and
  -- unioning the per-state results at the end. The product is the same, but
  -- the intermediate distributions are merged as they are built, so states
  -- that agree from here on collapse at once instead of being carried to a
  -- final union of as many maps as there were states.
  out <- foldM applyBlock d (zip3 [0 ..] blocks (piBlocks info))
  overParInfo (\pinfo -> pinfo {piRuns = piRuns pinfo + 1, piStatesIn = piStatesIn pinfo + M.size d, piStatesOut = piStatesOut pinfo + M.size out})
  denStmts next out
  where
    overParInfo f = modify' (\ds -> ds {dsParInfo = IM.adjust f lbl (dsParInfo ds)})
    parInfo = do
      cached <- gets (IM.lookup lbl . dsParInfo)
      case cached of
        Just pinfo -> pure pinfo
        Nothing -> do
          live <- gets (IM.lookup lbl . dsParLive)
          let keep = maybe id IS.intersection live
              pinfo = ParInfo [(IS.fromList (concatMap toList b), ws, keep ws) | b <- blocks, let ws = IS.fromList (concatMap stmtWrites b)] IM.empty 0 0 0
          modify' (\ds -> ds {dsParInfo = IM.insert lbl pinfo (dsParInfo ds)})
          pure pinfo
    applyBlock dist (i, block, (fp, writes, written)) =
      foldM
          ( \acc (sigma, w) -> do
              t <- blockTransition i block (IS.intersection fp sigma) written
              let rest = sigma IS.\\ writes
              pure $ M.unionWith (+) acc (mergeDistr (IS.union rest) ((*w) <$> t))
          )
          M.empty
          (M.toList dist)
    blockTransition i block key written = do
      cached <- gets (M.lookup key . IM.findWithDefault M.empty i . piCaches . (IM.! lbl) . dsParInfo)
      case cached of
        Just t -> pure t
        Nothing -> do
          r <- denStmts block (M.singleton key 1)
          let t = case r of
                Ret dist [] -> mergeDistr (IS.intersection written) dist
                -- A block cannot contain the loop being unrolled, or indeed any loop at all.
                _ -> error "internal error: loop terms escaped from a Par block"
              remember pinfo = pinfo {piCaches = IM.insertWith M.union i (M.singleton key t) (piCaches pinfo)}
          modify' (\ds -> ds {dsParInfo = IM.adjust remember lbl (dsParInfo ds)})
          pure t

-- | Which loop each 'Par' node sits in, by label: the innermost 'While' whose
-- body contains it. "Prob.Alloc" only ever builds a 'Par' out of a loop body,
-- so every node has one; a node outside every loop would simply not be
-- reported.
parLoops :: Prog Int -> IM.IntMap Int
parLoops (ReturnMult ss _) = execState (goStmts Nothing ss) IM.empty
  where
    goStmts loop = traverse_ (goStmt loop)
    goStmt loop s =
      case s of
        If _ s1 s2 -> goStmts loop s1 >> goStmts loop s2
        While o _ body -> goStmts (Just o) body
        Par o bs -> do
          for_ loop (modify' . IM.insert o)
          goStmts loop (concat bs)
        _ -> pure ()

-- | The variables live after each 'Par' node of a program, by label. What a
-- block writes that is dead afterwards need not be part of the states
-- "Prob.Den" builds from the blocks' results.
parLiveOuts :: Prog Int -> IM.IntMap IS.IntSet
parLiveOuts (ReturnMult ss es) = execState (goStmts (Set.fromList (foldMap toList es)) ss) IM.empty
  where
    goStmts out stmts = zipWithM_ goStmt (drop 1 (scanr liveIn out stmts)) stmts
    goStmt out s =
      case s of
        If _ s1 s2 -> goStmts out s1 >> goStmts out s2
        While _ _ body -> goStmts (liveIn s out) body
        Par o blocks -> do
          modify' (IM.insert o (IS.fromList (Set.toList out)))
          zipWithM_ goStmts (drop 1 (scanr liveIns out blocks)) blocks
        _ -> pure ()

runDenStmt :: Sigma -> Prog Int -> (Distr Sigma, InferStats)
runDenStmt sigma prog@(stmts `ReturnMult` _) = (extractDist r, stats)
  where
    (r, ds) =
      runState
        (denStmts stmts (M.singleton sigma 1))
        (DenState Nothing IM.empty IM.empty (parLiveOuts prog) 0 0 0)
    stats =
      InferStats
        { isStmtsRun = dsStmtsRun ds
        , isStatesPushed = dsStatesPushed ds
        , isLargestDistr = dsLargestDistr ds
        , isLoopStats =
            IM.mapWithKey
              (\lbl li ->
                 InferLoopStats
                   (IS.size (liFootprint li))
                   (IS.size (liCarried li))
                   (M.size (liKernels li))
                   (liDeterministicSteps li)
                   (IM.mapMaybeWithKey (parStatsOf lbl) (dsParInfo ds)))
              (dsLoopInfo ds)
        }
    -- A 'Par' node reports under the loop whose body holds it. A node that
    -- never ran has no 'ParInfo' and so is silently absent, which is the
    -- right answer: there is nothing to say about what it cost.
    inLoop = parLoops prog
    parStatsOf lbl par pinfo
      | IM.lookup par inLoop == Just lbl = Just (parStats pinfo)
      | otherwise = Nothing
    parStats pinfo =
      InferParStats
        { ipsBlocks = length (piBlocks pinfo)
        , ipsRuns = piRuns pinfo
        , ipsStatesIn = piStatesIn pinfo
        , ipsStatesOut = piStatesOut pinfo
        , ipsFootprints = [IS.size fp | (fp, _, _) <- piBlocks pinfo]
        , ipsSolved = [M.size (cacheOf i) | i <- [0 .. length (piBlocks pinfo) - 1]]
        , ipsFanOut = [maximum (0 : map M.size (M.elems (cacheOf i))) | i <- [0 .. length (piBlocks pinfo) - 1]]
        }
      where
        cacheOf i = IM.findWithDefault M.empty i (piCaches pinfo)

extractDist :: Ret -> Distr Sigma
extractDist (Ret d []) = d
extractDist _ = error "extractDist: contains unsolved loop variables"

-- | Run a program and evaluate its returned expressions in each ending state,
-- adding together the probabilities of the states that agree on all of them.
denProg :: Prog Int -> [([Bool], Rational)]
denProg = fst . denProgStats

-- | 'denProg', also reporting what the run cost. Forcing the 'InferStats'
-- forces the whole inference, so a caller that wants both should print the
-- results first if it wants them to stream.
denProgStats :: Prog Int -> ([([Bool], Rational)], InferStats)
denProgStats prog@(_ `ReturnMult` es) = (renormalize (nonzeroes (M.toList (M.mapKeysWith (+) project d))), stats)
  where
    (d, stats) = runDenStmt IS.empty prog
    project sigma = map (`denExpr` sigma) es

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
