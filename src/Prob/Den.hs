{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Denotational semantics using traditional normalized/unnormalized semantics.
module Prob.Den
  ( denProg
  , denProgStats
  , InferStats(..)
  , InferLoopStats(..)
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

-- | The loop currently being unrolled, identified by its label: the source
-- offset of the loop's @while@ keyword, assigned by the parser. Distinct
-- source loops therefore always have distinct labels, while the body copies a
-- do-while desugars into share the label of their single @while@ keyword —
-- which is sound, and desirable for kernel-cache sharing because
-- label-equal 'While' nodes are structurally identical and the loop kernel is
-- solved independently of what follows the loop.
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

-- | Find the carried set of a loop. The carried set is defined as the subset of
-- the footprint of the loop such that any variables that are always written
-- before read are excluded.
findCarried :: IS.IntSet -> [Stmt Int] -> IS.IntSet
findCarried footprint stmts = footprint IS.\\ (mustWrite IS.\\ mayRead)
  where Flow mayRead mustWrite = foldMap flowStmt stmts

-- | What inference actually cost, as opposed to what a static estimate of it
-- predicted. 'denStmts' pushes a whole distribution through one statement at a
-- time, so the time it takes is very nearly proportional to 'isStatesPushed':
-- the number of (statement, state) pairs it worked on.
--
-- When doing optimization work, the stats here are probably more useful than
-- say, the number of variables.
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
  { ilsFootprintSize, ilsCarriedSize, ilsKernels :: Int }

data LoopInfo = LoopInfo
  { liFootprint :: IS.IntSet
    -- ^ The variables mentioned by the loop.
  , liCarried :: IS.IntSet
    -- ^ The variables inside the loop body that need to be persisted from one
    -- iteration to the next. The loop's own guard is deliberately not part of
    -- the carried set. It is allowed for a loop guard to read some variables
    -- that are not read again in the loop itself. Such variables may safely be
    -- disregarded.
  , liKernels :: M.Map Sigma (Distr Sigma)
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
  , dsStmtsRun :: Int
  , dsStatesPushed :: Int
  , dsLargestDistr :: Int
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
      (fp, car) <- ((,) <$> liFootprint <*> liCarried) <$> loopInfo
      let (dTrue, dFalse) = M.partitionWithKey (\sigma _ -> denExpr e sigma) d
          dTrueStates = M.keysSet dTrue
          dTrueRelevantStates = Set.map (IS.intersection car) dTrueStates
      ensureKernels dTrueRelevantStates
      kernels <- gets (liKernels . (IM.! lbl) . dsLoopInfo)
      let transform (sigma, w) =
            let kernel = kernels M.! IS.intersection car sigma
            in mergeDistr (IS.union (sigma IS.\\ fp)) ((*w) <$> kernel)
          out = M.unionsWith (+) (dFalse : map transform (M.toList dTrue))
      denStmts next out
  where
    loopInfo = do
      cached <- gets (IM.lookup lbl . dsLoopInfo)
      case cached of
        Just li -> pure li
        Nothing -> do
          let fp = IS.fromList (toList e ++ concatMap toList s)
              li = LoopInfo { liFootprint = fp, liCarried = findCarried fp s, liKernels = M.empty }
          modify' (\ds -> ds {dsLoopInfo = IM.insert lbl li (dsLoopInfo ds)})
          pure li
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

runDenStmt :: Sigma -> [Stmt Int] -> (Distr Sigma, InferStats)
runDenStmt sigma stmts = (extractDist r, stats)
  where
    (r, ds) =
      runState
        (denStmts stmts (M.singleton sigma 1))
        (DenState Nothing IM.empty 0 0 0)
    stats =
      InferStats
        { isStmtsRun = dsStmtsRun ds
        , isStatesPushed = dsStatesPushed ds
        , isLargestDistr = dsLargestDistr ds
        , isLoopStats = IM.map (\(LoopInfo f c k) -> InferLoopStats (IS.size f) (IS.size c) (M.size k)) (dsLoopInfo ds)
        }

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
denProgStats (s `ReturnMult` es) = (renormalize (nonzeroes (M.toList (M.mapKeysWith (+) project d))), stats)
  where
    (d, stats) = runDenStmt IS.empty s
    project sigma = map (`denExpr` sigma) es

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
