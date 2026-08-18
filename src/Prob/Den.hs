{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Denotational semantics using traditional normalized/unnormalized semantics.
module Prob.Den
  ( denProg
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
data CurrentLoop = CurrentLoop
  { clLabel :: Int
  , clSeenSigma :: Set.Set Sigma
  , clEqns :: [(Sigma, Ret)]
  }

-- | The state threaded through the denotation: the loop currently being
-- unrolled (if any), the footprint of every loop in the program, and the
-- cache of already-solved loop kernels. Only 'dsCurrentLoop' is saved
-- and restored around a loop solve; the footprints and the kernel cache
-- only ever grows.
data DenState = DenState
  { dsCurrentLoop :: Maybe CurrentLoop
  , dsFootprints :: IM.IntMap IS.IntSet
  , dsKernels :: M.Map (Int, Sigma) (Distr Sigma)
  }

type Den = State DenState

-- | The denotation of a statement list: an (unnormalized) distribution over
-- ending states, plus — while inside a loop — symbolic references to loop states
-- (the 'L.Term's) that 'L.solveRow' later resolves. At the top level the
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
denStmts ((x := e):next) d =
  denStmts next (mergeDistr (\sigma -> sigmaInsert x (denExpr e sigma) sigma) d)
denStmts ((x :~ Bernoulli theta):next) d =
  -- Here, we ensure that no matter which branch is taken, the rest of the
  -- program is handled once, not twice.
  denStmts next (M.union (branch theta True) (branch (1 - theta) False))
  where
    branch w v = mergeDistr (sigmaInsert x v) ((*w) <$> d)
denStmts (Observe e:next) d = -- requires renormalization at the end
  denStmts next (M.filterWithKey (\sigma _ -> denExpr e sigma) d)
denStmts (If e s1 s2:next) d = do
  let (dThen, dElse) = M.partitionWithKey (\sigma _ -> denExpr e sigma) d
  rThen <- denStmts s1 dThen
  rElse <- denStmts s2 dElse
  case rThen `plusRet` rElse of
    Ret newD [] -> denStmts next newD
    _ -> error "internal error: loop terms escaped; loops should not be desugared via If"
  where
    plusRet (Ret d1 t1) (Ret d2 t2) = Ret (M.unionWith (+) d1 d2) (t1 ++ t2)
denStmts (loop@(While lbl e s):next) d = do
  cl <- gets dsCurrentLoop
  case cl of
    Just current
      | clLabel current == lbl -> do
        -- Arriving back at the loop being unrolled. Every state that has not
        -- been unrolled yet gets unrolled.
        for_ (M.keys d) $ \sigma -> do
          -- Re-get the clSeenSigma in each iteration of the for_.
          seen <- gets (clSeenSigma . fromJust . dsCurrentLoop)
          unless (sigma `Set.member` seen) (unrollOnce sigma)
        if null next
          then pure (Ret M.empty [L.Term w sigma | (sigma, w) <- M.toList d])
          else error "internal error: expected empty after unrolling loop"
    _ -> do
      -- The loop's dynamics depend only on the variables its guard and body
      -- mention (the footprint). We first compute (with cache) the footprint,
      -- then split each arriving state into its relevant and irrelevant parts.
      -- The relevant part is the cache key for this loop, so states differing
      -- only in the irrelevant part share one solve.
      fp <- footprint
      let irrelevantRemoved =
            M.mapWithKey (\sigma w -> M.singleton (sigma IS.\\ fp) w) d
          byRelevant =
            M.mapKeysWith (M.unionWith (+)) (IS.intersection fp) irrelevantRemoved
          -- Every state the loop can exit in, for one entry state: the kernel's
          -- ending states carry the footprint, the entry state the rest.
          exits kernel acc (irrelevant, w) =
            M.unionWith (+) acc (mergeDistr (IS.union irrelevant) ((*w) <$> kernel))
      out <-
        foldlM
          (\acc (relevant, irrelevants) -> do
             kernel <- solveFor relevant
             pure (foldl' (exits kernel) acc (M.toList irrelevants)))
          M.empty
          (M.toList byRelevant)
      denStmts next out
  where
    footprint = do
      cached <- gets (IM.lookup lbl . dsFootprints)
      case cached of
        Just fp -> pure fp
        Nothing -> do
          let fp = IS.fromList (toList e ++ concatMap toList s)
          modify' (\ds -> ds {dsFootprints = IM.insert lbl fp (dsFootprints ds)})
          pure fp
    solveFor relevant = do
      cached <- gets (M.lookup (lbl, relevant) . dsKernels)
      case cached of
        Just k -> pure k
        Nothing -> do
          outer <- gets dsCurrentLoop
          modify' (\ds -> ds {dsCurrentLoop = Just (CurrentLoop lbl Set.empty [])})
          unrollOnce relevant
          newEqns <- gets (clEqns . fromJust . dsCurrentLoop)
          -- We do not have to solve the entire system x=Ax+b. We only need
          -- the row corresponding to the entry state. The 'L.solveRow' does
          -- this by not solving the entire system, and combines that row with
          -- the per-state exit distributions. Safe fromJust: 'L.solveRow'
          -- returns Nothing only if a divergent (recurrent) state carries
          -- exit mass, which mass conservation rules out (see
          -- TODOs/TODO-proof.txt).
          let coeffs = [L.Row st tms | (st, Ret _ tms) <- newEqns]
              kernelExits = [(st, dist) | (st, Ret dist _) <- newEqns]
              k = fromJust (L.solveRow coeffs relevant kernelExits)
          modify'
            (\ds ->
               ds
                 { dsCurrentLoop = outer
                 , dsKernels = M.insert (lbl, relevant) k (dsKernels ds)
                 })
          pure k
    unrollOnce loopSigma = do
      modify' (overCurrentLoop (\c -> c {clSeenSigma = Set.insert loopSigma (clSeenSigma c)}))
      let needAnotherIteration = denExpr e loopSigma
      r <- if needAnotherIteration
           then denStmts (s ++ [loop]) (M.singleton loopSigma 1)
           else pure (Ret (M.singleton loopSigma 1) [])
      modify' (overCurrentLoop (\c -> c {clEqns = (loopSigma, r) : clEqns c}))
    overCurrentLoop f ds = ds {dsCurrentLoop = f <$> dsCurrentLoop ds}

runDenStmt :: Sigma -> [Stmt Int] -> Distr Sigma
runDenStmt sigma stmts =
  extractDist
    (evalState
       (denStmts stmts (M.singleton sigma 1))
       (DenState Nothing IM.empty M.empty))

extractDist :: Ret -> Distr Sigma
extractDist (Ret d []) = d
extractDist _ = error "extractDist: contains unsolved loop variables"

-- | Run a program and evaluate its returned expressions in each ending state,
-- adding together the probabilities of the states that agree on all of them.
denProg :: Prog Int -> [([Bool], Rational)]
denProg (s `ReturnMult` es) = renormalize . nonzeroes . M.toList . M.mapKeysWith (+) project $ runDenStmt IS.empty s
  where project sigma = map (`denExpr` sigma) es

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
