{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Denotational semantics using traditional normalized/unnormalized semantics.
module Prob.Den
  ( denExpr
  , denStmt
  , denProg
  , denProgReturn
  , denProgReturnAll
  , denProgReturnMult
  ) where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Prob.CoreAST
import Prob.CoreOpt
import qualified Prob.LinearEq as L

--------------------------------------------------------------------------------
-- Denotational Semantics
--------------------------------------------------------------------------------

-- | A (sparse, unnormalized) distribution over states, supported on the
-- reachable states. This is the main return type of 'denStmt': the whole
-- distribution over ending states is computed in one pass, rather than the
-- kernel being evaluated pointwise at each of the 2^N possible ending states.
type Distr s = M.Map s Rational

denExpr :: (Show vt, Ord vt) => Expr vt -> Sigma vt -> Bool
denExpr (Var x) sigma = Set.member x sigma
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
data CurrentLoop vt = CurrentLoop
  { clLabel :: Int
  , clSeenSigma :: Set.Set (Sigma vt)
  , clEqns :: [(Sigma vt, Ret vt)]
  }

-- | The state threaded through the denotation: the loop currently being
-- unrolled (if any), the footprint of every loop in the program, and the
-- cache of already-solved loop kernels. Only 'dsCurrentLoop' is saved
-- and restored around a loop solve; the footprints and the kernel cache
-- only ever grows.
data DenState vt = DenState
  { dsCurrentLoop :: Maybe (CurrentLoop vt)
  , dsFootprints :: IM.IntMap (Set.Set vt)
  , dsKernels :: M.Map (Int, Sigma vt) (Distr (Sigma vt))
  }

type Den vt = State (DenState vt)

-- | The denotation of a statement list: an (unnormalized) distribution over
-- ending states, plus — while inside a loop — symbolic references to loop states
-- (the 'L.Term's) that 'L.solveRow' later resolves. At the top level the
-- reference list is empty and a 'Ret' is just a 'Distr'.
data Ret vt = Ret (Distr (Sigma vt)) [L.Term (Sigma vt)]

scaleRet :: Rational -> Ret vt -> Ret vt
scaleRet k (Ret d tms) = Ret (M.map (k *) d) [L.Term (k * b) y | L.Term b y <- tms]

plusRet :: Ord vt => Ret vt -> Ret vt -> Ret vt
plusRet (Ret d1 t1) (Ret d2 t2) = Ret (M.unionWith (+) d1 d2) (t1 ++ t2)

denStmt :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Den vt (Ret vt)
denStmt [] sigma = pure (Ret (M.singleton sigma 1) [])
denStmt ((x := e):next) sigma = denStmt next (sigmaInsert x (denExpr e sigma) sigma)
denStmt ((x :~ Bernoulli theta):next) sigma = do
  dTrue <- denStmt next (sigmaInsert x True sigma)
  dFalse <- denStmt next (sigmaInsert x False sigma)
  pure (scaleRet theta dTrue `plusRet` scaleRet (1 - theta) dFalse)
denStmt (If e s1 s2:next) sigma
  | denExpr e sigma = denStmt (s1 ++ next) sigma
  | otherwise = denStmt (s2 ++ next) sigma
denStmt (Observe e:next) sigma -- requires renormalization at the end
  | denExpr e sigma = denStmt next sigma
  | otherwise = pure (Ret M.empty [])
denStmt (loop@(While lbl e s):next) sigma = do
  cl <- gets dsCurrentLoop
  case cl of
    Just CurrentLoop {..}
      | clLabel == lbl -> do
        when (sigma `Set.notMember` clSeenSigma) $
          unrollOnce sigma (CurrentLoop clLabel (Set.insert sigma clSeenSigma) clEqns)
        pure (Ret M.empty [L.Term 1 sigma])
    _ -> do
      -- The loop's dynamics depend only on the variables its guard and
      -- body mention (the footprint). We first compute (with cache) the
      -- footprint. Then we split sigma into relevant sigma and
      -- irrelevant sigma. The relevant sigma then becomes a cache key
      -- for this loop so we don't have to solve again if only irrelevant
      -- sigma had changed.
      cachedFp <- gets (IM.lookup lbl . dsFootprints)
      fp <- case cachedFp of
          Just fp -> pure fp
          _ -> do
            let fp = Set.fromList (toList e ++ concatMap toList s)
            modify (\ds -> ds { dsFootprints = IM.insert lbl fp (dsFootprints ds) })
            pure fp
      let sigmaRelevant = Set.intersection sigma fp
          sigmaIrrelevant = Set.difference sigma fp
      cached <- gets (M.lookup (lbl, sigmaRelevant) . dsKernels)
      kernel <- case cached of
          Just k -> pure k
          Nothing -> do
            unrollOnce sigmaRelevant (CurrentLoop lbl (Set.singleton sigmaRelevant) [])
            newEqns <- gets (clEqns . fromJust . dsCurrentLoop)
            -- We do not have to solve the entire system x=Ax+b. We only need
            -- the row corresponding to sigmaRelevant. The 'L.solveRow' does this by
            -- not solving the entire system, and combines that row with the
            -- per-state exit distributions. Safe fromJust: 'L.solveRow'
            -- returns Nothing only if a divergent (recurrent) state carries
            -- exit mass, which mass conservation rules out (see
            -- TODOs/TODO-proof.txt).
            let coeffs = [L.Row st tms | (st, Ret _ tms) <- newEqns]
                exits = [(st, d) | (st, Ret d _) <- newEqns]
                k = fromJust (L.solveRow coeffs sigmaRelevant exits)
            modify
              (\ds ->
                 ds
                   { dsCurrentLoop = cl
                   , dsKernels = M.insert (lbl, sigmaRelevant) k (dsKernels ds)
                   })
            pure k
      rets <-
        traverse
          (\(eps, w) -> scaleRet w <$> denStmt next (eps `Set.union` sigmaIrrelevant))
          (M.toList kernel)
      pure (foldl' plusRet (Ret M.empty []) rets)
  where
    unrollOnce loopSigma nl = do
      modify (\ds -> ds {dsCurrentLoop = Just nl})
      r <- denStmt [If e (s ++ [loop]) []] loopSigma
      modify
        (\ds ->
           ds { dsCurrentLoop =
                 fmap (\c -> c {clEqns = (loopSigma, r) : clEqns c}) (dsCurrentLoop ds)
              })

runDenStmt :: (Show vt, Ord vt) => Sigma vt -> [Stmt vt] -> Distr (Sigma vt)
runDenStmt sigma stmts =
  extractDist
    (evalState
       (denStmt stmts sigma)
       (DenState Nothing IM.empty M.empty))

extractDist :: Ret vt -> Distr (Sigma vt)
extractDist (Ret d []) = d
extractDist _ = error "extractDist: contains unsolved loop variables"

-- | Run a program and summarize each ending state by @f@, adding together the
-- probabilities of the states that @f@ maps to the same summary.
denProgProject :: (Show vt, Ord vt, Ord r) => (Sigma vt -> r) -> [Stmt vt] -> [(r, Rational)]
denProgProject f = renormalize . nonzeroes . M.toList . M.mapKeysWith (+) f . runDenStmt Set.empty

denProgReturn :: (Show vt, Ord vt) => [Stmt vt] -> Expr vt -> [(Bool, Rational)]
denProgReturn s e = denProgProject (denExpr e) s

denProgReturnMult :: (Show vt, Ord vt) => [Stmt vt] -> NE.NonEmpty (Expr vt) -> [(NE.NonEmpty Bool, Rational)]
denProgReturnMult s es = denProgProject (\sigma -> fmap (`denExpr` sigma) es) s

denProgReturnAll :: (Show vt, Ord vt) => [Stmt vt] -> [(Sigma vt, Rational)]
denProgReturnAll = renormalize . nonzeroes . M.toList . runDenStmt Set.empty

denProg' :: (Show vt, Ord vt) => Prog r vt -> [(r, Rational)]
denProg' (s `Return` e) = denProgReturn s e
denProg' (ReturnAll s) = denProgReturnAll s
denProg' (ReturnMult s es) = denProgReturnMult s es

denProg :: (Show vt, Ord vt) => Prog r vt -> [(r, Rational)]
denProg = denProg' . optimizeProgram

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
