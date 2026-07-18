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
  , runDenStmt
  ) where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Prob.CoreAST
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

data CurrentLoop vt = CurrentLoop
  { clGuard :: Expr vt
  , clBody :: [Stmt vt]
  , clSeenSigma :: Set.Set (Sigma vt)
  , clEqns :: [(Sigma vt, Ret vt)]
  }

type Den vt = State (Maybe (CurrentLoop vt))

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
denStmt (loop@(While e s):next) sigma = do
  cl <- get
  case cl of
    Just CurrentLoop {..}
      | clGuard == e && clBody == s -> do
        when (sigma `Set.notMember` clSeenSigma) $
          unrollOnce (CurrentLoop clGuard clBody (Set.insert sigma clSeenSigma) clEqns)
        pure (Ret M.empty [L.Term 1 sigma])
    _ -> do
      unrollOnce (CurrentLoop e s (Set.singleton sigma) [])
      newEqns <- gets (clEqns . fromJust)
      -- We do not have to solve the entire system x=Ax+b. We only need the row
      -- corresponding to sigma. The 'L.solveRow' does this by not solving the
      -- entire system, and combines that row with the per-state exit
      -- distributions. Safe fromJust: 'L.solveRow' returns Nothing only if a
      -- divergent (recurrent) state carries exit mass, which mass conservation
      -- rules out (see TODOs/TODO-proof.txt).
      let coeffs = [L.Row st tms | (st, Ret _ tms) <- newEqns]
          exits = [(st, d) | (st, Ret d _) <- newEqns]
          exitDist = fromJust (L.solveRow coeffs sigma exits)
      put cl
      pure (Ret exitDist [])
  where
    unrollOnce nl = do
      put (Just nl)
      r <- denStmt (If e (s ++ [loop]) [] : next) sigma
      modify (fmap (\c -> c { clEqns = (sigma, r) : clEqns c }))

runDenStmt :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Distr (Sigma vt)
runDenStmt stmts sigma = extractDist (evalState (denStmt stmts sigma) Nothing)

-- | Run a denotation from the initial state, in which all variables are
-- @False@. With the distribution-valued 'denStmt', the answer is the entire
-- distribution over ending states — there is no longer any 2^N enumeration of
-- ending states to drive.
fromInitialState :: (Sigma vt -> r) -> r
fromInitialState g = g Set.empty

extractDist :: Ret vt -> Distr (Sigma vt)
extractDist (Ret d []) = d
extractDist _ = error "extractDist: contains unsolved loop variables"

denProgReturn :: (Show vt, Ord vt) => [Stmt vt] -> Expr vt -> [(Bool, Rational)]
denProgReturn s e =
  renormalize $
  nonzeroes $
  M.toList $
  M.fromListWith (+) $
  map (first (denExpr e)) $
  fromInitialState $ \initialState ->
    M.toList (runDenStmt s initialState)

denProgReturnAll :: (Show vt, Ord vt) => [Stmt vt] -> [(Sigma vt, Rational)]
denProgReturnAll s =
  renormalize $
  nonzeroes $
  M.toList $
  fromInitialState $ \initialState ->
    runDenStmt s initialState

denProg :: (Show vt, Ord vt) => Prog r vt -> [(r, Rational)]
denProg (s `Return` e) = denProgReturn s e
denProg (ReturnAll s) = denProgReturnAll s

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
