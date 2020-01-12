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
  , allPossibleStates
  , runDenStmt
  ) where

import Control.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Prob.CoreAST
import qualified Prob.LinearEq as L

--------------------------------------------------------------------------------
-- Denotational Semantics
--------------------------------------------------------------------------------

allPossibleStates :: (Ord k, Foldable t) => t k -> [M.Map k Bool]
allPossibleStates = foldr (\var -> concatMap (\st -> [M.insert var True st, M.insert var False st])) [M.empty]

denExpr :: (Show vt, Ord vt) => Expr vt -> Sigma vt -> Bool
denExpr (Var x) sigma = fromMaybe (error $ "undefined variable " ++ show x) $ M.lookup x sigma
denExpr (Constant d) _ = d
denExpr (Or a b) sigma = denExpr a sigma || denExpr b sigma
denExpr (And a b) sigma = denExpr a sigma && denExpr b sigma
denExpr (Xor a b) sigma = denExpr a sigma /= denExpr b sigma
denExpr (Not a) sigma = not (denExpr a sigma)

data CurrentLoop vt = CurrentLoop
  { clGuard :: Expr vt
  , clBody :: [Stmt vt]
  , clSeenSigma :: Set.Set (Sigma vt)
  , clEqns :: L.System (Sigma vt)
  } deriving (Show)

type Den vt = StateT (Maybe (CurrentLoop vt)) (Reader (Sigma vt))

denStmt :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Den vt (L.RHS (Sigma vt))
denStmt [] sigma = lift $ ReaderT $ \sigma' -> if sigma' == sigma then pure (L.RHS 1 []) else pure (L.RHS 0 [])
denStmt ((x := e):next) sigma = denStmt next (M.insert x (denExpr e sigma) sigma)
denStmt ((x :~ Bernoulli theta):next) sigma = do
  dTrue <- denStmt next (M.insert x True sigma)
  dFalse <- denStmt next (M.insert x False sigma)
  pure ((theta `mult` dTrue) `plus` ((1 - theta) `mult` dFalse))
  where mult :: Rational -> L.RHS x -> L.RHS x
        mult k (L.RHS c tms) = L.RHS (k * c) (map (\(L.Term b y) -> L.Term (k*b) y) tms)
        plus :: L.RHS x -> L.RHS x -> L.RHS x
        plus (L.RHS c1 t1) (L.RHS c2 t2) = L.RHS (c1 + c2) (t1++t2)
denStmt (If e s1 s2:next) sigma
  | denExpr e sigma = denStmt (s1 ++ next) sigma
  | otherwise = denStmt (s2 ++ next) sigma
denStmt (Observe e:next) sigma -- requires renormalization at the end
  | denExpr e sigma = denStmt next sigma
  | otherwise = pure (L.RHS 0 [])
denStmt (loop@(While e s):next) sigma = do
  cl <- get
  case cl of
    Just CurrentLoop {..}
      | clGuard == e && clBody == s -> do
        when (sigma `Set.notMember` clSeenSigma) $
          unrollOnce (CurrentLoop clGuard clBody (Set.insert sigma clSeenSigma) clEqns)
        pure $ L.RHS 0 [L.Term 1 sigma]
    _ -> do
      unrollOnce (CurrentLoop e s (Set.singleton sigma) [])
      newEqns <- gets (clEqns . fromJust)
      case L.solve newEqns of
        Nothing -> error "solution of linear system involves infinity: matrix is not of full rank"
        Just m -> do
          let v = fromJust $ M.lookup sigma m
          put cl
          pure (L.RHS v [])
  where
    unrollOnce nl = do
      put (Just nl)
      r <- denStmt (If e (s ++ [loop]) [] : next) sigma
      modify
        (\st ->
           case st of
             Nothing -> Nothing
             Just CurrentLoop {..} -> Just (CurrentLoop clGuard clBody clSeenSigma (L.Equation sigma r : clEqns)))

runDenStmt :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Sigma vt -> Rational
runDenStmt stmts sigma =
  let c = runReader (evalStateT (denStmt stmts sigma) Nothing) in
  \sigma' -> extractRHS $ c sigma'

findDenProg :: (Ord vt) => [vt] -> (Set.Set vt -> Sigma vt -> r) -> r
findDenProg p g = g vars initialState
  where
    vars = Set.fromList p
    initialState = M.fromSet (const False) vars
                   -- initial state: all variables initialized to False

extractRHS :: L.RHS vt -> Rational
extractRHS (L.RHS c []) = c
extractRHS _ = error "extractRHS: contains unsolved variables"

denProgReturn :: (Show vt, Ord vt) => [Stmt vt] -> Expr vt -> [(Bool, Rational)]
denProgReturn s e =
  renormalize $
  nonzeroes $
  M.toList $
  M.fromListWith (+) $
  findDenProg (concatMap toList s ++ toList e) $ \vars initialState ->
    let r = runDenStmt s initialState in
    map (\endingState -> (denExpr e endingState, r endingState)) (allPossibleStates vars)

denProgReturnAll :: (Show vt, Ord vt) => [Stmt vt] -> [(Sigma vt, Rational)]
denProgReturnAll s =
  renormalize $
  nonzeroes $
  findDenProg (concatMap toList s) $ \vars initialState ->
    let r = runDenStmt s initialState in
    map (\endingState -> (endingState, r endingState)) (allPossibleStates vars)

denProg :: (Show vt, Ord vt) => Prog r vt -> [(r, Rational)]
denProg (s `Return` e) = denProgReturn s e
denProg (ReturnAll s) = denProgReturnAll s

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
