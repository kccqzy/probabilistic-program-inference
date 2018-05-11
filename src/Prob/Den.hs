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
  , denProgPretty
  , denProgReturn
  , denProgReturnAll
  , allPossibleStates
  ) where

import Control.Error
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
denExpr (Not a) sigma = not (denExpr a sigma)

data CurrentLoop vt = CurrentLoop
  { clGuard :: Expr vt
  , clBody :: [Stmt vt]
  , clSeenSigma :: Set.Set (Sigma vt)
  , clEqns :: L.System (Sigma vt)
  } deriving (Show)

type Den vt = State (Maybe (CurrentLoop vt))

denStmt :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Sigma vt -> Den vt (L.RHS (Sigma vt))
denStmt [] sigma' sigma
  | sigma' == sigma = pure (L.RHS 1 [])
  | otherwise = pure (L.RHS 0 [])
denStmt ((x := e):next) sigma' sigma = denStmt next sigma' (M.insert x (denExpr e sigma) sigma)
denStmt ((x :~ Bernoulli theta):next) sigma' sigma = do
  dTrue <- denStmt next sigma' (M.insert x True sigma)
  dFalse <- denStmt next sigma' (M.insert x False sigma)
  pure ((theta `mult` dTrue) `plus` ((1 - theta) `mult` dFalse))
  where mult :: Rational -> L.RHS x -> L.RHS x
        mult k (L.RHS c tms) = L.RHS (k * c) (map (\(L.Term b y) -> L.Term (k*b) y) tms)
        plus :: L.RHS x -> L.RHS x -> L.RHS x
        plus (L.RHS c1 t1) (L.RHS c2 t2) = L.RHS (c1 + c2) (t1++t2)
denStmt (If e s1 s2:next) sigma' sigma
  | denExpr e sigma = denStmt (s1 ++ next) sigma' sigma
  | otherwise = denStmt (s2 ++ next) sigma' sigma
denStmt (Observe e:next) sigma' sigma -- requires renormalization at the end
  | denExpr e sigma = denStmt next sigma' sigma
  | otherwise = pure (L.RHS 0 [])
denStmt (loop@(While e s):next) sigma' sigma = do
  cl <- get
  case cl of
    Just CurrentLoop {..}
      | clGuard == e && clBody == s -> do
        when (sigma `Set.notMember` clSeenSigma) $ do
          let clSeenSigma' = Set.insert sigma clSeenSigma
          put (Just (CurrentLoop clGuard clBody clSeenSigma' clEqns))
          r <- unrollOnce
          addEqn (L.Equation sigma r)
        pure $ L.RHS 0 [L.Term 1 sigma]
    _ -> do
      put (Just (CurrentLoop e s (Set.singleton sigma) []))
      r <- unrollOnce
      addEqn (L.Equation sigma r)
      newEqns <- gets (clEqns . fromJust)
      case L.solve newEqns of
        Nothing -> error "solution of linear system involves infinity: matrix is not of full rank"
        Just m -> do
          let v = fromJust $ M.lookup sigma m
          put cl
          pure (L.RHS v [])
  where
    unrollOnce = denStmt (If e (s ++ [loop]) [] : next) sigma' sigma
    addEqn eqn =
      modify
        (\st ->
           case st of
             Nothing -> Nothing
             Just CurrentLoop {..} -> Just (CurrentLoop clGuard clBody clSeenSigma (eqn : clEqns)))

runDenStmt :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Sigma vt -> Rational
runDenStmt stmts sigma' sigma = extractRHS $ evalState (denStmt stmts sigma' sigma) Nothing

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
    map (\endingState -> (denExpr e endingState, runDenStmt s endingState initialState)) (allPossibleStates vars)

denProgReturnAll :: (Show vt, Ord vt) => [Stmt vt] -> [(Sigma vt, Rational)]
denProgReturnAll s =
  renormalize $
  nonzeroes $
  findDenProg (concatMap toList s) $ \vars initialState ->
    map (\endingState -> (endingState, runDenStmt s endingState initialState)) (allPossibleStates vars)

denProg :: (Show vt, Ord vt) => Prog r vt -> [(r, Rational)]
denProg (s `Return` e) = denProgReturn s e
denProg (ReturnAll s) = denProgReturnAll s

denProgPretty :: forall vt r. (Show vt, Ord vt) => Prog r vt -> ShowS
denProgPretty p =
  case r of
    [] -> showString "No results produced.\n"
    _ -> bars . foldr (\(col1, col2) s -> showString col1 . showString (replicate (maxLen1 - length col1) ' ') . showString col2 . showChar '\n' . s) id r
  where
    r :: [(String, String)]
    r =
      case p of
        Return s e -> map (bimap (`shows` " ") show) (denProgReturn s e)
        ReturnAll s -> map (bimap pprMap show) (denProgReturnAll s)
    pprMap :: Sigma vt -> String
    pprMap =
      M.foldrWithKey
        (\var val s ->
           shows var .
           showString " ->" .
           showString
             (if val
                then "  true "
                else " false ") $
           s)
        " "
    maxLen1 = maximum (length . fst <$> r)
    maxLen2 = maximum (length . snd <$> r)
    bars = showString (replicate (maxLen1 - 1) '-') . showChar ' ' . showString (replicate maxLen2 '-') . showChar '\n'

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
