{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveTraversable #-}
-- | Denotational semantics using transitional/accepting semantics.
module Prob.Den2Ori where

import qualified Data.Map.Strict as M
import Prob.CoreAST (Dist (..), Expr (..), Sigma)
import Prob.Den

import Debug.Trace

data Stmt vt
  = Seq (Stmt vt)
        (Stmt vt)
  | vt := Expr vt
  | vt :~ Dist
  | If (Expr vt)
       (Stmt vt)
       (Stmt vt)
  | While (Expr vt)
          (Stmt vt)
  | Observe (Expr vt)
  | Skip
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Transitional semantics with debugging.
denStmtT' :: (Show vt, Ord vt) => Stmt vt -> Sigma vt -> Sigma vt -> Rational
denStmtT' s sigma' sigma =
  let !r = denStmtT s sigma' sigma
  in trace (concat ["denStmtT ", show s, " ", show (M.toList sigma'), " ", show (M.toList sigma), " = ", show r]) r

-- | Transitional semantics.
denStmtT :: (Show vt, Ord vt) => Stmt vt -> Sigma vt -> Sigma vt -> Rational
denStmtT (x :~ Bernoulli theta) sigma' sigma
  | sigma' == M.insert x True sigma = theta
  | sigma' == M.insert x False sigma = 1-theta
  | otherwise = 0
denStmtT (x := e) sigma' sigma
  | sigma' == M.insert x (denExpr e sigma) sigma = 1
  | otherwise = 0
denStmtT (If e s1 s2) sigma' sigma
  | denExpr e sigma = denStmtT' s1 sigma' sigma
  | otherwise = denStmtT' s2 sigma' sigma
denStmtT (Observe _) sigma' sigma
  | sigma' == sigma = 1
  | otherwise = 0
denStmtT Skip sigma' sigma
  | sigma' == sigma = 1
  | otherwise = 0
denStmtT (While _ _ ) _ _ = error "not yet"
denStmtT (Seq s1 s2) sigma' sigma =
  sumOverAll sigma (\tau -> denStmtT' s1 tau sigma * denStmtT' s2 sigma' tau * denStmtA' s2 tau) /
  sumOverAll sigma (\tau -> denStmtT' s1 tau sigma * denStmtA' s2 tau)

-- | Accepting semantics with debugging.
denStmtA' :: (Show vt, Ord vt) => Stmt vt -> Sigma vt -> Rational
denStmtA' s sigma =
  let !r = denStmtA s sigma
  in trace (concat ["denStmtA ", show s, " ",  show (M.toList sigma), " = ", show r]) r

-- | Accepting semantics.
denStmtA :: (Show vt, Ord vt) => Stmt vt -> Sigma vt -> Rational
denStmtA (Observe e) sigma
  | denExpr e sigma = 1
  | otherwise = 0
denStmtA (Seq s1 s2) sigma = denStmtA' s1 sigma * sumOverAll sigma (\tau -> denStmtT' s1 tau sigma * denStmtA' s2 tau)
denStmtA _ _ = 1

sumOverAll :: (Ord vt) => Sigma vt -> (Sigma vt -> Rational) -> Rational
sumOverAll sigma f = sum (map f (allPossibleStates vars))
  where vars = M.keys sigma

infixr 2 `Seq`

test1 :: Stmt String
test1 = "c1" :~ Bernoulli 0.5 `Seq` Observe (Var "c1")

test2 :: Stmt String
test2 = "c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 `Seq` Observe (Var "c1")
