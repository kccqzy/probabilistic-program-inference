{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Denotational semantics using transitional/accepting semantics.
module Prob.Den2Ori where

import qualified Data.Map.Strict as M
import Prob.CoreAST (Dist (..), Expr (..), Sigma)
import Prob.Den

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

newtype DivZeroRational = DivZeroRational Rational deriving (Show, Eq, Ord, Num, Enum)

instance Fractional DivZeroRational where
  fromRational = DivZeroRational . fromRational
  0 / 0 = 0
  DivZeroRational a / DivZeroRational b = DivZeroRational (a / b)

type R = DivZeroRational

-- | Transitional semantics.
denStmtT :: (Show vt, Ord vt) => Stmt vt -> Sigma vt -> Sigma vt -> R
denStmtT (x :~ Bernoulli theta) sigma' sigma
  | sigma' == M.insert x True sigma = fromRational theta
  | sigma' == M.insert x False sigma = fromRational (1-theta)
  | otherwise = 0
denStmtT (x := e) sigma' sigma
  | sigma' == M.insert x (denExpr e sigma) sigma = 1
  | otherwise = 0
denStmtT (If e s1 s2) sigma' sigma
  | denExpr e sigma = denStmtT s1 sigma' sigma
  | otherwise = denStmtT s2 sigma' sigma
denStmtT (Observe _) sigma' sigma
  | sigma' == sigma = 1
  | otherwise = 0
denStmtT Skip sigma' sigma
  | sigma' == sigma = 1
  | otherwise = 0
denStmtT (While _ _ ) _ _ = error "not yet"
denStmtT (Seq s1 s2) sigma' sigma =
  let !numerator = sumOverAll sigma (\tau -> denStmtT s1 tau sigma * denStmtT s2 sigma' tau * denStmtA s2 tau)
      !denominator = sumOverAll sigma (\tau -> denStmtT s1 tau sigma * denStmtA s2 tau)
  in numerator / denominator

-- | Accepting semantics.
denStmtA :: (Show vt, Ord vt) => Stmt vt -> Sigma vt -> R
denStmtA (Observe e) sigma
  | denExpr e sigma = 1
  | otherwise = 0
denStmtA (Seq s1 s2) sigma = denStmtA s1 sigma * sumOverAll sigma (\tau -> denStmtT s1 tau sigma * denStmtA s2 tau)
denStmtA (If e s1 s2) sigma
  | denExpr e sigma = denStmtA s1 sigma
  | otherwise = denStmtA s2 sigma
denStmtA _ _ = 1

sumOverAll :: (Ord vt) => Sigma vt -> (Sigma vt -> R) -> R
sumOverAll sigma f = sum (map f (allPossibleStates vars))
  where vars = M.keys sigma

infixr 2 `Seq`

test2 :: Stmt String
test2 = "c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 `Seq` Observe (Var "c1")

test3 :: Stmt String
test3 = "a" :~ Bernoulli 0.2 `Seq` Observe (Var "a")

test3' :: Stmt String
test3' = "a" :~ Bernoulli 0.2 `Seq` If (Var "a") Skip (Observe (Var "a") `Seq` Observe (Var "a"))
