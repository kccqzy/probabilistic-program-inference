-- | Denotational semantics using transitional/accepting semantics.
module Prob.Den2 where

import qualified Data.Map.Strict as M
import Prob.CoreAST
import Prob.Den

-- | Transitional semantics.
denStmtT :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Sigma vt -> Rational
denStmtT [] sigma' sigma
  | sigma' == sigma = 1
  | otherwise = 0
denStmtT ((x :~ Bernoulli theta):next) sigma' sigma =
  let tau1 = M.insert x True sigma
      tau2 = M.insert x False sigma
      accepting1 = denStmtA next tau1
      accepting2 = denStmtA next tau2
  in (theta * denStmtT next sigma' tau1 * accepting1 + (1 - theta) * denStmtT next sigma' tau2 * accepting2) /
     (theta * accepting1 + (1 - theta) * accepting2)
denStmtT ((x := e):next) sigma' sigma =
  let tau = M.insert x (denExpr e sigma) sigma
  in denStmtT next sigma' tau     -- multiply and division by accepting elided
denStmtT (If e s1 s2:next) sigma' sigma
  | denExpr e sigma = denStmtT (s1++next) sigma' sigma
  | otherwise = denStmtT (s2++next) sigma' sigma
denStmtT (Observe _:next) sigma' sigma = denStmtT next sigma' sigma
denStmtT (While _ _:_) _ _ = error "unsupported While"

-- | Accepting semantics.
denStmtA :: (Show vt, Ord vt) => [Stmt vt] -> Sigma vt -> Rational
denStmtA [] _ = 1
denStmtA (Observe e:next) sigma
  | denExpr e sigma = denStmtA next sigma
  | otherwise = 0
denStmtA ((x := e):next) sigma =
  let tau = M.insert x (denExpr e sigma) sigma
  in denStmtA next tau
denStmtA ((x :~ Bernoulli theta):next) sigma =
  let tau1 = M.insert x True sigma
      tau2 = M.insert x False sigma
  in theta * denStmtA next tau1 + (1 - theta) * denStmtA next tau2
denStmtA (If e s1 s2:next) sigma = sumOverAll sigma $ \tau -> denStmtT [If e s1 s2] tau sigma * denStmtA next tau
denStmtA (While _ _:_) _ = error "unsupported While"

sumOverAll :: (Ord vt) => Sigma vt -> (Sigma vt -> Rational) -> Rational
sumOverAll sigma f = sum (map f (allPossibleStates vars))
  where vars = M.keys sigma
