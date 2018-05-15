module Prob.Den3 where

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map.Strict as M
import Prob.CoreAST
import Prob.Den (denExpr)

-- | The 'P' monad represents results with probabilities.
newtype P vt a = P (Sigma vt -> [(Rational, (Sigma vt, a))])

instance Functor (P vt) where
  fmap = liftM

instance Applicative (P vt) where
  pure a = P $ \s -> [(1, (s, a))]
  (<*>) = ap

instance Monad (P vt) where
  P ma >>= f =
    P $ \s ->
      concatMap
        (\(pa, (s', a)) ->
           let P b = f a
           in map (first (pa *)) (b s'))
        (ma s)

denStmts :: (Ord vt, Show vt, Foldable t) => t (Stmt vt) -> P vt ()
denStmts = traverse_ denStmt

denStmt :: (Show vt, Ord vt) => Stmt vt -> P vt ()
denStmt (x := e) = P $ \s -> [(1, (M.insert x (denExpr e s) s, ()))]
denStmt (x :~ Bernoulli theta) = P $ \s -> [(1 - theta, (M.insert x False s, ())), (theta, (M.insert x True s, ()))]
denStmt (Observe e) = P $ \s -> if denExpr e s then [(1, (s, ()))] else []
denStmt (If e s1 s2) = P $ \s -> let P p = denStmts (if denExpr e s then s1 else s2) in p s
denStmt loop@(While e s1) = P $ \s -> if denExpr e s then (let P p = denStmts s1 >> denStmt loop in p s) else [(1, (s, ()))]

test1 :: [(Rational, (Sigma String, ()))]
test1 =
  let P p = denStmts ["c1" :~ Bernoulli 0.5, While (Var "c1") ["c1" :~ Bernoulli 0.5]]
  in take 10 (p (M.fromList []))
