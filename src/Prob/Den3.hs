module Prob.Den3 where

import Data.Bifunctor
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Prob.CoreAST
import Prob.Den (denExpr)

type P vt = Endo [(Rational, Sigma vt)]

denStmts :: (Ord vt, Show vt) => [Stmt vt] -> P vt
denStmts = foldl' (\p s -> denStmt s `mappend` p) mempty

denStmt :: (Show vt, Ord vt) => Stmt vt -> P vt
denStmt (x := e) = Endo $ \ss -> map (second (\s -> M.insert x (denExpr e s) s)) ss
denStmt (x :~ Bernoulli theta) =
  Endo $ \ss -> concatMap (\(p, s) -> [(p * theta, M.insert x True s), (p * (1 - theta), M.insert x False s)]) ss
denStmt (Observe e) = Endo $ \ss -> filter (denExpr e . snd) ss
denStmt (If e s1 s2) =
  Endo $ \ss ->
    let (thenBranch, elseBranch) = partition (denExpr e . snd) ss
    in appEndo (denStmts s1) thenBranch ++ appEndo (denStmts s2) elseBranch
denStmt loop@(While e s1) = undefined loop e s1

test1 :: [(Rational, Sigma String)]
test1 =
  let Endo p = denStmts ["c1" :~ Bernoulli 0.5, If (Var "c1") ["c2" := Constant False] ["c2" := Constant True]]
  in p [(1, M.fromList [])]
