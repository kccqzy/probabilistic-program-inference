{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Prob.Den (denExpr, denStmt, denProg)where

import Control.Applicative
import Control.Error
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Prob.CoreAST
--------------------------------------------------------------------------------
-- Denotational Semantics
--------------------------------------------------------------------------------

-- | A linear polynomial of the form a+bx. The variable is represented by Sigma.
data Lin vt = Lin Rational Rational (Maybe (Sigma vt)) deriving (Show)

instance Eq vt => Num (Lin vt) where
  fromInteger x = Lin (fromInteger x) 0 Nothing

  Lin a1 b1 x + Lin a2 _ Nothing = Lin (a1 + a2) b1 x
  Lin a1 _ Nothing + Lin a2 b2 x = Lin (a1 + a2) b2 x
  Lin a1 b1 x1 + Lin a2 b2 x2
    | x1 == x2 = Lin (a1 + a2) (b1 + b2) x1
    | otherwise = error "adding two Lin with different variables"

  negate (Lin a b x) = Lin (negate a) (negate b) x

  Lin 0 0 _ * _ = Lin 0 0 Nothing -- short circuiting; important to avoid infinite recursion
  Lin a _ Nothing * Lin c d y = Lin (a * c) (a * d) y
  Lin a b x * Lin c _ Nothing = Lin (a * c) (b * c) x
  Lin a b x * Lin c d y
    | x /= y = error "different vars"
    | b * d == 0 = Lin (a * c) (a * d + b * c) (x <|> y)
    | otherwise = error "quadratic"

  abs = error "unsupported operation: abs"
  signum = error "unsupported operation: signum"


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
  } deriving (Show)

denStmt :: (Show vt, Ord vt) => Maybe (CurrentLoop vt) -> [Stmt vt] -> Sigma vt -> Sigma vt -> Lin vt
denStmt _ [] sigma' sigma
  | sigma' == sigma = 1
  | otherwise = 0
denStmt cl ((x := e):next) sigma' sigma = denStmt cl next sigma' (M.insert x (denExpr e sigma) sigma)
denStmt cl ((x :~ Bernoulli theta):next) sigma' sigma =
  ratToLin theta * denStmt cl next sigma' (M.insert x True sigma) +
  ratToLin (1 - theta) * denStmt cl next sigma' (M.insert x False sigma)
denStmt cl (If e (Then s1) (Else s2):next) sigma' sigma
  | denExpr e sigma = denStmt cl (s1 ++ next) sigma' sigma
  | otherwise = denStmt cl (s2 ++ next) sigma' sigma
denStmt cl (Observe e:next) sigma' sigma -- requires renormalization at the end
  | denExpr e sigma = denStmt cl next sigma' sigma
  | otherwise = 0
denStmt cl (loop@(While e s):next) sigma' sigma =
  case cl of
    Just CurrentLoop {..}
      | clGuard == e && clBody == s ->
        if sigma `Set.member` clSeenSigma
          then Lin 0 1 (Just sigma)
          else trySolveLin sigma $ unrollOnce (Just (CurrentLoop e s (Set.insert sigma clSeenSigma)))
    _ -> trySolveLin sigma $ unrollOnce (Just (CurrentLoop e s (Set.singleton sigma)))
  where
    unrollOnce nl = denStmt nl (If e (Then (s ++ [loop])) (Else []) : next) sigma' sigma

trySolveLin :: (Eq vt) => Sigma vt -> Lin vt -> Lin vt
trySolveLin sigma (Lin a b (Just sigma2))
  | sigma == sigma2 = Lin (a / (1 - b)) 0 Nothing
trySolveLin _ r = r

ratToLin :: Rational -> Lin vt
ratToLin a = Lin a 0 Nothing

linToRat :: Lin vt -> Rational
linToRat (Lin a 0 Nothing) = a
linToRat _ = error "contains variables"

findDenProg :: (Foldable t, Ord vt) => t vt -> (Set.Set vt -> Sigma vt -> r) -> r
findDenProg p g = g vars initialState
  where
    vars = Set.fromList (toList p)
    initialState = M.fromSet (const False) vars
                   -- initial state: all variables initialized to False

denProg :: (Show vt, Ord vt) => Prog r vt -> [(r, Rational)]
denProg p@(s `Return` e) =
  renormalize $
  M.toList $
  M.fromListWith (+) $
  (fmap . fmap) linToRat $
  findDenProg p $ \vars initialState ->
    map (\endingState -> (denExpr e endingState, denStmt Nothing s endingState initialState)) (allPossibleStates vars)

denProg p@(ReturnAll s) =
  renormalize $
  nonzeroes $
  (fmap . fmap) linToRat $
  findDenProg p $ \vars initialState ->
    map (\endingState -> (endingState, denStmt Nothing s endingState initialState)) (allPossibleStates vars)

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

nonzeroes :: (Num c, Ord c) => [(a, c)] -> [(a, c)]
nonzeroes = filter ((>0) . snd)
