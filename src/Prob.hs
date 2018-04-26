{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Prob where

import Control.Applicative
import Control.Error
import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as Set
import Data.String
import System.Random.MWC
import System.Random.MWC.Distributions
import Text.Groom

-- | The syntax of expressions, parametrized by the representation of variables
-- (usually strings).
data Expr varTy
  = Var varTy
  | Constant Bool
  | (Expr varTy) `And` (Expr varTy)
  | (Expr varTy) `Or` (Expr varTy)
  | Not (Expr varTy)
  deriving (Show, Functor, Foldable, Traversable)

data Dist =
  Bernoulli Rational
  deriving (Show)

infix 2 :=

infixr 3 `And`

infixr 3 `Or`

infix 2 :~

infixl 1 `Seq`

infix 0 `Return`

newtype Then varTy =
  Then (Stmt varTy)
  deriving (Show, Functor, Foldable, Traversable)

newtype Else varTy =
  Else (Stmt varTy)
  deriving (Show, Functor, Foldable, Traversable)

data Stmt varTy
  = Skip
  | varTy := (Expr varTy)
  | varTy :~ Dist
  | Observe (Expr varTy)
  | (Stmt varTy) `Seq` (Stmt varTy)
  | If (Expr varTy)
       (Then varTy)
       (Else varTy)
  | While (Expr varTy)
          (Stmt varTy)
  deriving (Show, Functor, Foldable, Traversable)

data Prog varTy  = (Stmt varTy) `Return` (Expr varTy)
  deriving (Show, Functor, Foldable, Traversable)

instance IsString (Expr String) where
  fromString = Var

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------
-- | The program state consists of a list of variable assignments and the state
-- of the random number generator. All variables are global variables.
type ProgState vt s = (M.Map vt Bool, Gen s)

-- | The evaluation monad.
type Eval vt s = MaybeT (StateT (ProgState vt s) (ST s))

runE :: Eval vt s a -> IO (Maybe a)
runE e = withSystemRandom . asGenST $ (\rng -> evalStateT (runMaybeT e) (M.empty, rng))

runEs :: Int -> Eval vt s a -> IO [a]
runEs t e = withSystemRandom . asGenST $ (\rng -> catMaybes <$> evalStateT (replicateM t (runMaybeT e)) (M.empty, rng))

evalExpr :: (Show vt, Ord vt) => Expr vt -> Eval vt s Bool
evalExpr (Var x) = fromMaybe (error $ "undefined variable " ++ show x) <$> gets (M.lookup x . fst)
evalExpr (Constant d) = pure d
evalExpr (Or a b) = liftA2 (||) (evalExpr a) (evalExpr b)
evalExpr (And a b) = liftA2 (&&) (evalExpr a) (evalExpr b)
evalExpr (Not a) = not <$> evalExpr a

drawDist :: (Show vt, Ord vt) => Dist -> Eval vt s Bool
drawDist (Bernoulli p) = do
  rng <- gets snd
  lift (bernoulli (fromRational p) rng)

evalStmt :: (Show vt, Ord vt) => Stmt vt -> Eval vt s ()
evalStmt Skip = pure ()
evalStmt (x := a) = do
  v <- evalExpr a
  modify (first (M.insert x v))
evalStmt (x :~ d) = do
  v <- drawDist d
  modify (first (M.insert x v))
evalStmt (Observe e) = do
  e' <- evalExpr e
  if e'
    then pure ()
    else MaybeT $ pure Nothing
evalStmt (Seq a b) = evalStmt a >> evalStmt b
evalStmt (If e (Then thenn) (Else alt)) = do
  e' <- evalExpr e
  if e'
    then evalStmt thenn
    else evalStmt alt
evalStmt s@(While e stmt) = do
  e' <- evalExpr e
  if e'
    then evalStmt stmt >> evalStmt s
    else pure ()

evalProg :: (Show vt, Ord vt) => Prog vt -> Eval vt s Bool
evalProg (Return stmt expr) = evalStmt stmt >> evalExpr expr

--------------------------------------------------------------------------------
-- Denotational Semantics
--------------------------------------------------------------------------------
-- | The program state in denotational style is just the set of all variables
-- assignments. We do not need a random number generator here.
type ProgState' vt = M.Map vt Bool

sumOverAllPossibleStates :: (Num r, Ord vt) => Set.Set vt -> (ProgState' vt -> r) -> r
sumOverAllPossibleStates vars g = sum (map g allPossibleStates)
  where
    allPossibleStates = foldr (\var -> concatMap (\st -> [M.insert var True st, M.insert var False st])) [M.empty] vars

denExpr :: (Show vt, Ord vt) => Expr vt -> ProgState' vt -> Bool
denExpr (Var x) sigma = fromMaybe (error $ "undefined variable " ++ show x) $ M.lookup x sigma
denExpr (Constant d) _ = d
denExpr (Or a b) sigma = denExpr a sigma || denExpr b sigma
denExpr (And a b) sigma = denExpr a sigma && denExpr b sigma
denExpr (Not a) sigma = not (denExpr a sigma)

denStmt :: (Show vt, Ord vt) => Stmt vt -> ProgState' vt -> ProgState' vt -> Rational
denStmt Skip sigma' sigma
  | sigma' == sigma = 1
  | otherwise = 0
denStmt (x := e) sigma' sigma
  | sigma' == M.insert x (denExpr e sigma) sigma = 1
  | otherwise = 0
denStmt (x :~ Bernoulli theta) sigma' sigma
  | sigma' == M.insert x True sigma = theta
  | sigma' == M.insert x False sigma = 1 - theta
  | otherwise = 0
denStmt (Seq s1 s2) sigma' sigma =
  sumOverAllPossibleStates (M.keysSet sigma) $ \sigma'' -> denStmt s1 sigma'' sigma * denStmt s2 sigma' sigma''
denStmt (If e (Then s1) (Else s2)) sigma' sigma
  | denExpr e sigma = denStmt s1 sigma' sigma
  | otherwise = denStmt s2 sigma' sigma
denStmt (While _ _) _ _ = undefined -- XXX
denStmt (Observe _) _ _ = error "not allowed to call observe in the middle of a program"


denProg :: (Show vt, Ord vt) => Prog vt -> [(Bool, Rational)]
denProg ((s `Seq` Observe e1) `Return` e2) = [(False, 1 - probReturnTrue), (True, probReturnTrue)]
  where
    vars = Set.fromList (toList s <> toList e1 <> toList e2)
          -- initial state: all variables initialized to False
    initialState
     = M.fromSet (const False) vars
    probReturnTrue =
      sumOverAllPossibleStates vars (\sigma -> if denExpr (e1 `And` e2) sigma then denStmt s sigma initialState else 0)
      /
      sumOverAllPossibleStates vars (\sigma -> if denExpr e1 sigma then denStmt s sigma initialState else 0)

denProg (s `Return` e) = [(False, 1 - probReturnTrue), (True, probReturnTrue)]
  where
    vars = Set.fromList (toList s <> toList e)
          -- initial state: all variables initialized to False
    initialState
     = M.fromSet (const False) vars
    probReturnTrue =
      sumOverAllPossibleStates vars $ \sigma ->
        if denExpr e sigma
          then denStmt s sigma initialState
          else 0

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
-- | Crude pretty printer. Not pretty at all.
pr :: Show a => a -> IO ()
pr = putStrLn . groom

tally :: Ord a => [a] -> [(a, Int)]
tally = map (liftA2 (,) head length) . group . sort

sampled :: (Show vt, Ord vt) => Prog vt -> IO [(Bool, Int)]
sampled prog = tally <$> runEs 10000 (evalProg prog)

--------------------------------------------------------------------------------
-- Example Programs
--------------------------------------------------------------------------------
prog1 :: Prog String
prog1 = "c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 `Return` "c1" `And` "c2"

prog1Sampled :: IO [(Bool, Int)]
prog1Sampled = sampled prog1

prog1Den :: [(Bool, Rational)]
prog1Den = denProg prog1

prog2 :: Prog String
prog2 = "c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 `Seq` Observe ("c1" `Or` "c2") `Return` "c1" `And` "c2"

prog2Sampled :: IO [(Bool, Int)]
prog2Sampled = sampled prog2

prog2Den :: [(Bool, Rational)]
prog2Den = denProg prog2
