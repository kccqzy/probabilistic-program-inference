{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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

data Prog r varTy where
  Return :: Stmt varTy -> Expr varTy -> Prog Bool varTy
  ReturnAll :: Stmt varTy -> Prog (ProgState' varTy) varTy
deriving instance Show varTy => Show (Prog r varTy)
deriving instance Foldable (Prog r)

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

evalProg :: (Show vt, Ord vt) => Prog r vt -> Eval vt s r
evalProg (Return stmt expr) = evalStmt stmt >> evalExpr expr
evalProg (ReturnAll stmt) = evalStmt stmt >> gets fst

--------------------------------------------------------------------------------
-- Denotational Semantics
--------------------------------------------------------------------------------
-- | The program state in denotational style is just the set of all variables
-- assignments. We do not need a random number generator here.
type ProgState' vt = M.Map vt Bool

sumOverAllPossibleStates :: (Num r, Ord vt) => Set.Set vt -> (ProgState' vt -> r) -> r
sumOverAllPossibleStates vars g = sum (map g (allPossibleStates vars))

allPossibleStates :: (Ord k, Foldable t) => t k -> [M.Map k Bool]
allPossibleStates = foldr (\var -> concatMap (\st -> [M.insert var True st, M.insert var False st])) [M.empty]

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
denStmt (Observe e) sigma' sigma -- requires renormalization at the end
  | sigma' == sigma && denExpr e sigma = 1
  | otherwise = 0
denStmt (While e s) sigma' sigma =
  lim $ \n -> denStmt (unrollWhile e s n) sigma' sigma

lim :: (Int -> Rational) -> Rational
lim f = f 3

unrollWhile :: Expr varTy -> Stmt varTy -> Int -> Stmt varTy
unrollWhile e s = unroll
  where unroll 0 = Observe (Constant False)
        unroll n = If e (Then (s `Seq` unrollWhile e s (n - 1))) (Else Skip)

findDenProg :: (Foldable t, Ord vt) => t vt -> (Set.Set vt -> ProgState' vt -> r) -> r
findDenProg p g = g vars initialState
  where
    vars = Set.fromList (toList p)
    initialState = M.fromSet (const False) vars
                   -- initial state: all variables initialized to False

denProg :: (Show vt, Ord vt) => Prog r vt -> [(r, Rational)]
denProg p@(s `Return` e) =
  renormalize $ findDenProg p $ \vars initialState ->
  let
    probReturnTrue =
      sumOverAllPossibleStates vars $ \sigma ->
        if denExpr e sigma
          then denStmt s sigma initialState
          else 0
  in [(False, 1 - probReturnTrue), (True, probReturnTrue)]

denProg (ReturnAll s) =
  renormalize $ findDenProg s $ \vars initialState ->
    map (\endingState -> (endingState, denStmt s endingState initialState)) (allPossibleStates vars)

renormalize :: Fractional c => [(a, c)] -> [(a, c)]
renormalize l = map (second (/tot)) l
  where tot = sum (map snd l)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
-- | Crude pretty printer. Not pretty at all.
pr :: Show a => a -> IO ()
pr = putStrLn . groom

tally :: Ord a => [a] -> [(a, Int)]
tally = map (liftA2 (,) head length) . group . sort

sampled :: (Show vt, Ord vt, Ord r) => Prog r vt -> IO [(r, Int)]
sampled prog = tally <$> runEs 10000 (evalProg prog)

--------------------------------------------------------------------------------
-- Example Programs
--------------------------------------------------------------------------------
prog1 :: Prog Bool String
prog1 = "c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 `Return` "c1" `And` "c2"

prog1Sampled :: IO [(Bool, Int)]
prog1Sampled = sampled prog1

prog1Den :: [(Bool, Rational)]
prog1Den = denProg prog1

prog2 :: Prog Bool String
prog2 = "c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 `Seq` Observe ("c1" `Or` "c2") `Return` "c1" `And` "c2"

prog2Sampled :: IO [(Bool, Int)]
prog2Sampled = sampled prog2

prog2Den :: [(Bool, Rational)]
prog2Den = denProg prog2

prog1' :: Prog (M.Map String Bool) String
prog1' = ReturnAll ("c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 )

prog2' :: Prog (M.Map String Bool) String
prog2' = ReturnAll ("c1" :~ Bernoulli 0.5 `Seq` "c2" :~ Bernoulli 0.5 `Seq` Observe ("c1" `Or` "c2") )

progDice :: Prog (M.Map String Bool) String
progDice =
  ReturnAll (
  "bit 0" :~ Bernoulli 0.5 `Seq`
  "bit 1" :~ Bernoulli 0.5 `Seq`
  "bit 2" :~ Bernoulli 0.5 `Seq`
  -- not all zeroes
  Observe ("bit 0" `Or` "bit 1" `Or` "bit 2") `Seq`
  -- not all ones
  Observe (Not "bit 0" `Or` Not "bit 1" `Or` Not "bit 2")
  )

progGeo :: Prog (M.Map String Bool) String
progGeo = ReturnAll (
  "b" := Constant True `Seq`
  While "b" (
      "b" :~ Bernoulli 0.5 `Seq`
      "parity" := Not "parity"
            )
                    )
