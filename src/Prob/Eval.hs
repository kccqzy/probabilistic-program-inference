{-# LANGUAGE GADTs #-}
module Prob.Eval
  ( runE
  , runEs
  , Eval
  , ProgState
  , evalProg
  , sampled
  , tally
  ) where

import Control.Applicative
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set
import Prob.CoreAST
import System.Random.MWC
import System.Random.MWC.Distributions

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------
-- | The program state consists of a list of variable assignments and the state
-- of the random number generator. All variables are global variables.
type ProgState vt s = (Sigma vt, Gen s)

-- | The evaluation monad.
type Eval vt s = MaybeT (StateT (ProgState vt s) (ST s))

runE :: Eval vt s a -> IO (Maybe a)
runE e = withSystemRandom . asGenST $ (\rng -> evalStateT (runMaybeT e) (Set.empty, rng))

runEs :: Int -> Eval vt s a -> IO [a]
runEs t e = withSystemRandom . asGenST $ (\rng -> catMaybes <$> evalStateT (replicateM t (runMaybeT e)) (Set.empty, rng))

evalExpr :: (Show vt, Ord vt) => Expr vt -> Eval vt s Bool
evalExpr (Var x) = gets (Set.member x . fst)
evalExpr (Constant d) = pure d
evalExpr (Or a b) = liftA2 (||) (evalExpr a) (evalExpr b)
evalExpr (And a b) = liftA2 (&&) (evalExpr a) (evalExpr b)
evalExpr (Xor a b) = liftA2 (/=) (evalExpr a) (evalExpr b)
evalExpr (Not a) = not <$> evalExpr a

drawDist :: (Show vt, Ord vt) => Dist -> Eval vt s Bool
drawDist (Bernoulli p) = do
  rng <- gets snd
  lift (bernoulli (fromRational p) rng)

evalStmt :: (Show vt, Ord vt) => [Stmt vt] -> Eval vt s ()
evalStmt [] = pure ()
evalStmt ((x := a):next) = do
  v <- evalExpr a
  modify (first (sigmaInsert x v))
  evalStmt next
evalStmt ((x :~ d):next) = do
  v <- drawDist d
  modify (first (sigmaInsert x v))
  evalStmt next
evalStmt (Observe e:next) = do
  e' <- evalExpr e
  if e'
    then evalStmt next
    else MaybeT $ pure Nothing
evalStmt (If e thenn alt:next) = do
  e' <- evalExpr e
  if e'
    then evalStmt thenn
    else evalStmt alt
  evalStmt next
evalStmt s@(While e stmt:next) = do
  e' <- evalExpr e
  if e'
    then evalStmt stmt >> evalStmt s
    else evalStmt next

evalProg :: (Show vt, Ord vt) => Prog r vt -> Eval vt s r
evalProg (Return stmt expr) = evalStmt stmt >> evalExpr expr
evalProg (ReturnAll stmt) = evalStmt stmt >> gets fst

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
tally :: Ord a => [a] -> [(a, Int)]
tally = map (liftA2 (,) head length) . group . sort

renormalize :: [(a, Int)] -> [(a, Rational)]
renormalize l = fmap (fmap (\n -> fromIntegral n % fromIntegral tot)) l
  where tot = sum (map snd l)

sampled :: (Show vt, Ord vt, Ord r) => Int -> Prog r vt -> IO [(r, Rational)]
sampled t prog = renormalize . tally <$> runEs t (evalProg prog)
