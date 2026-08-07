{-# LANGUAGE RankNTypes #-}
module Prob.Eval
  ( runE
  , runEs
  , Eval
  , ProgState
  , evalProg
  , sampled
  , tally
  ) where

import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Control.Monad
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

runE :: (forall s. Eval vt s a) -> IO (Maybe a)
runE e = withSystemRandomST $ \rng -> evalStateT (runMaybeT e) (Set.empty, rng)

-- | Sample @t@ independent runs. The random number generator is threaded from
-- one run to the next, but the variable assignment must not be: it is reset
-- before each run, or a run would start from wherever its predecessor ended.
-- (Only a program that reads a variable before assigning it can tell, which is
-- why an all-boolean program that initializes everything never noticed.)
runEs :: Int -> (forall s. Eval vt s a) -> IO [a]
runEs t e =
  withSystemRandomST $ \rng ->
    catMaybes <$>
    evalStateT
      (replicateM t (modify (first (const Set.empty)) >> runMaybeT e))
      (Set.empty, rng)

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
evalStmt s@(While _ e stmt:next) = do
  e' <- evalExpr e
  if e'
    then evalStmt stmt >> evalStmt s
    else evalStmt next

evalProg :: (Show vt, Ord vt) => Prog vt -> Eval vt s [Bool]
evalProg (ReturnMult stmt exprs) = evalStmt stmt >> traverse evalExpr exprs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
tally :: Ord a => [a] -> [(a, Int)]
tally = map (liftA2 (,) NE.head length) . NE.group . sort

renormalize :: [(a, Int)] -> [(a, Rational)]
renormalize l = fmap (fmap (\n -> fromIntegral n % fromIntegral tot)) l
  where tot = sum (map snd l)

sampled :: (Show vt, Ord vt) => Int -> Prog vt -> IO [([Bool], Rational)]
sampled t prog = renormalize . tally <$> runEs t (evalProg prog)
