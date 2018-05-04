{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Dist =
  Bernoulli Rational
  deriving (Show, Eq)

infix 2 :=

infixr 3 `And`

infixr 3 `Or`

infix 2 :~

infix 0 `Return`

newtype Then varTy =
  Then [Stmt varTy]
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Else varTy =
  Else [Stmt varTy]
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Stmt varTy
  = Skip
  | varTy := (Expr varTy)
  | varTy :~ Dist
  | Observe (Expr varTy)
  | If (Expr varTy)
       (Then varTy)
       (Else varTy)
  | While (Expr varTy)
          [Stmt varTy]
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Prog r varTy where
  Return :: [Stmt varTy] -> Expr varTy -> Prog Bool varTy
  ReturnAll :: [Stmt varTy] -> Prog (Sigma varTy) varTy
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

evalStmt :: (Show vt, Ord vt) => [Stmt vt] -> Eval vt s ()
evalStmt [] = pure ()
evalStmt (Skip:next) = evalStmt next
evalStmt ((x := a):next) = do
  v <- evalExpr a
  modify (first (M.insert x v))
  evalStmt next
evalStmt ((x :~ d):next) = do
  v <- drawDist d
  modify (first (M.insert x v))
  evalStmt next
evalStmt (Observe e:next) = do
  e' <- evalExpr e
  if e'
    then evalStmt next
    else MaybeT $ pure Nothing
evalStmt (If e (Then thenn) (Else alt):next) = do
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
-- Denotational Semantics
--------------------------------------------------------------------------------
-- | Sigma is just the set of all variables assignments.
type Sigma vt = M.Map vt Bool

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
denStmt cl (Skip:next) sigma' sigma = denStmt cl next sigma' sigma
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
    unrollOnce nl = denStmt nl (If e (Then (s ++ [loop])) (Else [Skip]) : next) sigma' sigma

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
prog1 = ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5] `Return` "c1" `And` "c2"

prog2 :: Prog Bool String
prog2 = ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5, Observe ("c1" `Or` "c2")] `Return` "c1" `And` "c2"

prog1' :: Prog (M.Map String Bool) String
prog1' = ReturnAll ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5 ]

prog2' :: Prog (M.Map String Bool) String
prog2' = ReturnAll ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5, Observe ("c1" `Or` "c2")]

progDice :: Prog (M.Map String Bool) String
progDice =
  ReturnAll
    [ "bit 0" :~ Bernoulli 0.5
    , "bit 1" :~ Bernoulli 0.5
    , "bit 2" :~ Bernoulli 0.5
  -- not all zeroes
    , Observe ("bit 0" `Or` "bit 1" `Or` "bit 2")
  -- not all ones
    , Observe (Not "bit 0" `Or` Not "bit 1" `Or` Not "bit 2")
    ]

progGeo :: Prog Bool String
progGeo = ["b" := Constant True, "p" := Constant False, While "b" ["b" :~ Bernoulli 0.5, "p" := Not "p"]] `Return` "p"

progGeo2 :: Prog (M.Map String Bool) String
progGeo2 =
  ReturnAll
    [ "b" := Constant True
    , "x0" := Constant True
    , "x1" := Constant False
    , "x2" := Constant False
    , While "b" ["b" :~ Bernoulli 0.5, next]
    ]
  where
    next =
      If
        "x0"
        (Then ["x0" := Constant False, "x1" := Constant True])
        (Else
           [ If
               "x1"
               (Then ["x1" := Constant False, "x2" := Constant True])
               (Else [If "x2" (Then ["x2" := Constant False, "x0" := Constant True]) (Else [Skip])])
           ])

progGeo3 :: Prog (M.Map String Bool) String
progGeo3 =
  ReturnAll
    [ "b" := Constant True
    , "x0" := Constant True
    , "x1" := Constant False
    , "x2" := Constant False
    , While "b" ["b" :~ Bernoulli 0.5, next]
    ]
  where
    next =
      If
        "x0"
        (Then ["x0" := Constant False, "x1" := Constant True])
        (Else
           [ If
               "x1"
               (Then ["x1" := Constant False, "x2" := Constant True])
               (Else [If "x2" (Then ["x2" := Constant False, "x1" := Constant True]) (Else [Skip])])
           ])
