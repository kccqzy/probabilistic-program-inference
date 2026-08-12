{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
module Prob.CoreAST
  ( Expr(..)
  , Dist(..)
  , Stmt(..)
  , Prog(..)
  , Sigma
  , sigmaInsert
  , toIntProg
  , mkNot
  , mkAnd
  , mkOr
  , mkXor
  ) where

import Data.Foldable
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import Data.String

-- | The syntax of expressions, parametrized by the representation of variables
-- (usually strings).
data Expr varTy
  = Var varTy
  | Constant Bool
  | (Expr varTy) `And` (Expr varTy)
  | (Expr varTy) `Or` (Expr varTy)
  | (Expr varTy) `Xor` (Expr varTy)
  | Not (Expr varTy)
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Dist =
  Bernoulli Rational
  deriving (Show, Eq)

infix 2 :=

infixr 3 `And`

infixr 3 `Or`

infix 2 :~

infix 0 `ReturnMult`

data Stmt varTy
  = varTy := (Expr varTy)
  | varTy :~ Dist
  | Observe (Expr varTy)
  | If (Expr varTy)
       [Stmt varTy]
       [Stmt varTy]
  | While Int -- The Int here is a loop label.
          (Expr varTy)
          [Stmt varTy]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | A program: the statements to run, and the booleans to return.
data Prog varTy =
  [Stmt varTy] `ReturnMult` [Expr varTy]
  deriving (Show, Functor, Foldable, Traversable)

instance IsString (Expr String) where
  fromString = Var

-- | Sigma is just the set of all variables assignments. Since our language only
-- ever deals with Bool variables, we use a set and the presence/absence
-- indicates their values. The inference engines ('Prob.Den', 'Prob.Eval') run
-- on programs whose variables are ints, so this is an 'IS.IntSet'. States are
-- also used as map keys, so the comparison matters as much as the
-- representation: we use @containers >= 0.8@ because that version improves
-- performance of the Ord instance.
type Sigma = IS.IntSet

sigmaInsert :: Int -> Bool -> Sigma -> Sigma
sigmaInsert x v = (if v then IS.insert else IS.delete) x

-- | Number the variables of a program. Each variable becomes its index in the
-- sorted set of all variables occurring in the program.
toIntProg :: Ord vt => Prog vt -> Prog Int
toIntProg p = fmap (`Set.findIndex` vars) p
  where
    vars = Set.fromList (toList p)

--------------------------------------------------------------------------------
-- The expression simplifier
--------------------------------------------------------------------------------

-- Smart constructors implementing constant folding and the local boolean
-- identities. Each assumes its arguments are already simplified, so building an
-- expression exclusively out of them yields a simplified expression.

mkNot :: Expr vt -> Expr vt
mkNot (Constant b) = Constant (not b)
mkNot (Not e) = e
mkNot e = Not e

mkAnd :: Eq vt => Expr vt -> Expr vt -> Expr vt
mkAnd (Constant False) _ = Constant False
mkAnd _ (Constant False) = Constant False
mkAnd (Constant True) e = e
mkAnd e (Constant True) = e
mkAnd a b = if a == b then a else And a b

mkOr :: Eq vt => Expr vt -> Expr vt -> Expr vt
mkOr (Constant True) _ = Constant True
mkOr _ (Constant True) = Constant True
mkOr (Constant False) e = e
mkOr e (Constant False) = e
mkOr a b = if a == b then a else Or a b

mkXor :: Eq vt => Expr vt -> Expr vt -> Expr vt
mkXor (Constant False) e = e
mkXor e (Constant False) = e
mkXor (Constant True) e = mkNot e
mkXor e (Constant True) = mkNot e
mkXor a b = if a == b then Constant False else Xor a b
