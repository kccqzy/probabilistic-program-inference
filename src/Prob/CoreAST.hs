{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
module Prob.CoreAST
  ( Expr(..)
  , Dist(..)
  , Stmt(..)
  , Prog(..)
  , Sigma
  , sigmaInsert
  , mkNot
  , mkAnd
  , mkOr
  , mkXor
  ) where

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

infix 0 `Return`

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

data Prog r varTy where
  Return :: [Stmt varTy] -> Expr varTy -> Prog Bool varTy
  ReturnAll :: [Stmt varTy] -> Prog (Sigma varTy) varTy
deriving instance Show varTy => Show (Prog r varTy)
deriving instance Foldable (Prog r)

instance IsString (Expr String) where
  fromString = Var

-- | Sigma is just the set of all variables assignments. Since our language only
-- ever deals with Bool variables, we use a 'Set.Set' and the presence/absence
-- indicates their values.
type Sigma vt = Set.Set vt

sigmaInsert :: Ord vt => vt -> Bool -> Sigma vt -> Sigma vt
sigmaInsert x v = (if v then Set.insert else Set.delete) x

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

mkAnd :: Expr vt -> Expr vt -> Expr vt
mkAnd (Constant False) _ = Constant False
mkAnd _ (Constant False) = Constant False
mkAnd (Constant True) e = e
mkAnd e (Constant True) = e
mkAnd a b = And a b

mkOr :: Expr vt -> Expr vt -> Expr vt
mkOr (Constant True) _ = Constant True
mkOr _ (Constant True) = Constant True
mkOr (Constant False) e = e
mkOr e (Constant False) = e
mkOr a b = Or a b

mkXor :: Expr vt -> Expr vt -> Expr vt
mkXor (Constant False) e = e
mkXor e (Constant False) = e
mkXor (Constant True) e = mkNot e
mkXor e (Constant True) = mkNot e
mkXor a b = Xor a b
