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
  ) where

import qualified Data.Map.Strict as M
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

data Dist =
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

-- | Sigma is just the set of all variables assignments.
type Sigma vt = M.Map vt Bool
