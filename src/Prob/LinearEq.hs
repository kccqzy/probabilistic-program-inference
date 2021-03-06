{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
module Prob.LinearEq
  ( Term(..)
  , RHS(..)
  , Equation(..)
  , System
  , solve
  ) where

import Data.Monoid
import Data.Array
import Data.Proxy
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Reflection
import qualified Data.Set as Set
import Prob.Matrix

-- | A term is a rational multiplied by a variable.
data Term x = Term Rational x deriving (Show, Functor, Foldable, Traversable)

-- | An equation has the form var = constant + term1 + term2 + ...
data Equation x = Equation x (RHS x) deriving (Show, Functor, Foldable, Traversable)

data RHS x = RHS Rational [Term x] deriving (Show, Functor, Foldable, Traversable)

-- | A system is a bunch of equations, each of the form var = constant + term1 + term2 + ...
type System x = [Equation x]

-- | Represents an integral type with its bounds readjusted.
newtype BoundedW i s = BoundedW {unBoundedW :: i} deriving (Eq, Ord, Ix)

-- | The real bounds.
data Bounds i = Bounds {minBound_ , maxBound_ :: i}

instance (Reifies s (Bounds i)) => Bounded (BoundedW i s) where
  minBound = BoundedW (minBound_ (reflect (Proxy :: Proxy s)))
  maxBound = BoundedW (maxBound_ (reflect (Proxy :: Proxy s)))

solve ::
     forall x. (Ord x)
  => System x
  -> Maybe (M.Map x Rational)
solve eqns = reify (Bounds 0 (Set.size vars - 1)) f
  where
    vars :: Set.Set x
    vars = Set.fromList (concatMap toList eqns)
    indexMap :: M.Map x Int
    indexMap = M.fromList (zip (Set.toList vars) [0 ..])
    indexRMap :: M.Map Int x
    indexRMap = M.fromList (zip [0 ..] (Set.toList vars))
    reindexedEqns :: [Equation Int]
    reindexedEqns = (fmap . fmap) (indexMap M.!) eqns
    f :: forall s. Reifies s (Bounds Int)
      => Proxy s
      -> Maybe (M.Map x Rational)
    f _ = M.fromList <$> traverse (\(i, r) -> (indexRMap M.! unBoundedW i, ) <$> extractCompact r) (assocs x)
      where
        x :: Array (BoundedW Int s) (Compact Rational)
        (Vector x) = solveAffine a b
        b :: Vector (BoundedW Int s) Rational
        b =
          vectorFromFunc $ \(BoundedW i) ->
            getSum $ foldMap (\(Equation _ (RHS c _)) -> Sum c) (filter (\(Equation i' _) -> i == i') reindexedEqns)
        a :: Matrix (BoundedW Int s) Rational
        a =
          matrixFromFunc $ \(BoundedW i, BoundedW j) ->
            case filter (\(Equation i' _) -> i == i') reindexedEqns of
              (Equation _ (RHS _ tms):_) -> getSum $ foldMap (\(Term c _) -> Sum c) (filter (\(Term _ j') -> j == j') tms)
              [] -> error $ "no such equation when trying to construct coefficient matrix for system " ++ show reindexedEqns
