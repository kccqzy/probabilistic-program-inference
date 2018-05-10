{-# LANGUAGE StrictData #-}
-- | Matrices as star semirings
module Prob.Matrix where

import Control.Applicative
import Data.Array

-- | Alexandroff one-point compactification.
data Compact a
  = Real a
  | Inf

-- | A matrix, which is an array of array of elements, indexed by i.
newtype Matrix i e = Matrix (Array i (Array i e))

matrixFromFunc :: (Ix i, Bounded i) => ((i, i) -> e) -> Matrix i e
matrixFromFunc f =
  Matrix $
  listArray (minBound, maxBound) $
  map (\i -> listArray (minBound, maxBound) $ map (\j -> f (i, j)) entireRange) entireRange

inverse :: (Eq a, Ix i, Bounded i, Fractional a) => Matrix i a -> Matrix i (Compact a)
inverse m = star (one <+> fmap (Real . negate) m)

entireRange :: (Ix i, Bounded i) => [i]
entireRange = range (minBound, maxBound)

infixl 6 <+>
infixl 7 <.>

{- | A star semiring satisfies the following laws :
   a <+> b = b <+> a
   (a <+> b) <+> c = a <+> (b <+> c)
   a <+> zero  = zero <+> a  = a
   (a <.> b) <.> c = a <.> (b <.> c)
   a <.> one  = one <.> a  = a
   a <.> zero = zero <.> a = zero
   a <.> (b <+> c) = a <.> b <+> a <.> c
   (a <+> b) <.> c = a <.> c <+> b <.> c
   star a = one <+> a <.> star a -- RECURSIVE! It's an infinite sum
          = one <+> star a <.> a
-}
class StarSemiring a where
  zero :: a
  (<+>) :: a -> a -> a
  one :: a
  (<.>) :: a -> a -> a
  star :: a -> a

instance (Eq a, Num a, Fractional a) => StarSemiring (Compact a) where
  zero = Real 0
  Inf <+> _ = Inf
  _ <+> Inf = Inf
  Real x <+> Real y = Real (x + y)
  one = Real 1
  Real 0 <.> _ = Real 0
  _ <.> Real 0 = Real 0
  Inf <.> _ = Inf
  _ <.> Inf = Inf
  Real x <.> Real y = Real (x * y)
  star (Real 1) = Inf
  star (Real x) = Real (recip (1 - x))
  star Inf = Inf

instance (Ix i, Bounded i, StarSemiring a) => StarSemiring (Matrix i a) where
  zero = pure zero -- zero matrix
  (<+>) = liftA2 (<+>) -- matrix addition
  one = -- identity matrix
    matrixFromFunc
      (\(i, j) ->
         if i == j
           then one
           else zero)
  Matrix x <.> Matrix y = matrixFromFunc build -- matrix multiplication
    where
      build (i, j) = foldr (<+>) zero [x ! i ! k <.> y ! k ! j | k <- entireRange]
  star x = one <+> foldr f x entireRange -- matrix asteration
    where
      f k (Matrix m) = matrixFromFunc build
        where
          build (i, j) = m ! i ! j <+> m ! i ! k <.> star (m ! k ! k) <.> m ! k ! j

instance (Ix i) => Functor (Matrix i) where
  fmap f (Matrix m) = Matrix ((fmap . fmap) f m)

instance (Ix i, Bounded i) => Applicative (Matrix i) where
  pure x = matrixFromFunc (const x)
  Matrix f <*> Matrix x = matrixFromFunc (\(i, j) -> (f ! i ! j) (x ! i ! j))
  liftA2 f (Matrix ma) (Matrix mb) = matrixFromFunc (\(i, j) -> f (ma ! i ! j) (mb ! i ! j))

instance (Show a) => Show (Compact a) where
  show (Real a) = show a
  show Inf = "∞"
