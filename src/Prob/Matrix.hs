{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
-- | Matrices as star semirings
module Prob.Matrix where

import Control.Monad (forM_)
import Data.Array
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST (newArray_, runSTArray, thaw)

-- | Alexandroff one-point compactification.
data Compact a
  = Real a
  | Inf

extractCompact :: Compact a -> Maybe a
extractCompact Inf = Nothing
extractCompact (Real a) = Just a

-- | A square matrix of dimension @n@, indexed by @0 .. n-1@ on both axes and
-- stored flat in row-major order.
--
-- A previous version of this had a more elegant but slower design: it relies on
-- an Ix and Bounded instances, and so the caller had to use reflection to
-- construct an appropriate Bounded instance at run time, depending on the
-- number of actual variables. This is found via profiling where the @index@
-- method cost 8% of the run time in some cases.
data Matrix e = Matrix Int (Array Int e) deriving Functor

newtype Vector e = Vector (Array Int e) deriving Functor

matrixDim :: Matrix e -> Int
matrixDim (Matrix n _) = n

-- | The entry at row @i@, column @j@.
at :: Matrix e -> Int -> Int -> e
at (Matrix n a) i j = a ! (i * n + j)

matrixFromFunc :: Int -> ((Int, Int) -> e) -> Matrix e
matrixFromFunc n f =
  Matrix n $
  listArray (0, n * n - 1) [f (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1]]

vectorFromFunc :: Int -> (Int -> e) -> Vector e
vectorFromFunc n f = Vector $ listArray (0, n - 1) (map f [0 .. n - 1])

mult :: StarSemiring e => Matrix e -> Vector e -> Vector e
{-# SPECIALIZE mult :: Matrix (Compact Rational) -> Vector (Compact Rational) -> Vector (Compact Rational) #-}
mult m (Vector b) =
  vectorFromFunc n $ \i -> srsum [at m i k <.> (b ! k) | k <- [0 .. n - 1]]
  where
    n = matrixDim m

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

srsum :: StarSemiring a => [a] -> a
{-# SPECIALIZE srsum :: [Compact Rational] -> Compact Rational #-}
srsum = foldr (<+>) zero

-- Every caller in this program asterates over @Compact Rational@, but the
-- instance is polymorphic and lives in another module, so we need to manually
-- tell GHC to do this specialization.
instance (Eq a, Num a, Fractional a) => StarSemiring (Compact a) where
  {-# SPECIALIZE instance StarSemiring (Compact Rational) #-}
  zero = Real 0
  _ <+> Inf = Inf
  Inf <+> _ = Inf
  Real x <+> Real y = Real (x + y)
  one = Real 1
  _ <.> Real 0 = Real 0
  Real 0 <.> _ = Real 0
  _ <.> Inf = Inf
  Inf <.> _ = Inf
  Real x <.> Real y = Real (x * y)
  star (Real 1) = Inf
  star (Real x) = Real (recip (1 - x))
  star Inf = Inf

-- | Matrix asteration.
starMatrix :: StarSemiring e => Matrix e -> Matrix e
{-# SPECIALIZE starMatrix :: Matrix (Compact Rational) -> Matrix (Compact Rational) #-}
starMatrix (Matrix n a0) = Matrix n $ runSTArray $ do
  src0 <- thaw a0
  dst0 <- newArray_ (0, n * n - 1)
  final <- pivotAll [0 .. n - 1] src0 dst0
  -- Adding the identity only touches the diagonal: off it the added entry is
  -- 'zero', and @zero <+> x = x@ is a semiring law.
  forM_ [0 .. n - 1] $ \i -> do
    mii <- unsafeRead final (i * n + i)
    let !v = one <+> mii
    unsafeWrite final (i * n + i) v
  pure final
  where
    -- Two buffers, swapped after each pivot.
    pivotAll [] src _ = pure src
    pivotAll (k:ks) src dst = do
      -- Loop-invariant in i and j.
      skk <- star <$> unsafeRead src (k * n + k)
      forM_ [0 .. n - 1] $ \i -> do
        mik <- unsafeRead src (i * n + k)
        forM_ [0 .. n - 1] $ \j -> do
          mij <- unsafeRead src (i * n + j)
          mkj <- unsafeRead src (k * n + j)
          let !v = mij <+> mik <.> skk <.> mkj
          unsafeWrite dst (i * n + j) v
      pivotAll ks dst src

instance (Show a) => Show (Compact a) where
  show (Real a) = show a
  show Inf = "∞"
