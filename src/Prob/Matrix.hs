{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
-- | Matrices as star semirings
module Prob.Matrix where

import Data.Array

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
srsum = foldr (<+>) zero

instance (Eq a, Num a, Fractional a) => StarSemiring (Compact a) where
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

-- | Matrix asteration, by the Kleene recurrence: pivot on each @k@ in turn,
-- then add the identity. This is not a 'StarSemiring' instance because 'zero'
-- and 'one' would each have to conjure a dimension out of nothing.
starMatrix :: StarSemiring e => Matrix e -> Matrix e
starMatrix m0 = addOne (foldr pivot m0 [0 .. n - 1])
  where
    n = matrixDim m0
    pivot k m = matrixFromFunc n build
      where
        -- Loop-invariant in i and j, so it is named here rather than left in
        -- 'build' for the simplifier to float out.
        skk = star (at m k k)
        build (i, j) = at m i j <+> at m i k <.> skk <.> at m k j
    addOne m =
      matrixFromFunc n $ \(i, j) ->
        (if i == j then one else zero) <+> at m i j

instance (Show a) => Show (Compact a) where
  show (Real a) = show a
  show Inf = "∞"
