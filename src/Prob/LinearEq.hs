{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
module Prob.LinearEq
  ( Term(..)
  , Row(..)
  , Coeffs
  , Vec
  , solveMany
  ) where

import Data.Array
import Data.Proxy
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Reflection
import qualified Data.Set as Set
import Prob.Matrix

-- | A term is a rational multiplied by a variable.
data Term x = Term Rational x deriving (Show, Functor, Foldable, Traversable)

-- | One row of the coefficient matrix @A@ of a system @x = A x + b@: the
-- variable the row defines, together with its linear combination of variables.
-- The constant @b@ is deliberately /not/ stored here — it is supplied
-- separately, and in bulk, to 'solveMany'. Keeping @A@ apart from the
-- right-hand sides allows a single factorization to serve many @b@s.
data Row x = Row x [Term x] deriving (Show, Functor, Foldable, Traversable)

-- | The coefficient matrix @A@ of a system @x = A x + b@, one 'Row' per
-- variable.
type Coeffs x = [Row x]

-- | A sparse vector indexed by variables; a missing key denotes zero.
type Vec x = M.Map x Rational

-- | Represents an integral type with its bounds readjusted.
newtype BoundedW i s = BoundedW i deriving (Eq, Ord, Ix)

-- | The real bounds.
data Bounds i = Bounds {minBound_ , maxBound_ :: i}

instance (Reifies s (Bounds i)) => Bounded (BoundedW i s) where
  minBound = BoundedW (minBound_ (reflect (Proxy :: Proxy s)))
  maxBound = BoundedW (maxBound_ (reflect (Proxy :: Proxy s)))

-- | Solve @x = A x + b@ for a whole collection of right-hand sides @b@ at once.
--
-- This is the standard "same factorization, many right-hand sides" primitive,
-- specialized to the star-semiring solver: the expensive part, @star A@, depends
-- only on the coefficient matrix, so 'solveAffineMany' computes it once and
-- reuses it for every column @b@. The columns live in an arbitrary traversable
-- @t@ — the labelling of the right-hand sides is the caller's concern, not this
-- module's.
--
-- Returns 'Nothing' exactly when some column diverges — an @Inf@ entry of
-- @star A@ meeting nonzero mass, detected coordinate-wise by 'extractCompact'.
solveMany ::
     forall t x. (Ord x, Traversable t)
  => Coeffs x
  -> t (Vec x)
  -> Maybe (t (Vec x))
solveMany rows bs = reify (Bounds 0 (Set.size vars - 1)) f
  where
    vars :: Set.Set x
    vars = Set.fromList (concatMap toList rows) <> foldMap M.keysSet bs
    indexMap :: M.Map x Int
    indexMap = M.fromList (zip (Set.toList vars) [0 ..])
    indexRMap :: M.Map Int x
    indexRMap = M.fromList (zip [0 ..] (Set.toList vars))
    -- Row i of A as a sparse map @j -> coefficient@. Every variable has (at
    -- most) one row; a variable with no row is an all-zero row, i.e. @x = b_x@.
    coeffMap :: M.Map Int (M.Map Int Rational)
    coeffMap =
      M.fromListWith (M.unionWith (+))
        [ (indexMap M.! x, M.fromListWith (+) [(indexMap M.! y, c) | Term c y <- tms])
        | Row x tms <- rows
        ]
    f :: forall s. Reifies s (Bounds Int)
      => Proxy s
      -> Maybe (t (Vec x))
    f _ = traverse extract (solveAffineMany a bsVec)
      where
        a :: Matrix (BoundedW Int s) Rational
        a =
          matrixFromFunc $ \(BoundedW i, BoundedW j) ->
            M.findWithDefault 0 j (M.findWithDefault M.empty i coeffMap)
        bsVec :: t (Vector (BoundedW Int s) Rational)
        bsVec = fmap toVec bs
          where
            toVec b =
              let bInt = M.mapKeys (indexMap M.!) b
               in vectorFromFunc $ \(BoundedW i) -> M.findWithDefault 0 i bInt
        extract :: Vector (BoundedW Int s) (Compact Rational) -> Maybe (Vec x)
        extract (Vector arr) =
          M.filter (/= 0) . M.fromList <$>
          traverse
            (\(BoundedW i, c) -> (indexRMap M.! i, ) <$> extractCompact c)
            (assocs arr)
