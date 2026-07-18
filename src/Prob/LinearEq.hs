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

-- | The variables of the system in index order, reified so that the solver's
-- index type can recover a variable from its position with no separate map.
newtype VarArr x = VarArr (Array Int x)

-- | The index type threaded through the star-semiring solver: a variable's
-- position — which drives the O(1) 'Ix' and 'Bounded' instances — paired with
-- the variable itself, so results read back without a lookup. The payload is
-- lazy (@~x@) so that 'minBound'/'maxBound' on an empty system never
-- dereference the (empty) 'VarArr'.
data VarW x s = VarW !Int ~x

instance Eq (VarW x s) where
  VarW i _ == VarW j _ = i == j

instance Ord (VarW x s) where
  compare (VarW i _) (VarW j _) = compare i j

-- | The 'VarW' at a given position, its variable read from the reified array.
varAt :: forall s x. Reifies s (VarArr x) => Int -> VarW x s
varAt i = let VarArr a = reflect (Proxy :: Proxy s) in VarW i (a ! i)

instance Reifies s (VarArr x) => Bounded (VarW x s) where
  minBound = varAt 0
  maxBound =
    let VarArr a = reflect (Proxy :: Proxy s)
     in varAt (snd (bounds a))

-- | Indices are ranked by position, so every 'Ix' operation is O(1) integer
-- arithmetic.
instance Reifies s (VarArr x) => Ix (VarW x s) where
  range (VarW lo _, VarW hi _) = map varAt [lo .. hi]
  index (VarW lo _, _) (VarW i _) = i - lo
  inRange (VarW lo _, VarW hi _) (VarW i _) = lo <= i && i <= hi

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
solveMany rows bs = reify (VarArr varArr) f
  where
    vars :: Set.Set x
    vars = Set.fromList (concatMap toList rows) <> foldMap M.keysSet bs
    -- The variables in order as an array.
    varArr :: Array Int x
    varArr = listArray (0, Set.size vars - 1) (Set.toAscList vars)
    -- The coefficient as a sparse map.
    coeffMap :: M.Map (x, x) Rational
    coeffMap =
      M.fromListWith (+)
        [ ((x, y), c) | Row x tms <- rows, Term c y <- tms ]
    f :: forall s. Reifies s (VarArr x)
      => Proxy s
      -> Maybe (t (Vec x))
    f _ = traverse extract (solveAffineMany a bsVec)
      where
        a :: Matrix (VarW x s) Rational
        a =
          matrixFromFunc $ \(VarW _ x, VarW _ y) ->
            M.findWithDefault 0 (x, y) coeffMap
        bsVec :: t (Vector (VarW x s) Rational)
        bsVec = fmap toVec bs
          where
            toVec b = vectorFromFunc $ \(VarW _ x) -> M.findWithDefault 0 x b
        extract :: Vector (VarW x s) (Compact Rational) -> Maybe (Vec x)
        extract (Vector arr) =
          M.filter (/= 0) . M.fromList <$>
          traverse
            (\(VarW _ x, c) -> (x, ) <$> extractCompact c)
            (assocs arr)
