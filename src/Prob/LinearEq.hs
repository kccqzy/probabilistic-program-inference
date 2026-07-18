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
  , solveRow
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
-- The constant @b@ is deliberately /not/ stored here — the constant columns
-- are supplied separately, per variable, to 'solveRow'.
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

-- | Compute the @target@ coordinate of the solutions of @x = A x + b@ for a
-- whole collection of constant columns @b@ at once — supplied not as (dense)
-- columns but as a sparse map, as @[(st, b_st)]@ where @b_st@ maps each column
-- label @k@ to that column's constant for variable @st@. Notice that given that
-- @target@ coordinate, we essentially want @e_target^T x@.
--
-- Internally, for performance reasons, this solves a different system, namely
-- @y = A^T y + e_target@, so @y = (A^T)* e_target@ and @y^T = e_target^T A*@.
-- So the final result is also written as @e_target^T x = y^T b@. This is more
-- performant than solving the original system.
solveRow ::
     forall x k. (Ord x, Ord k)
  => Coeffs x
  -> x
  -> [(x, Vec k)]
  -> Maybe (Vec k)
solveRow rows target bs = reify (VarArr varArr) f
  where
    vars :: Set.Set x
    vars = Set.insert target (Set.fromList (concatMap toList rows))
    -- The variables in order as an array.
    varArr :: Array Int x
    varArr = listArray (0, Set.size vars - 1) (Set.toAscList vars)
    -- The transposed coefficients as a sparse map.
    coeffMapT :: M.Map (x, x) Rational
    coeffMapT =
      M.fromListWith (+)
        [ ((j, i), c) | Row i tms <- rows, Term c j <- tms ]
    f :: forall s. Reifies s (VarArr x)
      => Proxy s
      -> Maybe (Vec k)
    f _ = combine (solveAffine a b)
      where
        a :: Matrix (VarW x s) Rational
        a =
          matrixFromFunc $ \(VarW _ x, VarW _ y) ->
            M.findWithDefault 0 (x, y) coeffMapT
        b :: Vector (VarW x s) Rational
        b = vectorFromFunc $ \(VarW _ x) -> if x == target then 1 else 0
        combine :: Vector (VarW x s) (Compact Rational) -> Maybe (Vec k)
        combine (Vector arr) =
          fmap (M.filter (/= 0)) $
          traverse extractCompact $
          M.fromListWith (<+>)
            [ (k, y <.> Real mass)
            | (st, bst) <- bs
            -- y is the specific row in the solution corresponding to st
            , let y = M.findWithDefault (Real 0) st yMap
            , (k, mass) <- M.toList bst
            ]
          where
            -- The y vector (solution) converted to Map form.
            yMap :: M.Map x (Compact Rational)
            yMap = M.fromList [(v, c) | (VarW _ v, c) <- assocs arr]
