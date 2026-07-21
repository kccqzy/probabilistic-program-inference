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
import Data.Graph (SCC(..), stronglyConnComp)
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
--
-- The transposed system is solved sparsely: the dependency graph of the
-- variables is decomposed into strongly connected components, which are
-- processed in dependency order. A variable in a trivial component is obtained
-- by direct substitution of already-solved variables; only nontrivial
-- components (the genuine recurrences) are solved as dense blocks, with the
-- star-semiring 'Prob.Matrix' machinery restricted to the component. Solving
-- component-by-component in dependency order is valid in any ω-continuous
-- star semiring, so the result is the same as asterating the full matrix,
-- at typically far lower cost.
solveRow ::
     forall x k. (Ord x, Ord k)
  => Coeffs x
  -> x
  -> [(x, Vec k)]
  -> Maybe (Vec k)
solveRow rows target bs = combine
  where
    vars :: Set.Set x
    vars = Set.insert target (Set.fromList (concatMap toList rows))
    -- The dependencies of each variable in the transposed system: @y j@
    -- depends on @y i@ with coefficient @A i j@, for every row @i@ whose terms
    -- mention @j@. Terms with a zero coefficient are dropped so that they
    -- neither enlarge components nor add substitution work.
    depsMap :: M.Map x (M.Map x Rational)
    depsMap =
      M.fromListWith (M.unionWith (+))
        [ (j, M.singleton i c) | Row i tms <- rows, Term c j <- tms, c /= 0 ]
    deps :: x -> M.Map x Rational
    deps j = M.findWithDefault M.empty j depsMap
    e :: x -> Compact Rational
    e j = if j == target then Real 1 else Real 0
    -- 'stronglyConnComp' returns components in reverse topological order.
    -- Notice that if @y j@ depends on @y i@ there is an edge from @j@ to @i@.
    -- So the reverse topological order would put @i@ before @j@ (or in the same
    -- component). This is the order we want.
    comps :: [SCC x]
    comps = stronglyConnComp [(j, j, M.keys (deps j)) | j <- Set.toList vars]
    yMap :: M.Map x (Compact Rational)
    yMap = foldl' solveComp M.empty comps
    solvedAt :: M.Map x (Compact Rational) -> x -> Compact Rational
    solvedAt sol i = M.findWithDefault (Real 0) i sol
    solveComp ::
         M.Map x (Compact Rational) -> SCC x -> M.Map x (Compact Rational)
    -- A single variable. Just substitution.
    solveComp sol (AcyclicSCC j) =
      M.insert
        j
        (e j <+> srsum [Real c <.> solvedAt sol i | (i, c) <- M.toList (deps j)])
        sol
    -- A nontrivial component: solve the dense block @y_C = B y_C + r@ where @B@ is the
    -- within-component coefficients and @r@ collects @e@ and the already-solved
    -- external contributions.
    solveComp sol (CyclicSCC comp) =
      foldl' (\s (j, y) -> M.insert j y s) sol (solveBlock sol (Set.fromList comp))
    solveBlock ::
         M.Map x (Compact Rational) -> Set.Set x -> [(x, Compact Rational)]
    solveBlock sol compSet = reify (VarArr blockArr) g
      where
        blockArr :: Array Int x
        blockArr = listArray (0, Set.size compSet - 1) (Set.toAscList compSet)
        rMap :: M.Map x (Compact Rational)
        rMap =
          M.fromSet
             ( \j
             -> e j <+>
                srsum
                  [ Real c <.> solvedAt sol i
                  | (i, c) <- M.toList (deps j)
                  , i `Set.notMember` compSet
                  ]) compSet
        g :: forall s. Reifies s (VarArr x)
          => Proxy s
          -> [(x, Compact Rational)]
        g _ = [(v, c) | (VarW _ v, c) <- assocs arr]
          where
            bMat :: Matrix (VarW x s) Rational
            bMat =
              matrixFromFunc $ \(VarW _ j, VarW _ i) ->
                M.findWithDefault 0 i (deps j)
            rVec :: Vector (VarW x s) (Compact Rational)
            rVec = vectorFromFunc $ \(VarW _ j) -> rMap M.! j
            Vector arr = star (fmap Real bMat) `mult` rVec
    combine :: Maybe (Vec k)
    combine =
      fmap (M.filter (/= 0)) $
      traverse extractCompact $
      M.fromListWith (<+>)
        [ (k, y <.> Real m)
        | (st, bst) <- bs
        -- y is the specific row in the solution corresponding to st
        , let y = M.findWithDefault (Real 0) st yMap
        , (k, m) <- M.toList bst
        ]
