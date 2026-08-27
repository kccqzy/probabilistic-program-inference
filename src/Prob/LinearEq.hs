{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
module Prob.LinearEq
  ( Term(..)
  , Row(..)
  , Coeffs
  , Vec
  , solveRows
  ) where

import Data.Array
import Data.Foldable
import Data.Graph (SCC(..), stronglyConnComp)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Prob.Matrix

-- | A term is a rational multiplied by a variable.
data Term x = Term Rational x deriving (Show, Functor, Foldable, Traversable)

-- | One row of the coefficient matrix @A@ of a system @x = A x + b@: the
-- variable the row defines, together with its linear combination of variables.
-- The constant @b@ is deliberately /not/ stored here — the constant columns
-- are supplied separately, per variable, to 'solveRows'.
data Row x = Row x [Term x] deriving (Show, Functor, Foldable, Traversable)

-- | The coefficient matrix @A@ of a system @x = A x + b@, one 'Row' per
-- variable.
type Coeffs x = [Row x]

-- | A sparse vector indexed by variables; a missing key denotes zero.
type Vec x = M.Map x Rational

-- | Compute the @targets@ coordinates of the solutions of @x = A x + b@ for a
-- whole collection of constant columns @b@ at once — supplied not as (dense)
-- columns but as a sparse map, as @[(st, b_st)]@ where @b_st@ maps each column
-- label @k@ to that column's constant for variable @st@. The result has one
-- entry per target @t@ (present even when its row is all zero): the map from
-- each column label to the @t@ coordinate of that column's solution.
--
-- Internally, for performance reasons, this solves a different system, namely
-- @Y = A^T Y + E@ where @E@ has one indicator column @e_t@ per target, so
-- @Y = (A^T)* E@ and row @t@ of the answer is @Y(·)[t]^T b@. This is more
-- performant than solving the original system: the work per variable scales
-- with the number of /targets/ whose rows pass through it, not with the width
-- of the exit vectors @b_st@ — and the expensive step, asterating the
-- recurrent blocks, is paid once no matter how many targets are asked for.
-- A caller who wants several rows should therefore ask for them in one call.
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
solveRows ::
     forall x k. (Ord x, Ord k)
  => Coeffs x
  -> Set.Set x
  -> [(x, Vec k)]
  -> Maybe (M.Map x (Vec k))
solveRows rows targetSet bs = combine
  where
    vars :: Set.Set x
    vars = targetSet `Set.union` Set.fromList (concatMap toList rows)
    -- The dependencies of each variable in the transposed system: @Y j@
    -- depends on @Y i@ with coefficient @A i j@, for every row @i@ whose terms
    -- mention @j@. Terms with a zero coefficient are dropped so that they
    -- neither enlarge components nor add substitution work.
    depsMap :: M.Map x (M.Map x Rational)
    depsMap =
      M.fromListWith (M.unionWith (+))
        [ (j, M.singleton i c) | Row i tms <- rows, Term c j <- tms, c /= 0 ]
    deps :: x -> M.Map x Rational
    deps j = M.findWithDefault M.empty j depsMap
    -- @Y j@ is a sparse row of @E@-derived values: for each target @t@, the
    -- Compact scalar @star(A^T)[j, t] = star(A)[t, j]@.
    e :: x -> M.Map x (Compact Rational)
    e j = if j `Set.member` targetSet then M.singleton j (Real 1) else M.empty
    vplus :: M.Map x (Compact Rational) -> M.Map x (Compact Rational) -> M.Map x (Compact Rational)
    vplus = M.unionWith (<+>)
    vsum :: [M.Map x (Compact Rational)] -> M.Map x (Compact Rational)
    vsum = foldl' vplus M.empty
    vscale :: Compact Rational -> M.Map x (Compact Rational) -> M.Map x (Compact Rational)
    vscale s = fmap (s <.>)
    -- 'stronglyConnComp' returns components in reverse topological order.
    -- Notice that if @Y j@ depends on @Y i@ there is an edge from @j@ to @i@.
    -- So the reverse topological order would put @i@ before @j@ (or in the same
    -- component). This is the order we want.
    comps :: [SCC x]
    comps = stronglyConnComp [(j, j, M.keys (deps j)) | j <- Set.toList vars]
    yMap :: M.Map x (M.Map x (Compact Rational))
    yMap = foldl' solveComp M.empty comps
    solvedAt :: M.Map x (M.Map x (Compact Rational)) -> x -> M.Map x (Compact Rational)
    solvedAt sol i = M.findWithDefault M.empty i sol
    solveComp ::
         M.Map x (M.Map x (Compact Rational)) -> SCC x -> M.Map x (M.Map x (Compact Rational))
    -- A single variable. Just substitution.
    solveComp sol (AcyclicSCC j) =
      M.insert
        j
        (e j `vplus` vsum [vscale (Real c) (solvedAt sol i) | (i, c) <- M.toList (deps j)])
        sol
    -- A nontrivial component: solve the dense block @Y_C = B Y_C + r@ where @B@ is the
    -- within-component coefficients and @r@ collects @E@ and the already-solved
    -- external contributions; then @Y_C = star B · r@.
    solveComp sol (CyclicSCC comp) =
      foldl' (\s (j, yj) -> M.insert j yj s) sol (solveBlock sol (Set.fromList comp))
    solveBlock ::
         M.Map x (M.Map x (Compact Rational)) -> Set.Set x -> [(x, M.Map x (Compact Rational))]
    solveBlock sol compSet =
      [ (blockArr ! rj, vsum [vscale (at starB rj ri) (rArr ! ri) | ri <- [0 .. n - 1], not (M.null (rArr ! ri))])
      | rj <- [0 .. n - 1] ]
      where
        -- The component's variables in index order.
        blockVars :: [x]
        blockVars = Set.toAscList compSet
        n :: Int
        n = Set.size compSet
        blockArr :: Array Int x
        blockArr = listArray (0, n - 1) blockVars
        rArr :: Array Int (M.Map x (Compact Rational))
        rArr =
          listArray (0, n - 1)
            [ e j `vplus`
              vsum
                [ vscale (Real c) (solvedAt sol i)
                | (i, c) <- M.toList (deps j)
                , i `Set.notMember` compSet
                ]
            | j <- blockVars ]
        starB :: Matrix (Compact Rational)
        starB =
          starMatrix $
          fmap Real $
          matrixFromFunc n $ \(rj, ri) ->
            M.findWithDefault 0 (blockArr ! ri) (deps (blockArr ! rj))
    combine :: Maybe (M.Map x (Vec k))
    combine =
      traverse (fmap (M.filter (/= 0)) . traverse extractCompact) $
      M.unionsWith (M.unionWith (<+>)) $
      M.fromSet (const M.empty) targetSet :
      [ M.singleton t (M.singleton k (y <.> Real m))
      | (st, bst) <- bs
      , (t, y) <- M.toList (M.findWithDefault M.empty st yMap)
      , (k, m) <- M.toList bst
      ]
