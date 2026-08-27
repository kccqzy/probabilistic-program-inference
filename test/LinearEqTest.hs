{-# LANGUAGE ScopedTypeVariables #-}

-- | Compares the sparse SCC-ordered 'L.solveRows' against a dense oracle that
-- asterates the full (untransposed) coefficient matrix with the star-semiring
-- 'Prob.Matrix' module and reads off the target coordinates of every column's
-- solution. The oracle is the specification: @e_t^T (star A) b_k@ for every
-- requested target @t@.
module Main
  ( main
  ) where

import Control.Monad
import Data.Array
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set
import qualified Prob.LinearEq as L
import Prob.Matrix
import System.Exit
import Test.QuickCheck

-- | The specification of 'L.solveRows', computed the pre-SCC way: build the
-- full dense system @x = A x + b_k@ over all @n@ variables, asterate @A@ once,
-- and take each target's coordinate of each column's solution.
denseSolveRows ::
     Int -> L.Coeffs Int -> Set.Set Int -> [(Int, L.Vec Char)] -> Maybe (M.Map Int (L.Vec Char))
denseSolveRows n rows targets bs =
  traverse (fmap (M.filter (/= 0)) . traverse solveCol) targetCols
  where
    coeff :: M.Map (Int, Int) Rational
    coeff = M.fromListWith (+) [((i, j), c) | L.Row i tms <- rows, L.Term c j <- tms]
    columns :: M.Map Char (M.Map Int Rational)
    columns =
      M.fromListWith
        (M.unionWith (+))
        [(k, M.singleton st mass) | (st, bst) <- bs, (k, mass) <- M.toList bst]
    aStar :: Matrix (Compact Rational)
    aStar =
      starMatrix $
      fmap Real $
      matrixFromFunc n $ \(i, j) -> M.findWithDefault 0 (i, j) coeff
    targetCols :: M.Map Int (M.Map Char (Int, M.Map Int Rational))
    targetCols = M.fromList [(t, (,) t <$> columns) | t <- Set.toList targets]
    solveCol :: (Int, M.Map Int Rational) -> Maybe Rational
    solveCol (target, col) =
      let bvec = vectorFromFunc n $ \j -> Real (M.findWithDefault 0 j col)
          Vector arr = aStar `mult` bvec
       in extractCompact (arr ! target)

-- | A random system in the shape 'Prob.Den' produces: non-negative rows whose
-- coefficients sum to at most 1 (sometimes exactly 1, so that Inf actually
-- occurs), plus per-variable exit masses.
data Sys = Sys Int (L.Coeffs Int) (Set.Set Int) [(Int, L.Vec Char)]
  deriving Show

genRow :: Int -> Int -> Gen (L.Row Int)
genRow n i = do
  m <- chooseInt (0, 3)
  tms <-
    if m == 0
      then pure []
      else do
        succs <- vectorOf m (chooseInt (0, n - 1))
        ws <- vectorOf m (chooseInt (1, 4))
        p <- elements [1 % 2, 3 % 4, 1, 1]
        let tot = fromIntegral (sum ws)
        pure [L.Term (fromIntegral w * p / tot) j | (j, w) <- zip succs ws]
  -- Occasionally include an explicit zero coefficient: it must neither create
  -- a dependency edge nor change the answer.
  zeroTms <-
    frequency
      [(3, pure []), (1, (\j -> [L.Term 0 j]) <$> chooseInt (0, n - 1))]
  pure (L.Row i (tms ++ zeroTms))

genExit :: Gen (L.Vec Char)
genExit = do
  ks <- sublistOf "abc"
  ms <- forM ks $ \k -> (,) k . fromIntegral <$> chooseInt (0, 3)
  pure (M.fromList ms)

instance Arbitrary Sys where
  arbitrary = do
    n <- chooseInt (1, 6)
    rowVars <- sublistOf [0 .. n - 1]
    rows <- mapM (genRow n) rowVars
    targets <- Set.fromList <$> sublistOf [0 .. n - 1]
    exitVars <- sublistOf [0 .. n - 1]
    bs <- forM exitVars $ \st -> (,) st <$> genExit
    pure (Sys n rows targets bs)

prop_matchesDense :: Sys -> Property
prop_matchesDense (Sys n rows targets bs) =
  let dense = denseSolveRows n rows targets bs
   in classify (isNothing dense) "divergent (Nothing)" $
      L.solveRows rows targets bs === dense

main :: IO ()
main = do
  r <- quickCheckWithResult stdArgs {maxSuccess = 5000} prop_matchesDense
  unless (isSuccess r) exitFailure
