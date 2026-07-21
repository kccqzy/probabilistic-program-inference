{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compares the sparse SCC-ordered 'L.solveRow' against a dense oracle that
-- asterates the full (untransposed) coefficient matrix with the star-semiring
-- 'Prob.Matrix' module and reads off the target coordinate of every column's
-- solution. The oracle is the specification: @e_target^T (star A) b_k@.
module Main
  ( main
  ) where

import Control.Monad
import Data.Array
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Ratio
import Data.Reflection
import qualified Prob.LinearEq as L
import Prob.Matrix
import System.Exit
import Test.QuickCheck

-- | A dense index for the oracle: positions @0 .. n-1@ with the size @n@
-- reified in the phantom @s@, so 'matrixFromFunc' can enumerate the range.
newtype I s = I Int deriving (Eq, Ord, Show)

instance Ix (I s) where
  range (I a, I b) = map I [a .. b]
  index (I a, _) (I i) = i - a
  inRange (I a, I b) (I i) = a <= i && i <= b

instance Reifies s Int => Bounded (I s) where
  minBound = I 0
  maxBound = I (reflect (Proxy :: Proxy s) - 1)

-- | The specification of 'L.solveRow', computed the pre-SCC way: build the full
-- dense system @x = A x + b_k@ over all @n@ variables, asterate @A@ once, and
-- take the @target@ coordinate of each column's solution.
denseSolveRow ::
     Int -> L.Coeffs Int -> Int -> [(Int, L.Vec Char)] -> Maybe (L.Vec Char)
denseSolveRow n rows target bs = reify n f
  where
    coeff :: M.Map (Int, Int) Rational
    coeff = M.fromListWith (+) [((i, j), c) | L.Row i tms <- rows, L.Term c j <- tms]
    columns :: M.Map Char (M.Map Int Rational)
    columns =
      M.fromListWith
        (M.unionWith (+))
        [(k, M.singleton st mass) | (st, bst) <- bs, (k, mass) <- M.toList bst]
    f :: forall s. Reifies s Int => Proxy s -> Maybe (L.Vec Char)
    f _ = fmap (M.filter (/= 0)) (traverse solveCol columns)
      where
        aStar :: Matrix (I s) (Compact Rational)
        aStar =
          star $
          fmap Real $
          matrixFromFunc $ \(I i, I j) -> M.findWithDefault 0 (i, j) coeff
        solveCol :: M.Map Int Rational -> Maybe Rational
        solveCol col =
          let bvec = vectorFromFunc $ \(I j) -> Real (M.findWithDefault 0 j col)
              Vector arr = aStar `mult` bvec
           in extractCompact (arr ! I target)

-- | A random system in the shape 'Prob.Den' produces: non-negative rows whose
-- coefficients sum to at most 1 (sometimes exactly 1, so that Inf actually
-- occurs), plus per-variable exit masses.
data Sys = Sys Int (L.Coeffs Int) Int [(Int, L.Vec Char)]
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
    target <- chooseInt (0, n - 1)
    exitVars <- sublistOf [0 .. n - 1]
    bs <- forM exitVars $ \st -> (,) st <$> genExit
    pure (Sys n rows target bs)

prop_matchesDense :: Sys -> Property
prop_matchesDense (Sys n rows target bs) =
  let dense = denseSolveRow n rows target bs
   in classify (isNothing dense) "divergent (Nothing)" $
      L.solveRow rows target bs === dense

main :: IO ()
main = do
  r <- quickCheckWithResult stdArgs {maxSuccess = 5000} prop_matchesDense
  unless (isSuccess r) exitFailure
