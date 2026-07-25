{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Print inference/evaluation results in a slightly pretty way
module Prob.Pretty
  ( Mode(..)
  , handleProgPretty
  ) where

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Ord
import Data.Ratio
import Data.Scientific
import qualified Data.Set as Set
import Prob.CoreAST
import Prob.Den (denProg)
import Prob.Eval (sampled)

data Mode = ModeDen | ModeEval Int

handleProgPretty :: forall vt r. (Show vt, Ord vt) => Prog r vt -> Mode -> IO ShowS
handleProgPretty p m = formatResult <$> r
  where
    allVars :: Set.Set vt
    allVars =
      Set.fromList $ case p of Return s e -> concatMap toList s ++ toList e; ReturnAll s -> concatMap toList s
    r :: IO [(ShowS, Rational)]
    r =
      case p of
        Return {} -> map (first pprVar) <$> (case m of ModeDen -> pure (denProg p); ModeEval t -> sampled t p)
        ReturnAll {} -> map (first pprMap) <$> (case m of ModeDen -> pure (denProg p); ModeEval t -> sampled t p)
      where
        pprVar :: Bool -> ShowS
        pprVar True = showString " true  "
        pprVar False = showString "false  "
        pprMap :: Sigma vt -> ShowS
        pprMap sigma =
          foldr
            (\var s ->
               shows var .
               showString " ->" .
               showString
                 (if Set.member var sigma
                    then "  true "
                    else " false ") .
               s)
            (showString " ")
            allVars
    formatResult :: [(ShowS, Rational)] -> ShowS
    formatResult [] = showString "No results produced.\n"
    formatResult rr =
      bars .
      foldr
        (\(col1, col2) s -> col1 . showString col2 . showChar '\n' . s)
        id
        formattedRats
      where
        formattedRats :: [(ShowS, String)]
        formattedRats = map (second formatRational) (sortOn (Down . snd) rr)
        maxLen1 = case p of
          ReturnAll {} -> sum [ 10 + length (show v)| v <- Set.toList allVars ]
          Return {} -> 6
        maxLen2 = maximum (map (length . snd) formattedRats)
        bars =
          showString (replicate (maxLen1 - 1) '-') . showString "  " . showString (replicate maxLen2 '-') . showChar '\n'

formatRational :: Rational -> String
formatRational rat = ($ []) $
  if denominator rat >= 50
  then
    formatRationalExact . showString " (≈ " . formatRationalApprox . showChar ')'
  else
    formatRationalExact
  where
    formatRationalExact = shows (numerator rat) . showChar '/' . shows (denominator rat)
    formatRationalApprox =
      if approx == 0 then showChar '0'
      else showString (formatScientific (if approx < e then Exponent else Fixed) Nothing approx)
      where
        e = scientific 1 (-4)
        approx =
          case fromRationalRepetendLimited 8 rat of
            Left (r, _) -> r
            Right (r, _) -> r
