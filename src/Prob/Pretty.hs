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
        Return {} -> map (first shows) <$> (case m of ModeDen -> pure (denProg p); ModeEval t -> sampled t p)
        ReturnAll {} -> map (first pprMap) <$> (case m of ModeDen -> pure (denProg p); ModeEval t -> sampled t p)
      where
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
        (\(col1, col2) s -> col1 . formatRational col2 . showChar '\n' . s)
        id
        (sortBy (comparing snd) rr)
      where
        formatRational rat = shows (numerator rat) . showChar '/' . shows (denominator rat)
        maxLen1 = sum [ 10 + length (show v)| v <- Set.toList allVars ]
        maxLen2 = maximum [ length (formatRational v []) | v <- map snd rr ]
        bars =
          showString (replicate (maxLen1 - 1) '-') . showString "  " . showString (replicate maxLen2 '-') . showChar '\n'
