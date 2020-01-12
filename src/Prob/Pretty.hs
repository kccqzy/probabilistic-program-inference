{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Print inference/evaluation results in a slightly pretty way
module Prob.Pretty
  ( Mode(..)
  , handleProgPretty
  ) where

import Data.Bifunctor
import qualified Data.Map.Strict as M
import Data.Ratio
import Prob.CoreAST
import Prob.Den (denProg)
import Prob.Eval (sampled)

data Mode = ModeDen | ModeEval Int

handleProgPretty :: forall vt r. (Show vt, Ord vt) => Prog r vt -> Mode -> IO ShowS
handleProgPretty p m = formatResult <$> r
  where
    r :: IO [(String, String)]
    r =
      case p of
        Return {} -> map (bimap (`shows` " ") (($ []) . formatRational)) <$> (case m of ModeDen -> pure (denProg p); ModeEval t -> sampled t p)
        ReturnAll {} -> map (bimap pprMap (($ []) . formatRational)) <$> (case m of ModeDen -> pure (denProg p); ModeEval t -> sampled t p)
      where
        pprMap :: Sigma vt -> String
        pprMap =
          M.foldrWithKey
            (\var val s ->
               shows var .
               showString " ->" .
               showString
                 (if val
                    then "  true "
                    else " false ") $
               s)
            " "
        formatRational rat = shows (numerator rat) . showChar '/' . shows (denominator rat)
    formatResult :: [(String, String)] -> ShowS
    formatResult [] = showString "No results produced.\n"
    formatResult rr =
      bars .
      foldr
        (\(col1, col2) s ->
           showString col1 . showString (replicate (maxLen1 - length col1) ' ') . showString col2 . showChar '\n' . s)
        id
        rr
      where
        maxLen1 = maximum (length . fst <$> rr)
        maxLen2 = maximum (length . snd <$> rr)
        bars =
          showString (replicate (maxLen1 - 1) '-') . showChar ' ' . showString (replicate maxLen2 '-') . showChar '\n'
