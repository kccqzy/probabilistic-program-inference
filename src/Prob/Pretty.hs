{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Print inference/evaluation results in a slightly pretty way
module Prob.Pretty
  ( Mode(..)
  , Display(..)
  , handleProgPretty
  ) where

import Data.Bifunctor
import Data.Bits
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.Scientific
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word
import Prob.CoreAST
import Prob.Den (denProg)
import Prob.Eval (sampled)
import Prob.SurfaceAST (Ty(..), Var(..))

data Mode = ModeDen | ModeEval Int

-- | What the desugarer and the type checker know that the core program no
-- longer does: which variables are to be shown in which order and at which
-- type, and what the surface type of the @return@ expression was.
data Display = Display
  { dColumns :: [(T.Text, Ty)]
    -- ^ In name order. In ReturnAll mode these come from the declaration
    -- preamble (or, in legacy mode, from the variables that occur), so a
    -- variable that is declared but never used still gets a column showing its
    -- initial value.
  , dRetTy :: Maybe (NE.NonEmpty Ty)
  }

handleProgPretty :: forall r. Prog r Var -> Display -> Mode -> IO ShowS
handleProgPretty p disp m = formatResult <$> r
  where
    r :: IO [(ShowS, Rational)]
    r =
      case p of
        ReturnMult {} -> map (first (reassembleReturn (fromJust (dRetTy disp)))) <$> results p
        ReturnAll {} -> mergeRows . map (first project) <$> results p
      where
        -- Written to take the program back as an argument so that each branch
        -- above passes in a 'Prog' whose result type the GADT match has
        -- refined, which is where the 'Ord' instance comes from.
        results :: forall q. Ord q => Prog q Var -> IO [(q, Rational)]
        results q =
          case m of
            ModeDen -> pure (denProg q)
            ModeEval t -> sampled t q
    -- Drop the desugarer's temporaries, which are not user-visible, and add
    -- together the states that become equal once they are gone.
    project :: Sigma Var -> Sigma Var
    project = Set.filter (not . isTmp)
      where
        isTmp (TmpVar _) = True
        isTmp _ = False
    mergeRows :: [(Sigma Var, Rational)] -> [(ShowS, Rational)]
    mergeRows rows = map (first pprRow) (M.toList (M.fromListWith (+) rows))
    pprRow :: Sigma Var -> ShowS
    pprRow sigma =
      foldr (\c s -> cell c . s) (showString "│ ") (dColumns disp)
      where
        cell (name, TyBool) =
          shows name .
          showString " ->" .
          showString (if Set.member (BoolVar name) sigma then "  true " else " false ")
        cell (name, TyU8 _) =
          shows name .
          showString " ->" .
          showString
            (' ' :
             padLeft
               3
               (show (fromBits [Set.member (U8Var name i) sigma | i <- [0 .. 7]])) ++
             " ")
    formatResult :: [(ShowS, Rational)] -> ShowS
    formatResult [] = showString "No results produced.\n"
    formatResult rr =
      headerBar .
      foldr
        (\(col1, col2) s ->
           col1 . showString col2 . showChar '\n' . middleBar . s)
        id
        formattedRats .
        -- Once TODOs/TODO-diverge is implemented, we should show that instead.
      showChar '∑' . showString (replicate (maxLen1 - 1) ' ') . showString "│ 1\n" .
      footerBar
      where
        formattedRats :: [(ShowS, String)]
        formattedRats = map (second formatRational) (sortOn (Down . snd) rr)
        maxLen1 :: Int
        maxLen1 = case p of
          ReturnAll {} -> sum (map columnWidth (dColumns disp))
          ReturnMult {} ->
            case dRetTy disp of
              Nothing -> error "internal error: not possible to return Nothing here"
              Just tys -> sum (fmap tyLen tys) + 2 * (length tys - 1) + 1
            where
              tyLen TyBool = 5
              tyLen TyU8 {} = 3
        maxLen2 = maximum (map (length . snd) formattedRats)
        bar colsep =
          showString (replicate (maxLen1 - 1) '═') . showString colsep . showString (replicate maxLen2 '═') . showChar '\n'
        headerBar = bar "═╤═"
        middleBar = bar "═╪═"
        footerBar = bar "═╧═"

reassembleReturn :: NE.NonEmpty Ty -> NE.NonEmpty Bool -> ShowS
reassembleReturn tys bools =
  foldr (.) (showString " │ ") (intersperse (showString ", ") (cells (NE.toList tys) (NE.toList bools)))
  where
    cells [] _ = []
    cells (ty : tss) bs =
      let (bits, rest) = splitAt (tyBits ty) bs
      in cell ty bits : cells tss rest
    cell TyBool bits = showString (if or bits then " true" else "false")
    cell TyU8 {} bits = showString (padLeft 3 (show (fromBits bits)))
    tyBits TyBool = 1
    tyBits TyU8 {} = 8

-- | The width of one column of a ReturnAll table: the quoted name, the arrow,
-- and a right-aligned value field. A bool needs five characters for its value
-- (@false@), a u8 three (@255@).
columnWidth :: (T.Text, Ty) -> Int
columnWidth (name, ty) = length (show name) + 5 + valueWidth ty

valueWidth :: Ty -> Int
valueWidth TyBool = 5
valueWidth (TyU8 _) = 3

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- | Assemble bits, least significant first, into the integer they denote.
fromBits :: [Bool] -> Word8
fromBits bs = foldl' (\acc (i, b) -> if b then setBit acc i else acc) 0 (zip [0 ..] bs)

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
