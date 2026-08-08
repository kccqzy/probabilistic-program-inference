-- | Print inference/evaluation results in a slightly pretty way
module Prob.Pretty
  ( Display(..)
  , prettyPrintResults
  ) where

import Data.Bifunctor
import Data.Bits
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Data.Ratio
import Data.Scientific
import qualified Data.Text as T
import Data.Word
import Prob.SurfaceAST (Ty(..))

-- | How to read the booleans a program returns: what the type checker knows
-- about them and the core program no longer does.
data Display
  = Columns [(T.Text, Ty)]
    -- ^ The program had no @return@ and so reports its variables: these, in
    -- name order. They come from the declaration preamble (or, in legacy mode,
    -- from the variables that occur), so a variable that is declared but never
    -- used still gets a column showing its initial value.
  | Returned (NE.NonEmpty Ty)
    -- ^ The surface types of the @return@ expressions.

prettyPrintResults :: [([Bool], Rational)] -> Display -> ShowS
prettyPrintResults results disp = formatResult . render $ results
  where
    render :: [([Bool], Rational)] -> [(ShowS, Rational)]
    render =
      case disp of
        Returned tys -> map (first (reassembleReturn (NE.toList tys)))
        Columns cols -> map (first (pprRow cols))
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
        maxLen1 = case disp of
          Columns cols -> sum (map columnWidth cols)
          Returned tys -> sum (fmap valueWidth tys) + 2 * (length tys - 1) + 1
        maxLen2 = maximum (map (length . snd) formattedRats)
        bar colsep =
          showString (replicate (maxLen1 - 1) '═') . showString colsep . showString (replicate maxLen2 '═') . showChar '\n'
        headerBar = bar "═╤═"
        middleBar = bar "═╪═"
        footerBar = bar "═╧═"

-- | One row of the table a @return@ produces: the returned values, in order.
reassembleReturn :: [Ty] -> [Bool] -> ShowS
reassembleReturn tys bools =
  foldr (.) (showString " │ ") (intersperse (showString ", ") cells)
  where
    cells = [showString (value ty bs) | (ty, bs) <- splitByTy tys bools]

-- | One row of the table a program with no @return@ produces: every variable
-- and the value it ended up with.
pprRow :: [(T.Text, Ty)] -> [Bool] -> ShowS
pprRow cols bits =
  foldr (\c s -> cell c . s) (showString "│ ") (zip (map fst cols) values)
  where
    values = splitByTy (map snd cols) bits
    cell (name, (ty, bs)) =
      shows name . showString " -> " . showString (value ty bs) . showChar ' '

-- | Cut a run of bits into values according to the given types.
splitByTy :: [Ty] -> [Bool] -> [(Ty, [Bool])]
splitByTy [] _ = []
splitByTy (ty:tys) bs = (ty, here) : splitByTy tys rest
  where
    (here, rest) = splitAt (if ty == TyBool then 1 else 8) bs

-- | A value, right-aligned in the field width its type gets.
value :: Ty -> [Bool] -> String
value TyBool bs = padLeft (valueWidth TyBool) (if or bs then "true" else "false")
value ty@TyU8 {} bs = padLeft (valueWidth ty) (show (fromBits bs))

-- | The width of one column of a 'Columns' table: the quoted name, the arrow,
-- and a right-aligned value field. A bool needs five characters for its value
-- (@false@), a u8 three (@255@). The bool case is exactly the fixed width the
-- table used before u8 existed, which is what keeps legacy output
-- byte-identical.
columnWidth :: (T.Text, Ty) -> Int
columnWidth (name, ty) = length (show name) + 5 + valueWidth ty

valueWidth :: Ty -> Int
valueWidth TyBool = 5
valueWidth (TyU8 _) = 3

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- | Assemble bits, most significant first, into the integer they denote.
fromBits :: [Bool] -> Word8
fromBits bs = foldl' (\acc (i, b) -> if b then setBit acc i else acc) 0 (zip [7, 6 .. 0] bs)

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
