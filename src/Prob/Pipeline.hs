-- | The middle of the pipeline, between the front end ('Prob.Parse') and the
-- back ends ('Prob.Den', 'Prob.Eval'). It lives here so that the @prob@
-- executable and the benchmarks run exactly the same compilation.
module Prob.Pipeline
  ( compileProgram
  , programDisplay
  , inferText
  ) where

import qualified Data.Text as T
import Prob.Alloc (allocIntProg)
import Prob.CoreAST (Prog, toIntProg)
import Prob.CoreOpt (substituteProgram)
import Prob.Den (denProgStats)
import Prob.Desugar (desugarProgram)
import Prob.Parse (Program(..), processText)
import Prob.Pretty (Display(..), prettyPrintResults)
import Prob.SurfaceAST (reportedVars)

-- | Desugar and lower a type checked program to the integer-variable core,
-- optimizing it if asked to.
compileProgram :: Bool -> Program -> Prog Int
compileProgram opt p
  | opt = allocIntProg (substituteProgram desugared)
  | otherwise = toIntProg desugared
  where
    desugared = desugarProgram (pgSurface p)

-- | How the results of a program are laid out.
programDisplay :: Program -> Display
programDisplay p =
  case pgRetTy p of
    Just tys -> Returned tys
    -- When the program does not have a return the columns are the reported
    -- vars in name order. This is safe because @desugarProgram@ makes the same
    -- choice.
    Nothing -> Columns (reportedVars (pgSurface p))

-- | Everything @prob FILE@ does in its default mode, minus the I/O: parse,
-- type check, optimize, infer, and render the table it would print.
inferText :: FilePath -> T.Text -> Either String String
inferText name input = do
  p <- processText name input
  let (rs, _) = denProgStats (compileProgram True p)
  pure (prettyPrintResults rs (programDisplay p) [])
