module Main
  ( main
  ) where

import Control.Monad
import Options.Applicative
import Prob.SurfaceAST
import Prob.Desugar
import Prob.Parse
import Prob.Pretty
import System.Exit
import System.IO

modeEval :: Parser Mode
modeEval = ModeEval <$> option auto (long "eval" <> help "Sample the execution of the program N times" <> metavar "N")

modeDen :: Parser Mode
modeDen = flag ModeDen ModeDen (long "infer" <> help "Perform inference of the program")

progs :: Parser [FilePath]
progs = some (argument str (metavar "FILES..."))

run :: (Mode, [FilePath]) -> IO ()
run (m, args) =
  forM_ args $ \f -> do
    r <- processFile f
    case r of
      Left e -> hPutStr stderr e >> exitWith (ExitFailure 1)
      Right p ->
        let display =
              case pgRetTy p of
                Just tys -> Returned tys
                -- When the program does not have a return the columns are the
                -- reported vars in name order. This is safe because
                -- @desugarProgram@ makes the same choice.
                Nothing -> Columns (reportedVars (pgSurface p))
        in handleProgPretty (desugarProgram (pgSurface p)) display m >>= putStr . ($ [])

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (allParsers <**> helper) (fullDesc <> progDesc "Perform inference or run a probabilistic program")
    allParsers = (,) <$> (modeDen <|> modeEval) <*> progs
