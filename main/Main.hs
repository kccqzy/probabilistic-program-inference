module Main
  ( main
  ) where

import Control.Monad
import Options.Applicative
import Prob.CoreAST (toIntProg)
import Prob.CoreOpt
import Prob.Den (denProg)
import Prob.Eval (sampled)
import Prob.SurfaceAST
import Prob.Desugar
import Prob.Parse
import Prob.Pretty
import System.Exit
import System.IO
import Text.Groom (groom)

newtype Optimize = Optimize Bool

newtype DumpCore = DumpCore Bool

data Mode = ModeDen | ModeEval Int

modeEval :: Parser Mode
modeEval = ModeEval <$> option auto (long "eval" <> help "Sample the execution of the program N times" <> metavar "N")

modeDen :: Parser Mode
modeDen = flag ModeDen ModeDen (long "infer" <> help "Perform inference of the program")

dumpCore :: Parser DumpCore
dumpCore = DumpCore <$> switch (long "dump-internal" <> help "Dump the internal representation of the program")

optimize :: Parser Optimize
optimize = option (maybeReader p) (long "optimize" <> help "Whether to optimize the program" <> showDefaultWith s <> value (Optimize True) <> metavar "BOOL")
  where
    p "true" = Just (Optimize True)
    p "false" = Just (Optimize False)
    p _ = Nothing
    s (Optimize True) = "true"
    s (Optimize False) = "false"

progs :: Parser [FilePath]
progs = some (argument str (metavar "FILES..."))

run :: (Mode, Optimize, DumpCore, [FilePath]) -> IO ()
run (m, Optimize opt, DumpCore dc, args) =
  forM_ args $ \f -> do
    r <- processFile f
    case r of
      Left e -> hPutStr stderr e >> exitWith (ExitFailure 1)
      Right p -> do
        let display =
              case pgRetTy p of
                Just tys -> Returned tys
                -- When the program does not have a return the columns are the
                -- reported vars in name order. This is safe because
                -- @desugarProgram@ makes the same choice.
                Nothing -> Columns (reportedVars (pgSurface p))
        let desugared = desugarProgram (pgSurface p)
            optimized = if opt then optimizeProgram desugared else desugared
        when dc $ hPutStrLn stderr (groom optimized)
        let intProg = toIntProg optimized
        results <- case m of
          ModeDen -> pure (denProg intProg)
          ModeEval t -> sampled t intProg
        putStr (prettyPrintResults results display [])

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (allParsers <**> helper) (fullDesc <> progDesc "Perform inference or run a probabilistic program")
    allParsers = (,,,) <$> (modeDen <|> modeEval) <*> optimize <*> dumpCore <*> progs
