{-# LANGUAGE BangPatterns #-}
module Main
  ( main
  ) where

import Control.Monad
import Data.Coerce
import Options.Applicative
import Prob.Alloc (allocIntProg)
import Prob.CoreAST (toIntProg, Prog)
import Prob.CoreOpt
import Prob.Den (InferStats(..), denProgStats)
import Prob.Eval (sampled)
import Prob.SurfaceAST (reportedVars)
import Prob.Desugar
import Prob.Parse
import Prob.Pretty
import System.Exit
import System.IO
import Text.Groom (groom)

newtype Optimize = Optimize Bool

newtype DumpCore = DumpCore Bool

newtype DumpInferStats = DumpInferStats Bool

data Mode = ModeDen | ModeEval Int

modeEval :: Parser Mode
modeEval = ModeEval <$> option auto (long "eval" <> help "Sample the execution of the program N times" <> metavar "N")

modeDen :: Parser Mode
modeDen = flag ModeDen ModeDen (long "infer" <> help "Perform inference of the program")

dumpCore :: Parser DumpCore
dumpCore = DumpCore <$> switch (long "dump-internal" <> help "Dump the internal representation of the program")

dumpInferStats :: Parser DumpInferStats
dumpInferStats = DumpInferStats <$> switch (long "dump-infer-stats" <> help "Report what inference cost, to stderr")

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

-- | This newtype is just to make the displayed result nicer.
newtype Var = Var Int deriving Show

toVarProg :: Prog Int -> Prog Var
toVarProg = coerce

run :: (Mode, Optimize, DumpCore, DumpInferStats, [FilePath]) -> IO ()
run (m, Optimize opt, DumpCore dc, DumpInferStats dis, args) =
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
            intProg = if opt
                      then allocIntProg (substituteProgram desugared)
                      else toIntProg desugared
        when dc $ hPutStrLn stderr (groom (toVarProg intProg))
        results <- case m of
          ModeDen -> do
            let (rs, stats) = denProgStats intProg
            -- Forcing the stats runs the whole inference, so this necessarily
            -- happens before any result is printed.
            when dis $ hPutStr stderr (formatInferStats f stats)
            pure rs
          ModeEval t -> sampled t intProg
        putStr (prettyPrintResults results display [])

-- | What the inference cost. 'isStatesPushed' is the one to watch: it is very
-- nearly proportional to the running time, and unlike the wall clock it is
-- exact and repeatable, which makes it the right thing to compare when
-- judging a change to 'Prob.Alloc'.
formatInferStats :: FilePath -> InferStats -> String
formatInferStats f !s =
  unlines $
  ("Infer stats for " ++ f ++ ":") :
  [ "  " ++ label ++ replicate (22 - length label) ' ' ++ show n
  | (label, n) <-
      [ ("States transformed", isStatesPushed s)
      , ("Statements run", isStmtsRun s)
      , ("Largest distribution", isLargestDistr s)
      , ("Loops solved", isKernelsSolved s)
      ]
  ]

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (allParsers <**> helper) (fullDesc <> progDesc "Perform inference or run a probabilistic program")
    allParsers = (,,,,) <$> (modeDen <|> modeEval) <*> optimize <*> dumpCore <*> dumpInferStats <*> progs
