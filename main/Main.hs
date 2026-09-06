{-# LANGUAGE BangPatterns #-}
module Main
  ( main
  ) where

import Control.Monad
import Data.Coerce
import qualified Data.IntMap as IM
import Data.List (intercalate)
import Options.Applicative
import Prob.CoreAST (Prog)
import Prob.Den (InferStats(..), InferLoopStats(..), InferParStats(..), denProgStats)
import Prob.Eval (sampled)
import Prob.Parse
import Prob.Pipeline (compileProgram, programDisplay)
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
        let display = programDisplay p
            intProg = compileProgram opt p
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

-- | What the inference cost. Two numbers are worth watching, and they have to
-- be watched together: 'isStatesPushed' is very nearly proportional to the time
-- spent running statements. Besides running statements, the other two major
-- cost centers are (1) solving of loops; (2) combination of the blocks of a Par
-- node. These are reported in a nested manner.
--
-- Every 'Par' node is reported under the loop whose body it splits.
formatInferStats :: FilePath -> InferStats -> String
formatInferStats f !s =
  unlines (("Infer stats for " ++ f ++ ":") : map render rows)
  where
    render (d, label, val)
      | null val = indent d ++ label ++ ":"
      | otherwise = let l = indent d ++ label in l ++ replicate (width - length l) ' ' ++ "  " ++ val
    indent d = replicate (2 + 2 * d) ' '
    width = maximum (0 : [length (indent d ++ l) | (d, l, v) <- rows, not (null v)])
    rows =
      [ (0, "States transformed", show (isStatesPushed s)),
        (0, "Statements run", show (isStmtsRun s)),
        (0, "Largest distribution", show (isLargestDistr s)),
        (0, "Par nodes", show (sum (map (IM.size . ilsPars) (IM.elems (isLoopStats s)))))
      ]
        ++ concatMap loopRows (IM.toList (isLoopStats s))
    loopRows (ln, ils) =
      (0, "Loop " ++ show ln, "")
        :  (1, "Footprint size", show (ilsFootprintSize ils))
        :  (1, "Carried set size", show (ilsCarriedSize ils))
        :  [(1, "Distinct kernels", show (ilsKernels ils)) | ilsKernels ils > 0]
        ++ [(1, "Deterministic iterations", show (ilsDeterministicSteps ils)) | ilsDeterministicSteps ils > 0]
        ++ concatMap parRows (IM.toList (ilsPars ils))
    parRows (pn, ips) =
      (1, "Par " ++ show pn, "")
        : [ (2, "Blocks", show (ipsBlocks ips)),
            (2, "Node runs", show (ipsRuns ips)),
            (2, "States in", show (ipsStatesIn ips)),
            (2, "States out", show (ipsStatesOut ips)),
            (2, "Block footprints", commas (ipsFootprints ips)),
            (2, "Block states solved", commas (ipsSolved ips)),
            (2, "Block fan-out", commas (ipsFanOut ips))
          ]
    commas = intercalate ", " . map show

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (allParsers <**> helper) (fullDesc <> progDesc "Perform inference or run a probabilistic program")
    allParsers = (,,,,) <$> (modeDen <|> modeEval) <*> optimize <*> dumpCore <*> dumpInferStats <*> progs
