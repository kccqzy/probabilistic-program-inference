{-# LANGUAGE GADTs #-}
module Main
  ( main
  ) where

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
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
      Right p -> do
        let display =
              Display
                { dColumns =
                  if M.null (pgEnv p)
                  then [(v, TyBool) | v <- Set.toAscList (programVars (pgSurface p))]
                  else M.toAscList (pgEnv p)
                , dRetTy = pgRetTy p
                }
        case desugarProgram (pgSurface p) of
          AnyProg dp -> handleProgPretty dp display m >>= putStr . ($ [])

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (allParsers <**> helper) (fullDesc <> progDesc "Perform inference or run a probabilistic program")
    allParsers = (,) <$> (modeDen <|> modeEval) <*> progs
