{-# LANGUAGE GADTs #-}
module Main
  ( main
  ) where

import Data.Foldable
import Data.Semigroup ((<>))
import Options.Applicative
import Prob.Parse
import Prob.Pretty
import System.Exit

modeEval :: Parser Mode
modeEval = ModeEval <$> option auto (long "eval" <> help "Sample the execution of the program N times" <> metavar "N")

modeDen :: Parser Mode
modeDen = flag ModeDen ModeDen (long "infer" <> help "Perform inference of the program")

progs :: Parser [FilePath]
progs = some (argument str (metavar "FILES..."))

run :: (Mode, [FilePath]) -> IO ()
run (m, args) =
  forM_ args $ \f -> do
    mp <- doParseFromFile f
    case mp of
      Nothing -> exitWith (ExitFailure 1)
      Just (Prog p) -> handleProgPretty p m >>= putStr . ($ [])

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (allParsers <**> helper) (fullDesc <> progDesc "Perform inference or run a probabilistic program")
    allParsers = (,) <$> (modeDen <|> modeEval) <*> progs
