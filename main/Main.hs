{-# LANGUAGE GADTs #-}
module Main where

import Data.Foldable
import Prob.Den
import Prob.Parse
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \f -> do
    mp <- doParseFromFile f
    case mp of
      Nothing -> pure ()
      Just (Prog p) -> putStr (denProgPretty p [])
