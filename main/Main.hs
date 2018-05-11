{-# LANGUAGE GADTs #-}
module Main where

import Data.Bifunctor
import Data.Foldable
import Prob.CoreAST
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
      Just (Prog p) -> do
        let r :: [(String, Rational)]
            r = case p of
                  Return s e -> map (first show) (denProgReturn s e)
                  ReturnAll s -> map (first show) (denProgReturnAll s)
            maxLen1 = maximum (length . fst <$> r)
            maxLen2 = maximum (length . show . snd <$> r)
        putStr (replicate maxLen1 '-')
        putStr " "
        putStrLn (replicate maxLen2 '-')
        forM_ r $ \(c, v) -> do
          putStr c
          putStr (replicate (maxLen1 - length c + 1) ' ')
          print v
