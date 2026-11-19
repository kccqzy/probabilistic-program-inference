module Main
  ( main
  ) where

import Criterion.Main
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prob.Pipeline (inferText)
import System.FilePath ((</>))

-- | The examples worth benchmarking: those whose inference took at least 1 ms
-- in a criterion run in Nov 2026. The rest exist to test error messages or to
-- show one aspect of the language, and finish in microseconds.
benchmarked :: [FilePath]
benchmarked =
  [ "archery-three-lanes.txt"
  , "beacons-random-horizon.txt"
  , "dice-chain-add-last-big-expr.txt"
  , "dice-chain-add-last.txt"
  , "dice-chain-loop.txt"
  , "dice-chain.txt"
  , "dice-until-6.txt"
  , "loop-solve-system-3var-no-sol.txt"
  , "loop-solve-system-3var.txt"
  , "move-cars-large.txt"
  , "move-cars.txt"
  , "one-of-216.txt"
  , "queues-three-lanes.txt"
  , "quota-three-machines.txt"
  , "random-length-game.txt"
  , "repeat-die.txt"
  , "stress-test-8.txt"
  , "stress-test-8o.txt"
  , "u8-wrap-counter.txt"
  ]

-- | The benchmarked programs, read into memory up front so that no benchmark
-- measures I/O or depends on the state of the page cache.
loadExamples :: IO [(FilePath, T.Text)]
loadExamples = traverse load benchmarked
  where
    load n = do
      let path = "examples" </> n
      src <- TIO.readFile path
      pure (path, src)

main :: IO ()
main = do
  examples <- loadExamples
  -- Forcing the rendered table to normal form forces every probability in it,
  -- and therefore the whole inference.
  defaultMain
    [ bgroup "infer" [bench path (nf (inferText path) src) | (path, src) <- examples]
    ]
