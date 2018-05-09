{-# LANGUAGE OverloadedStrings #-}
module Prob.Examples where

import qualified Data.Map.Strict as M
import Prob.CoreAST

--------------------------------------------------------------------------------
-- Example Programs
--------------------------------------------------------------------------------
prog1 :: Prog Bool String
prog1 = ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5] `Return` "c1" `And` "c2"

prog2 :: Prog Bool String
prog2 = ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5, Observe ("c1" `Or` "c2")] `Return` "c1" `And` "c2"

prog1' :: Prog (M.Map String Bool) String
prog1' = ReturnAll ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5 ]

prog2' :: Prog (M.Map String Bool) String
prog2' = ReturnAll ["c1" :~ Bernoulli 0.5, "c2" :~ Bernoulli 0.5, Observe ("c1" `Or` "c2")]

progDice :: Prog (M.Map String Bool) String
progDice =
  ReturnAll
    [ "bit 0" :~ Bernoulli 0.5
    , "bit 1" :~ Bernoulli 0.5
    , "bit 2" :~ Bernoulli 0.5
  -- not all zeroes
    , Observe ("bit 0" `Or` "bit 1" `Or` "bit 2")
  -- not all ones
    , Observe (Not "bit 0" `Or` Not "bit 1" `Or` Not "bit 2")
    ]

progGeo :: Prog Bool String
progGeo = ["b" := Constant True, "p" := Constant False, While "b" ["b" :~ Bernoulli 0.5, "p" := Not "p"]] `Return` "p"

progGeo2 :: Prog (M.Map String Bool) String
progGeo2 =
  ReturnAll
    [ "b" := Constant True
    , "x0" := Constant True
    , "x1" := Constant False
    , "x2" := Constant False
    , While "b" ["b" :~ Bernoulli 0.5, next]
    ]
  where
    next =
      If
        "x0"
        (Then ["x0" := Constant False, "x1" := Constant True])
        (Else
           [ If
               "x1"
               (Then ["x1" := Constant False, "x2" := Constant True])
               (Else [If "x2" (Then ["x2" := Constant False, "x0" := Constant True]) (Else [])])
           ])

progGeo3 :: Prog (M.Map String Bool) String
progGeo3 =
  ReturnAll
    [ "b" := Constant True
    , "x0" := Constant True
    , "x1" := Constant False
    , "x2" := Constant False
    , While "b" ["b" :~ Bernoulli 0.5, next]
    ]
  where
    next =
      If
        "x0"
        (Then ["x0" := Constant False, "x1" := Constant True])
        (Else
           [ If
               "x1"
               (Then ["x1" := Constant False, "x2" := Constant True])
               (Else [If "x2" (Then ["x2" := Constant False, "x1" := Constant True]) (Else [])])
           ])

infini :: Prog (M.Map String Bool) String
infini = ReturnAll [While (Constant True) []]

progGeoFlipInside :: Prog Bool String
progGeoFlipInside = ["b" := Constant True, While "b" ["b" :~ Bernoulli 0.5, "p" :~ Bernoulli 0.5]] `Return` "p"
