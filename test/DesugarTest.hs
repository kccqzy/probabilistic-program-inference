{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end properties of u8 support: source text goes through the whole
-- front end and the inference engine, and the resulting distribution is
-- compared against the answer computed directly in Haskell.
--
-- These complement the golden examples, which pin down a handful of programs
-- exactly; here the programs themselves are generated.
module Main
  ( main
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.List (sort)
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import Data.Word
import Prob.CoreAST (Prog, toIntProg)
import Prob.Den (denProg)
import Prob.Desugar
import Prob.Eval (sampled)
import Prob.Parse
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Monadic

--------------------------------------------------------------------------------
-- Running a program written as source text
--------------------------------------------------------------------------------

compile :: T.Text -> Prog Int
compile src =
  case processText "<property>" src of
    Left e -> error ('\n' : e)
    Right p -> toIntProg (desugarProgram (pgSurface p))

-- | Infer a program that returns a u8. An empty result means every trace was
-- rejected, which is what @never@ does on overflow.
inferU8 :: T.Text -> [(Word8, Rational)]
inferU8 src = [(fromBits bs, q) | (bs, q) <- denProg (compile src)]

inferBool :: T.Text -> [(Bool, Rational)]
inferBool src = [(onlyBit bs, q) | (bs, q) <- denProg (compile src)]
  where
    onlyBit [b] = b
    onlyBit _ = error "inferBool: the program does not return a bool"

fromBits :: [Bool] -> Word8
fromBits bs = foldr (\(i, b) acc -> if b then setBit acc i else acc) 0 (zip [7, 6 .. 0] bs)

-- | Every distribution the tool reports is normalized, unless it is empty.
sumsToOne :: [(a, Rational)] -> Bool
sumsToOne [] = True
sumsToOne d = sum (map snd d) == 1

-- | The distribution that puts all its mass on one value.
certainly :: Word8 -> [(Word8, Rational)] -> Bool
certainly v d = d == [(v, 1)]

--------------------------------------------------------------------------------
-- Arithmetic under each overflow semantics
--------------------------------------------------------------------------------

data Op = Add | Sub deriving (Eq, Show)

instance Arbitrary Op where
  arbitrary = elements [Add, Sub]

opText :: Op -> T.Text
opText Add = "+"
opText Sub = "-"

data Sem = Wrap | Saturate | Never deriving (Eq, Show)

instance Arbitrary Sem where
  arbitrary = elements [Wrap, Saturate, Never]

semText :: Sem -> T.Text
semText Wrap = "[wrap]"
semText Saturate = "[saturate]"
semText Never = "[never]"

-- | Whether a program gets through the front end at all.
typeChecks :: T.Text -> Bool
typeChecks src =
  case processText "<property>" src of
    Left _ -> False
    Right _ -> True

-- | The three overflow behaviors are three distinct types: @+@ and @-@ demand
-- that their operands agree, and a cast on either operand is what resolves a
-- disagreement. Assignment, which does no arithmetic, never cares.
prop_semMustAgree :: Sem -> Sem -> Property
prop_semMustAgree s1 s2 =
  counterexample (T.unpack decls) $
  conjoin
    [ counterexample "a + b" $ typeChecks (stmt "a + b") === (s1 == s2)
    , counterexample "cast the right" $
      property (typeChecks (stmt (T.concat ["a + b as u8", semText s1])))
    , counterexample "cast the left" $
      property (typeChecks (stmt (T.concat ["a as u8", semText s2, " + b"])))
      -- A literal has no behavior of its own, so it fits either way round.
    , counterexample "literal operand" $ property (typeChecks (stmt "a + 1"))
      -- Assignment copies bits; the behaviors need not match.
    , counterexample "assignment" $ property (typeChecks (stmt "b"))
      -- Nor does a comparison care.
    , counterexample "comparison" $
      property (typeChecks (T.concat [decls, "c : bool;\nc := a < b;\n"]))
      -- Two literals never disagree: the operation is folded away.
    , counterexample "two literals" $ property (typeChecks (stmt "1 + 2"))
    ]
  where
    decls =
      T.concat
        ["a : u8", semText s1, ";\nb : u8", semText s2, ";\nr : u8;\n"]
    stmt rhs = T.concat [decls, "r := ", rhs, ";\n"]

--------------------------------------------------------------------------------
-- Comparisons on bools
--------------------------------------------------------------------------------

-- | On bools, == and != are the one-bit case of the same operation. The
-- ordered comparisons are u8-only and must be rejected.
prop_compareBool :: Bool -> Bool -> Bool -> Property
prop_compareBool useEq a b =
  counterexample (T.unpack src) $
  conjoin
    [ inferBool src === [(cmpFunc c (toW a) (toW b), 1)]
    , counterexample "ordered comparisons must be rejected on bools" $
      property (not (any (typeChecks . ordered) ["<", "<=", ">", ">="]))
    ]
  where
    c = if useEq then Eq else Ne
    toW x = if x then 1 else 0 :: Word8
    lit x = if x then "true" else "false" :: T.Text
    src = T.concat ["return ", lit a, " ", cmpText c, " ", lit b, ";\n"]
    ordered o = T.concat ["return ", lit a, " ", o, " ", lit b, ";\n"]

-- | Arithmetic on two constants is evaluated by the compiler; a result outside
-- 0..255 is a static error rather than a wrap or a conditioning.
prop_constantFolding :: Op -> Word8 -> Word8 -> Property
prop_constantFolding op a b =
  counterexample (T.unpack src) $
  case exact of
    Just v -> property (certainly v (inferU8 src))
    Nothing -> property (not (typeChecks src))
  where
    exact
      | v < 0 || v > 255 = Nothing
      | otherwise = Just (fromInteger v)
      where
        v =
          if op == Add
            then toInteger a + toInteger b
            else toInteger a - toInteger b
    -- Folded inside a larger expression, so that what is checked is the fold
    -- and not merely a literal being returned.
    src =
      T.concat
        ["return (", num a, " ", opText op, " ", num b, ") as u8[wrap];\n"]

num :: Word8 -> T.Text
num = T.pack . show

-- | What @a op b@ should come out as, or 'Nothing' when the whole trace is
-- conditioned away.
expected :: Sem -> Op -> Word8 -> Word8 -> Maybe Word8
expected sem op a b =
  case sem of
    Wrap -> Just wrapped
    Saturate
      | overflowed -> Just (if op == Add then 255 else 0)
      | otherwise -> Just wrapped
    Never
      | overflowed -> Nothing
      | otherwise -> Just wrapped
  where
    wrapped = if op == Add then a + b else a - b
    overflowed =
      if op == Add
        then toInteger a + toInteger b > 255
        else toInteger a < toInteger b

-- | The operands are variables of the flavor under test, not literals: an
-- operation on two literals is folded by the compiler and never reaches the
-- desugarer at all (see 'prop_constantFolding').
prop_arith :: Sem -> Op -> Word8 -> Word8 -> Property
prop_arith sem op a b =
  counterexample (T.unpack src) $
  case expected sem op a b of
    Nothing -> result === []
    Just v -> property (certainly v result)
  where
    src =
      T.concat
        [ "p, q, x : u8", semText sem, ";\n"
        , "p := ", num a, ";\nq := ", num b, ";\n"
        , "x := p ", opText op, " q;\n"
        , "return x;\n"
        ]
    result = inferU8 src

-- | Arithmetic reads a variable operand exactly as it reads a literal, and an
-- assignment whose target is also an operand must see the target's original
-- value rather than a half-written one.
prop_arithAliased :: Op -> Word8 -> Word8 -> Property
prop_arithAliased op a b =
  counterexample (T.unpack src) $ certainly v (inferU8 src)
  where
    v = if op == Add then a + b else a - b
    src =
      T.concat
        [ "x, y : u8[wrap];\n"
        , "x := ", num a, ";\n"
        , "y := ", num b, ";\n"
        , "x := x ", opText op, " y;\n"
        , "return x;\n"
        ]

-- | Subtracting back what was added returns the original value, under wrapping
-- arithmetic, for every pair of operands. Written with an equality observe, so
-- that a wrong answer shows up as a rejected trace (an empty result) rather
-- than as a differing value.
prop_addSubRoundTrip :: Word8 -> Word8 -> Property
prop_addSubRoundTrip a b =
  counterexample (T.unpack src) $ certainly a (inferU8 src)
  where
    src =
      T.concat
        [ "t, d : u8[wrap];\n"
        , "t := ", num a, ";\nd := ", num b, ";\n"
        , "t := t + d;\n"
        , "t := t - d;\n"
        , "observe t == ", num a, ";\n"
        , "return t;\n"
        ]

-- | A chain of operations applies the target's semantics at every step, so a
-- wrapping chain is just the arithmetic done modulo 256 throughout.
prop_arithChain :: Word8 -> Word8 -> Word8 -> Property
prop_arithChain a b c =
  counterexample (T.unpack src) $ certainly (a + b - c) (inferU8 src)
  where
    src =
      T.concat
        [ "x, p, q : u8[wrap];\n"
        , "p := ", num b, ";\nq := ", num c, ";\n"
        , "x := ", num a, " + p - q;\n"
        , "return x;\n"
        ]

--------------------------------------------------------------------------------
-- Comparisons and casts
--------------------------------------------------------------------------------

data Cmp = Eq | Ne | Lt | Le | Gt | Ge deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Cmp where
  arbitrary = elements [minBound .. maxBound]

cmpText :: Cmp -> T.Text
cmpText Eq = "=="
cmpText Ne = "!="
cmpText Lt = "<"
cmpText Le = "<="
cmpText Gt = ">"
cmpText Ge = ">="

cmpFunc :: Cmp -> Word8 -> Word8 -> Bool
cmpFunc Eq = (==)
cmpFunc Ne = (/=)
cmpFunc Lt = (<)
cmpFunc Le = (<=)
cmpFunc Gt = (>)
cmpFunc Ge = (>=)

prop_compare :: Cmp -> Word8 -> Word8 -> Property
prop_compare c a b =
  counterexample (T.unpack src) $
  inferBool src === [(cmpFunc c a b, 1)]
  where
    src =
      T.concat
        ["return ", num a, " ", cmpText c, " ", num b, ";\n"]

-- | A u8 is true exactly when it is nonzero, and a bool becomes 1 or 0.
prop_casts :: Word8 -> Property
prop_casts a =
  counterexample (T.unpack src) $
  conjoin
    [ inferBool (T.concat ["return ", num a, " as bool;\n"]) === [(a /= 0, 1)]
    , property (certainly (if a /= 0 then 1 else 0) (inferU8 src))
    ]
  where
    src = T.concat ["return (", num a, " as bool) as u8;\n"]

--------------------------------------------------------------------------------
-- Uniform distributions
--------------------------------------------------------------------------------

-- | A bounded uniform is exactly flat over its range, with no mass anywhere
-- else and none rejected.
prop_uniform :: Word8 -> Word8 -> Property
prop_uniform lo hi' =
  counterexample (T.unpack src) $
  conjoin
    [ property (sumsToOne result)
    , map fst result === [lo .. hi]
    , property (all ((== 1 % fromIntegral m) . snd) result)
    ]
  where
    hi = max lo hi'
    m = fromIntegral hi - fromIntegral lo + 1 :: Integer
    src = T.concat ["x : u8;\nx ~ uniform ", num lo, " ", num hi, ";\nreturn x;\n"]
    result = sort (inferU8 src)

-- | A uniform drawn inside a probabilistic loop must not disturb the
-- distribution over iteration counts. Rejection-based sampling would multiply
-- each trace's weight by the acceptance probability once per iteration and
-- skew this towards short traces.
prop_uniformInLoop :: Word8 -> Property
prop_uniformInLoop hi' =
  counterexample (T.unpack src) $
  conjoin
    [ property (sumsToOne result)
      -- The loop runs k times with probability 2^-k, and each of those traces
      -- ends with n = k. n stops at 8 because of the guard, so the last row
      -- collects the whole tail.
    , [q | (v, q) <- result, v < 8] === [1 % 2 ^ k | k <- [1 .. 7 :: Integer]]
    ]
  where
    hi = min 3 hi'
    src =
      T.concat
        [ "n, x : u8[wrap];\ncoin : bool;\n"
        , "do {\n"
        , "  coin ~ bernoulli 0.5;\n"
        , "  x ~ uniform 0 ", num hi, ";\n"
        , "  n := n + 1;\n"
        , "} while coin and n < 8;\n"
        , "return n;\n"
        ]
    result = sort (inferU8 src)

--------------------------------------------------------------------------------
-- Sampling agrees with inference
--------------------------------------------------------------------------------

-- | Eval mode should reproduce inference mode, within sampling error. The
-- program is the wrapping counter of examples/u8-wrap-counter.txt, whose exact
-- answer is P(i = k) = 2^-k.
prop_evalAgreesWithInfer :: Property
prop_evalAgreesWithInfer =
  once $
  monadicIO $ do
    d <- run sampleIt
    forM_ [1 .. 4 :: Int] $ \k -> do
      let got = fromMaybe 0 (lookup (fromIntegral k) d)
          want = 1 % (2 ^ k)
          -- Four standard deviations of a proportion at this sample size is
          -- under 0.015, so this is loose enough not to be flaky and tight
          -- enough to catch a genuinely wrong distribution.
          off = abs (fromRational (got - want)) :: Double
      unless (off < 0.02) $
        fail ("P(i = " ++ show k ++ ") = " ++ show (fromRational got :: Double))
  where
    sampleIt :: IO [(Word8, Rational)]
    sampleIt = map (first fromBits) <$> sampled trials (compile src)
    trials = 20000 :: Int
    src =
      T.concat
        [ "i : u8[wrap];\ncoin : bool;\n"
        , "do {\n  coin ~ bernoulli 0.5;\n  i := i + 1;\n} while coin;\n"
        , "return i;\n"
        ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  rs <-
    sequence
      [ run' 2000 "arithmetic under each overflow semantics" prop_arith
      , run' 500 "arithmetic with an aliased target" prop_arithAliased
      , run' 500 "(a + b) - b == a when wrapping" prop_addSubRoundTrip
      , run' 300 "a chain of operations" prop_arithChain
      , run' 1000 "comparisons" prop_compare
      , run' 100 "comparisons on bools" prop_compareBool
      , run' 500 "constant arithmetic is folded, or a static error" prop_constantFolding
      , run' 100 "the overflow behaviors are distinct types" prop_semMustAgree
      , run' 300 "casts between bool and u8" prop_casts
      , run' 100 "bounded uniform is flat" prop_uniform
      , run' 20 "uniform inside a loop does not skew it" prop_uniformInLoop
      , run' 1 "sampling agrees with inference" prop_evalAgreesWithInfer
      ]
  unless (and rs) exitFailure
  where
    run' n name p = do
      putStrLn ("=== " ++ name)
      isSuccess <$> quickCheckWithResult stdArgs {maxSuccess = n} p
