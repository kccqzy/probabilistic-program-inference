{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Lowering of the surface program into the all-boolean core the inference
-- engine understands. Every u8 becomes eight boolean bits; arithmetic becomes
-- ripple-carry expressions; uniform integer distributions become a decision
-- tree over bernoulli coins.
module Prob.Desugar
  ( AnyProg (..),
    desugarProgram,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict
import Data.Bits (shiftL, testBit)
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Ratio
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Word
import Prob.CoreAST
import Prob.SurfaceAST

-- | A desugared program whose result type has been forgotten. Pattern
-- matching on the 'Prog' GADT recovers it.
data AnyProg = forall r. AnyProg (Prog r Var)

--------------------------------------------------------------------------------
-- Emission of core statements
--------------------------------------------------------------------------------

-- | The state of desugaring one surface statement: the next unused temporary
-- and the core statements emitted so far.
data Emit = Emit
  { emNext :: Int,
    emOut :: Seq.Seq (Stmt Var)
  }

-- | The monad for desugaring.
type D = State Emit

freshTmp :: D Var
freshTmp = state (\s -> (TmpVar (emNext s), s {emNext = emNext s + 1}))

emit :: Seq.Seq (Stmt Var) -> D ()
emit ss = modify (\s -> s {emOut = emOut s Seq.>< ss})

-- | Run a sub-computation and hand back what it emitted instead of leaving it
-- in the output. Used for a while guard, whose statements have to appear at
-- every point where the guard is evaluated.
capture :: D a -> D (Seq.Seq (Stmt Var), a)
capture d = do
  outer <- gets emOut
  modify' (\s -> s {emOut = []})
  a <- d
  inner <- gets emOut
  modify' (\s -> s {emOut = outer})
  pure (inner, a)

-- | Run the desugaring of a single surface statement. The temporary counter
-- starts at zero for every statement, so temporaries form a small pool that is
-- reused from statement to statement.
runD :: D a -> (Seq.Seq (Stmt Var), a)
runD d = (emOut s, a)
  where
    (a, s) = runState d (Emit 0 [])

runD_ :: D () -> Seq.Seq (Stmt Var)
runD_ = fst . runD

--------------------------------------------------------------------------------
-- Expression lowering
--------------------------------------------------------------------------------

-- | Lower an expression to its bit vector, least significant first: eight bits
-- for a u8 and one for a bool.
lowerBits :: TyckedSExpr -> D [Expr Var]
lowerBits = go
  where
    tyOf = sExprAnn
    semOfArith t =
      case t of
        TyBool -> error "internal error: no overflow semantics for bool"
        TyU8 Nothing -> error "internal error: typechecker should have folded constants"
        TyU8 (Just s) -> s
    go e0 =
      case e0 of
        SVar _ x ->
          case tyOf e0 of
            TyU8 {} -> pure [Var (U8Var x i) | i <- [0 .. 7]]
            _ -> pure [Var (BoolVar x)]
        SBoolLit _ b -> pure [Constant b]
        SIntLit _ k -> pure (constBits k)
        SNot _ e -> one . mkNot =<< bit e
        SAnd _ a b -> boolOp mkAnd a b
        SOr _ a b -> boolOp mkOr a b
        SXor _ a b -> boolOp mkXor a b
        SCmp _ op a b -> do
          as <- go a
          bs <- go b
          one (compareBits op as bs)
        SAdd t a b -> arith rippleAdd mkOr (semOfArith t) a b
        SSub t a b -> arith rippleSub (mkAnd . mkNot) (semOfArith t) a b
        SCast _ e ann ->
          case (ann, tyOf e) of
            (TyBool, TyU8 _) -> one . foldr1 mkOr =<< go e
            (TyBool, _) -> go e
            (TyU8 _, TyBool) -> do
              b <- bit e
              pure (b : replicate 7 (Constant False))
            (TyU8 _, TyU8 _) -> go e
    one e = pure [e]
    bit e = onlyBit <$> go e
    boolOp f a b = do
      x <- bit a
      y <- bit b
      one (f x y)
    -- @clampWith@ says what saturation does to a result bit given the overflow
    -- bit: addition clamps to 255 (all bits set), subtraction clamps to 0.
    arith ripple clampWith sem a b = do
      as <- go a
      bs <- go b
      let (rs, ovf) = ripple as bs
      case sem of
        Wrap -> pure rs
        Never -> do
          -- Rejecting the overflowing mass is conditioning: the reported
          -- distribution becomes conditional on no overflow having occurred.
          emit [Observe (mkNot ovf)]
          pure rs
        Saturate -> do
          -- The overflow bit goes into a temporary rather than being
          -- substituted into all eight result bits, which would multiply the
          -- expression size by eight.
          o <- freshTmp
          emit [o := ovf]
          pure (map (clampWith (Var o)) rs)

-- | Lower a bool-typed expression to the single expression it denotes.
lowerBool :: TyckedSExpr -> D (Expr Var)
lowerBool e = onlyBit <$> lowerBits e

-- | The one bit of a bool's bit vector. Total on type-checked programs.
onlyBit :: [Expr Var] -> Expr Var
onlyBit [e] = e
onlyBit bs =
  error ("onlyBit: expected a bool, but got a " ++ show (length bs) ++ "-bit value")

-- | Compare two bit vectors of equal width.
compareBits :: CmpOp -> [Expr Var] -> [Expr Var] -> Expr Var
compareBits op as bs =
  case op of
    CmpEq -> eq
    CmpNe -> mkNot eq
    CmpLt -> lt as bs
    CmpGe -> mkNot (lt as bs)
    CmpGt -> lt bs as
    CmpLe -> mkNot (lt bs as)
  where
    eq = foldr1 mkAnd [mkNot (mkXor a b) | (a, b) <- zip as bs]
    lt xs ys = snd (rippleSub xs ys)

--------------------------------------------------------------------------------
-- Ripple-carry arithmetic
--------------------------------------------------------------------------------

-- | Addition: the sum bits and the carry out of the top bit (the overflow).
rippleAdd :: [Expr Var] -> [Expr Var] -> ([Expr Var], Expr Var)
rippleAdd as bs = ([mkXor (mkXor a b) c | ((a, b), c) <- zip ps carries], last cs)
  where
    ps = zip as bs
    cs = scanl step (Constant False) ps
    carries = init cs
    step c (a, b) = mkOr (mkAnd a b) (mkAnd (mkXor a b) c)

-- | Subtraction in two's complement: the difference bits and the borrow out of
-- the top bit (the underflow).
rippleSub :: [Expr Var] -> [Expr Var] -> ([Expr Var], Expr Var)
rippleSub as bs = ([mkXor (mkXor a b) c | ((a, b), c) <- zip ps borrows], last cs)
  where
    ps = zip as bs
    cs = scanl step (Constant False) ps
    borrows = init cs
    step c (a, b) = mkOr (mkAnd (mkNot a) b) (mkAnd (mkNot (mkXor a b)) c)

--------------------------------------------------------------------------------
-- Statement lowering
--------------------------------------------------------------------------------

desugarStmts :: [TyckedSStmt] -> Seq.Seq (Stmt Var)
desugarStmts = foldMap desugarStmt

desugarStmt :: TyckedSStmt -> Seq.Seq (Stmt Var)
desugarStmt s0 = runD_ (emitStmt s0)

emitStmt :: TyckedSStmt -> D ()
emitStmt s0 =
  case s0 of
    SObserve _ e -> do
      e' <- lowerBool e
      emit [Observe e']
    SAssign _ x e -> emitAssign x e
    SSample _ x (SBernoulli p) -> emit [BoolVar x :~ Bernoulli p]
    SSample _ x (SUniform b) -> emitUniform x b
    SIf e a b -> do
      -- The guard's prelude (the conditioning of a `never` overflow in it)
      -- runs once, before the branch is chosen, and is independent of which
      -- branch would have been taken.
      e' <- lowerBool e
      emit [If e' (toList (desugarStmts a)) (toList (desugarStmts b))]
    SWhile lbl e body -> do
      -- A while guard is evaluated on entry and after every iteration, so its
      -- prelude has to appear at both of those points. A `never` overflow in a
      -- guard therefore conditions the program once per evaluation, and the
      -- rejections compound across iterations.
      (guardStmts, e') <- capture (lowerBool e)
      emit guardStmts
      emit [While lbl e' (toList (desugarStmts body Seq.>< guardStmts))]
    SDoWhile lbl body e -> do
      (guardStmts, e') <- capture (lowerBool e)
      let desugaredBody = desugarStmts body
          stmts = desugaredBody Seq.>< guardStmts
      emit stmts
      emit [While lbl e' (toList stmts)]

-- | Store a bit vector into a u8 variable.
storeBits :: T.Text -> [Expr Var] -> Seq.Seq (Stmt Var)
storeBits x rs = Seq.fromList [U8Var x i := r | (i, r) <- zip [0 ..] rs]

emitAssign :: T.Text -> TyckedSExpr -> D ()
emitAssign x e =
  case sExprAnn e of
    TyBool -> do
      e' <- lowerBool e
      emit [BoolVar x := e']
    TyU8 _
      | SVar _ y <- e,
        y == x ->
          pure () -- self-assignment: a no-op
      | otherwise -> do
          rs <- lowerBits e
          if mentions x e
            -- The result bits read the ORIGINAL bits of the operands, so when
            -- the target is itself an operand (`x := x + y`, `x := x + x`) they
            -- must all be computed before any of them is stored.
            then do
              ts <- replicateM 8 freshTmp
              emit $ Seq.fromList [t := r | (t, r) <- zip ts rs]
              emit $ Seq.fromList [U8Var x i := Var t | (i, t) <- zip [0 ..] ts]
            else emit (storeBits x rs)

-- | A uniform integer distribution, desugared entirely into bernoulli coins.
emitUniform :: T.Text -> (Word8, Word8) -> D ()
emitUniform x (0, hi) = emit (uniformFromZero x 7 (toInteger hi))
emitUniform x (lo, hi) = do
  emit (uniformFromZero x 7 (toInteger (hi - lo)))
  let (res, _) = rippleAdd (constBits lo) [Var (U8Var x i) | i <- [0 .. 7]]
  -- We know that we are adding a constant. Therefore every bit only depends on
  -- bits lower than this bit. Instead of creating 8 temporaries like in
  -- emitAssign, we can simply overwrite from the MSB to the LSB.
  emit . Seq.reverse . Seq.fromList $ [U8Var x i := r | (r, i) <- zip res [0 .. 7], r /= Var (U8Var x i)]

uniformFromZero :: T.Text -> Int -> Integer -> Seq.Seq (Stmt Var)
uniformFromZero x bit hi =
  if hi >= limit
    then
      let zeroCount = 1 `shiftL` bit -- how many numbers have this bit zero
          oneCount = hi + 1 - zeroCount -- how many numbers have this bit one
       in (U8Var x bit :~ Bernoulli (oneCount % (hi + 1)))
            Seq.<| ( if bit == 0
                       then []
                       else
                         genIf
                           (Var (U8Var x bit))
                           (uniformFromZero x (bit - 1) (hi - limit))
                           (uniformFromZero x (bit - 1) (limit - 1))
                   )
    else
      -- Even for high, this bit is 0. Therefore it is constant 0 for the entire
      -- range.
      (U8Var x bit := Constant False) Seq.<| (if bit == 0 then [] else uniformFromZero x (bit - 1) hi)
  where
    limit = 1 `shiftL` bit
    genIf cond whenTrue whenFalse
      | null whenTrue && null whenFalse = []
      | otherwise =
          -- We can optimize this as follows. For a reasonably large range, the
          -- last few bits will all be bernoulli 0.5. So we can hoist the shared
          -- suffix out of the conditional.
          case (whenTrue, whenFalse) of
            (tr Seq.:|> ttail, fr Seq.:|> ftail)
              | ttail == ftail -> genIf cond tr fr Seq.|> ttail
            _ -> [If cond (toList whenTrue) (toList whenFalse)]

constBits :: Word8 -> [Expr Var]
constBits v = [Constant (testBit v i) | i <- [0 .. 7]]

--------------------------------------------------------------------------------

desugarProgram :: TyckedSProgram -> AnyProg
desugarProgram p =
  case spRet p of
    Nothing -> AnyProg (ReturnAll (toList stmts))
    Just es ->
      let (retStmts, bits) = runD (traverse lowerBits es)
      in AnyProg (toList (stmts Seq.>< retStmts) `ReturnMult` NE.fromList (concat bits))
  where
    stmts = desugarStmts (spStmts p)
