{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Lowering of the surface program into the all-boolean core the inference
-- engine understands. Every u8 becomes eight boolean bits; arithmetic becomes
-- ripple-carry expressions; uniform integer distributions become a decision
-- tree over bernoulli coins.
module Prob.Desugar
  ( desugarProgram,
  )
where

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict
import Data.Bits (shiftL, testBit)
import Data.Foldable
import Data.List (foldl1')
import Data.Ratio
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Word
import Prob.CoreAST
import Prob.SurfaceAST

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

-- | Bind an expression to a temporary and hand back a reference to it, so that
-- reading it many times costs one variable rather than one copy of the tree
-- each time. This is exempt if the original expression is a constant or a
-- variable.
share :: Expr Var -> D (Expr Var)
share e@(Constant _) = pure e
share e@(Var _) = pure e
share e = do
  t <- freshTmp
  emit [t := e]
  pure (Var t)

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
          compareBits op as bs >>= one
        SAdd t a b -> arith rippleAdd mkOr (semOfArith t) a b
        SSub t a b -> arith rippleSub (mkAnd . mkNot) (semOfArith t) a b
        SMul t a b -> arith rippleMul mkOr (semOfArith t) a b
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
    -- bit: addition and multiplication clamp to 255 (all bits set),
    -- subtraction clamps to 0.
    arith ripple clampWith sem a b = do
      as <- go a
      bs <- go b
      (rs, ovf) <- ripple as bs
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
          ovf' <- share ovf
          pure (map (clampWith ovf') rs)

-- | Lower a bool-typed expression to the single expression it denotes.
lowerBool :: TyckedSExpr -> D (Expr Var)
lowerBool e = onlyBit <$> lowerBits e

-- | The one bit of a bool's bit vector. Total on type-checked programs.
onlyBit :: [Expr Var] -> Expr Var
onlyBit [e] = e
onlyBit bs =
  error ("onlyBit: expected a bool, but got a " ++ show (length bs) ++ "-bit value")

-- | Compare two bit vectors of equal width.
compareBits :: CmpOp -> [Expr Var] -> [Expr Var] -> D (Expr Var)
compareBits op as bs = do
  case op of
    CmpEq -> pure eq
    CmpNe -> pure (mkNot eq)
    CmpLt -> lt as bs
    CmpGe -> mkNot <$> lt as bs
    CmpGt -> lt bs as
    CmpLe -> mkNot <$> lt bs as
  where
    eq = foldr1 mkAnd [mkNot (mkXor a b) | (a, b) <- zip as bs]
    lt xs ys = snd <$> rippleSub xs ys

--------------------------------------------------------------------------------
-- Ripple-carry arithmetic
--------------------------------------------------------------------------------

-- | Like 'scanl' from the standard library, but monadic.
scanlM :: Monad m => (a -> t -> m a) -> a -> [t] -> m [a]
scanlM f q ls =
  case ls of
    [] -> pure [q]
    x:xs -> do
      v <- f q x
      vs <- scanlM f v xs
      pure (q : vs)

rippleAddSub :: (Expr Var -> (Expr Var, Expr Var) -> Expr Var)
             -> [Expr Var]
             -> [Expr Var]
             -> D ([Expr Var], Expr Var)
rippleAddSub step as bs = do
  as' <- traverse share as
  bs' <- traverse share bs
  let ps = zip as' bs'
  cs <- scanlM (\c ab -> share $ step c ab) (Constant False) ps
  let carriesOrBorrows = init cs
  pure ([mkXor (mkXor a b) c | ((a, b), c) <- zip ps carriesOrBorrows], last cs)

-- | Addition: the sum bits and the carry out of the top bit (the overflow).
rippleAdd :: [Expr Var] -> [Expr Var] -> D ([Expr Var], Expr Var)
rippleAdd = rippleAddSub (\ c (a, b) -> mkOr (mkAnd a b) (mkAnd (mkXor a b) c))

-- | Subtraction in two's complement: the difference bits and the borrow out of
-- the top bit (the underflow).
rippleSub :: [Expr Var] -> [Expr Var] -> D ([Expr Var], Expr Var)
rippleSub = rippleAddSub (\ c (a, b) -> mkOr (mkAnd (mkNot a) b) (mkAnd (mkNot (mkXor a b)) c))

-- | Multiplication: the low eight bits of the product and the overflow bit.
--
-- The usual shift-and-add array, truncated to the lower 8 bits. Any higher bits
-- contribute to the overflow bit, which is very common here.
rippleMul :: [Expr Var] -> [Expr Var] -> D ([Expr Var], Expr Var)
rippleMul as bs = do
  as' <- traverse share as
  bs' <- traverse share bs
  foldM (step bs') (replicate 8 (Constant False), Constant False) (zip [0 ..] as')
  where
    step bs' (acc, discarded) (i, a) = do
      -- The partial product @a_i * b@ sits at positions i..i+7. Only the bits
      -- below position eight are added in. The rest are part of the overflow bit.
      let (fits, lost) = splitAt (8 - i) [mkAnd a b | b <- bs']
          (settled, live) = splitAt i acc
      (sums, carry) <- rippleAdd live fits
      sums' <- traverse share sums
      -- We do not share the overflow bit because it may well be unnecessary (in
      -- case of wrap).
      let ovf = foldl1' mkOr (carry : discarded : lost)
      pure (settled ++ sums', ovf)

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
      (guardStmts, e') <- capture (lowerBool e >>= share)
      emit guardStmts
      emit [While lbl e' (toList (desugarStmts body Seq.>< guardStmts))]
    SDoWhile lbl body e -> do
      (guardStmts, e') <- capture (lowerBool e >>= share)
      let desugaredBody = desugarStmts body
          stmts = desugaredBody Seq.>< guardStmts
      emit stmts
      emit [While lbl e' (toList stmts)]

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
          -- For arithmetic, in case the variable to be assigned is mentioned in
          -- the expression, it is more likely that we can avoid buffering bits
          -- if we assign from MSB to LSB. Consider the common increment
          -- expression (`x := x + 1`); if we assign from MSB to LSB then there
          -- is no need for any temporary variables. So we assign from MSB to
          -- LSB. We still have the detection in place in case temporaries are
          -- needed.
          irs' <- traverse buffer (zip [7, 6 .. 0] (reverse rs))
          emit $ Seq.fromList [U8Var x i := r | (i, r) <- irs', r /= Var (U8Var x i)]
  where
    buffer ir@(i, r)
      | any (readsTarget i) r = do
          t <- freshTmp
          emit [t := r]
          pure (i, Var t)
      | otherwise = pure ir
    -- When we assign bits, we assign from MSB to LSB. So when assigning bit i,
    -- we are allowed to read bits <= i. We just cannot read bits > i.
    readsTarget i (U8Var y j) = y == x && j > i
    readsTarget _ _ = False

-- | A uniform integer distribution, desugared entirely into bernoulli coins.
emitUniform :: T.Text -> (Word8, Word8) -> D ()
emitUniform x (0, hi) = emit (uniformFromZero x 7 (toInteger hi))
emitUniform x (lo, hi) = do
  emit (uniformFromZero x 7 (toInteger (hi - lo)))
  let ty = TyU8 (Just Wrap)
  emitAssign x (SAdd ty (SVar ty x) (SIntLit ty lo))

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

desugarProgram :: TyckedSProgram -> Prog Var
desugarProgram p =
  case spRet p of
    -- Without a @return@ the program reports its variables. No preludes
    -- needed.
    Nothing -> toList stmts `ReturnMult` concatMap varBits (reportedVars p)
    Just es ->
      let (retStmts, bits) = runD (traverse lowerBits es)
      in toList (stmts Seq.>< retStmts) `ReturnMult` concatMap reverse bits
  where
    stmts = desugarStmts (spStmts p)
    -- For a nice sort, the returned bits are MSB first. This matches what
    -- @fromBits@ does in Pretty.
    varBits (x, TyBool) = [Var (BoolVar x)]
    varBits (x, TyU8 _) = [Var (U8Var x i) | i <- [7, 6 .. 0]]
