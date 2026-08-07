{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

-- | Name lookup and type checking of the surface program.
--
-- Besides accepting or rejecting the program, this pass folds all-constant
-- arithmetic away, which is what lets the rest of the compiler assume that
-- every surviving @+@ or @-@ has operands that determine its overflow
-- behavior.
--
-- Errors are reported as if they are parse errors.
module Prob.TypeCheck
  ( Checked(..)
  , typeCheck
  ) where

import Control.Monad
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Prob.SurfaceAST
import Text.Megaparsec.Error
import Text.Megaparsec (Parsec, registerParseError, Stream)

-- | The type of every variable of the program. In legacy mode this is
-- empty, which is interpreted to mean all variables are bool.
type Env = M.Map T.Text Ty

data Checked = Checked
  { ckProgram :: TyckedSProgram -- ^ With all-constant arithmetic folded away.
  , ckRetTy :: Maybe (NE.NonEmpty Ty) -- ^ The type of the @return@ expression, if any.
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- The checking monad
--------------------------------------------------------------------------------

-- | We wish to render errors in exactly the style megaparsec renders parse
-- errors; therefore, the type checker monad is actually the parser monad! We
-- don't use any feature of the Parser monad except for error reporting.
type Tc a = forall e s. (Ord e, Stream s) => Parsec e s a

err :: Int -> String -> Tc ()
err o = registerParseError . FancyError o . Set.singleton . ErrorFail

--------------------------------------------------------------------------------
-- The environment
--------------------------------------------------------------------------------

-- | Build the environment, rejecting duplicate declarations.
buildEnv :: SProgram a -> Tc Env
buildEnv p = foldM add M.empty (spDecls p)
  where
    add e d
      | M.member (dName d) e =
        err (dOffset d) ("duplicate declaration of variable " ++ show (dName d)) >>
        pure e
      | otherwise = pure (M.insert (dName d) (dAnn d) e)

--------------------------------------------------------------------------------
-- Expression checking
--------------------------------------------------------------------------------

lookupVar :: Env -> Int -> T.Text -> Tc (Maybe Ty)
lookupVar env o x =
  if M.null env
  then pure (Just TyBool)
  else case M.lookup x env of
         t@Just {} -> pure t
         Nothing -> err o ("undeclared variable " ++ show x) >> pure Nothing

-- | Require an expression to have a given type, reporting whether it did.
-- An expression that already failed to type is not complained about twice, and
-- reports 'False' so that its enclosing expression stays quiet as well.
expect :: Ty -> Int -> String -> Maybe Ty -> Tc Bool
expect _ _ _ Nothing = pure False
expect want o what (Just got) =
  case (want, got) of
    (TyBool, TyBool) -> pure True
    (TyU8 Nothing, TyU8 _) -> pure True
    (TyU8 _, TyBool) -> do
           err o $
             what ++
             " must be u8, but this expression has type " ++ prettyTy got
           pure False
    (TyU8 (Just s), TyU8 (Just s')) | s == s' -> pure True
    _ -> do
           err o $
             what ++
             " must be " ++
             prettyTy want ++
             ", but this expression has type " ++ prettyTy got
           pure False

-- | Check an expression, returning its type and the expression with
-- all-constant arithmetic folded away. Returns a tuple, containing the type for
-- the current expression and the type-checked expression. The type may be
-- @Nothing@, in which case the program is already ill-formed, but we merely
-- wish to continue type-checking in the interest of giving the user more
-- errors in a single run.
checkExpr :: Env -> ParsedSExpr -> Tc (Maybe Ty, TyckedSExpr)
checkExpr env = go
  where
    go :: ParsedSExpr -> Tc (Maybe Ty, TyckedSExpr)
    go e0 =
      case e0 of
        SVar o x -> do
          mty <- lookupVar env o x
          pure (mty, SVar (fromMaybe TyBool mty) x)
        SBoolLit _ b -> pure (Just TyBool, SBoolLit TyBool b)
        SIntLit _ i -> pure (Just (TyU8 Nothing), SIntLit (TyU8 Nothing) i)
        SNot _ e -> unBool "not" (SNot TyBool) e
        SAnd _ a b -> binBool "and" (SAnd TyBool) a b
        SOr _ a b -> binBool "or" (SOr TyBool) a b
        SXor _ a b -> binBool "xor" (SXor TyBool) a b
        SCmp o op a b -> compareOp o op a b
        SAdd o a b -> arith o "+" SAdd (\x y -> toInteger x + toInteger y) a b
        SSub o a b -> arith o "-" SSub (\x y -> toInteger x - toInteger y) a b
        SCast _ e ann -> do
          (_, e') <- go e
          pure (Just ann, SCast ann e' ann)
    -- Every boolean connective yields a bool, but only claim so when the
    -- operands checked out, so that one mistake deep in an expression produces
    -- one error rather than one per enclosing operator.
    unBool name rebuild e = do
      (t, e') <- go e
      ok <- boolOperand name e t
      pure (if ok then Just TyBool else Nothing, rebuild e')
    binBool name rebuild a b = do
      (ta, a') <- go a
      (tb, b') <- go b
      oka <- boolOperand name a ta
      okb <- boolOperand name b tb
      pure (if oka && okb then Just TyBool else Nothing, rebuild a' b')
    boolOperand name e =
      expect TyBool (sExprAnn e) ("the operand of " ++ name)
    compareOp o op a b = do
      (ta, a') <- go a
      (tb, b') <- go b
      let rebuilt = SCmp TyBool op a' b'
      let ordered = op `notElem` [CmpEq, CmpNe]
      case (ta, tb) of
        (Just TyBool, Just TyBool)
          | ordered -> do
            err o "the ordered comparisons (<, <=, >, >=) are not available on bools; only == and != are"
            pure (Nothing, rebuilt)
          | otherwise -> pure (Just TyBool, rebuilt)
        (Just x, Just y)
          -- Comparisons ignore the overflow behavior entirely: they perform no
          -- arithmetic, so they can't overflow.
          | toSuperType x == toSuperType y -> pure (Just TyBool, rebuilt)
          | otherwise -> do
            err o $
              "the operands of a comparison must have the same type, but the left has type " ++
              prettyTy x ++
              " and the right has type " ++
              prettyTy y ++
              "; consider adding a cast"
            pure (Nothing, rebuilt)
        _ -> pure (Nothing, rebuilt)
    -- Both operands must be u8 and must agree on their overflow behavior, so
    -- that there is never a question of whose rule applies.
    arith o name rebuild exactOp a b = do
      (ta, a') <- go a
      (tb, b') <- go b
      -- For reason of ergonomics, we first do type checking ignoring overflow
      -- semantics.
      oka <- expect (TyU8 Nothing) (sExprAnn a) ("the operand of " ++ name) ta
      okb <- expect (TyU8 Nothing) (sExprAnn b) ("the operand of " ++ name) tb
      let rebuilt s = rebuild (TyU8 s) a' b'
      case (oka && okb, ta, tb) of
        (True, Just (TyU8 sa), Just (TyU8 sb)) ->
          case unifySem sa sb of
            Just Nothing ->
              -- The only case both operands have the supertype u8[_] is
              -- when both are constants. Fold it.
              case (a', b') of
                (SIntLit _ ka, SIntLit _ kb) -> foldConst o name exactOp ka kb
                _ ->
                  error
                    "checkExpr: an unspecified operand that is not a literal"
            Just s -> pure (Just (TyU8 s), rebuilt s)
            Nothing -> do
              err o $
                      "mixed overflow behaviors in " ++
                      name ++
                      ": the left operand is " ++
                      prettyTy (TyU8 sa) ++
                      " and the right is " ++
                      prettyTy (TyU8 sb) ++
                      "; consider adding a cast: \"as " ++
                      prettyTy (TyU8 sb) ++
                      "\" on the left, or \"as " ++
                      prettyTy (TyU8 sa) ++
                      "\" on the right"
              pure (Nothing, rebuilt Nothing)
        _ -> pure (Nothing, rebuilt Nothing)
    -- An out-of-range constant is a static error rather than wrap or saturate or
    -- conditioning: nothing in the program said which of those was
    -- wanted, and quietly picking one would be a poor guess.
    foldConst o name exactOp ka kb
      | v < 0 || v > 255 = do
        err o $
          "constant arithmetic goes outside u8: " ++
          show ka ++ " " ++ name ++ " " ++ show kb ++ " = " ++ show v ++
          ", which is not in 0..255"
        pure (Nothing, SIntLit (TyU8 Nothing) 0)
      | otherwise = pure (Just (TyU8 Nothing), SIntLit (TyU8 Nothing) (fromInteger v))
      where
        v = exactOp ka kb

--------------------------------------------------------------------------------
-- Statement checking
--------------------------------------------------------------------------------

checkStmts :: Env -> Bool -> [ParsedSStmt] -> Tc [TyckedSStmt]
checkStmts env inLoop = mapM go
  where
    go (SAssign o x e) = do
      tx <- lookupVar env o x
      (t, e') <- checkExpr env e
      case tx of
        Nothing -> pure ()
        Just want -> void $ expect (toSuperType want) (sExprAnn e) ("the right-hand side of an assignment to the variable " ++ show x) t
      pure (SAssign o x e')
    go (SSample o x d) = do
      tx <- lookupVar env o x
      case (tx, d) of
        (Nothing, _) -> pure ()
        (Just TyBool, SBernoulli _) -> pure ()
        (Just (TyU8 _), SUniform _) -> pure ()
        (Just TyBool, SUniform _) ->
          err o $
          "the bool variable " ++
          show x ++ " cannot be sampled from a uniform integer distribution"
        (Just (TyU8 _), SBernoulli _) ->
          err o $
          "the u8 variable " ++ show x ++ " cannot be sampled from a bernoulli distribution"
      pure (SSample o x d)
    go (SObserve o e) = do
      (t, e') <- checkExpr env e
      void $ expect TyBool (sExprAnn e) "the expression of an observe" t
      pure (SObserve o e')
    go (SIf e s1 s2) = do
      (t, e') <- checkExpr env e
      void $ expect TyBool (sExprAnn e) "the condition of an if" t
      SIf e' <$> checkStmts env inLoop s1 <*> checkStmts env inLoop s2
    go (SWhile lbl e s) = do
      (t, e') <- checkExpr env e
      void $ expect TyBool (sExprAnn e) "the condition of a while" t
      SWhile lbl e' <$> checkStmts env True s
    go (SDoWhile lbl s e) = do
      body <- checkStmts env True s
      (t, e') <- checkExpr env e
      void $ expect TyBool (sExprAnn e) "the condition of a do-while" t
      pure (SDoWhile lbl body e')

--------------------------------------------------------------------------------

typeCheck :: ParsedSProgram -> Tc Checked
typeCheck p =
         do env <- buildEnv p
            stmts <- checkStmts env False (spStmts p)
            ret <- (traverse . traverse) (checkExpr env) (spRet p)
            pure
              Checked
                { ckProgram = p {spStmts = stmts, spRet = (fmap . fmap) snd ret}
                , ckRetTy = ret >>= traverse fst
                }
