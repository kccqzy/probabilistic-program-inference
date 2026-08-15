{-# LANGUAGE StrictData #-}

-- | The surface syntax of the language: what the parser produces and what the
-- type checker and the slicer operate on. This is deliberately kept separate
-- from "Prob.CoreAST": the inference engine never sees any of it, because
-- "Prob.Desugar" lowers everything here into the all-boolean core.
module Prob.SurfaceAST
  ( Ty(..)
  , Var(..)
  , OverflowSem(..)
  , CmpOp(..)
  , Decl(..)
  , SExpr(..)
  , ParsedSExpr
  , TyckedSExpr
  , SDist(..)
  , SStmt(..)
  , ParsedSStmt
  , TyckedSStmt
  , SProgram(..)
  , ParsedSProgram
  , TyckedSProgram
  , sExprAnn
  , prettyTy
  , toSuperType
  , unifySem
  , reportedVars
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word

-- | The variables of a desugared (core) program supplied from the surface
-- syntax. Every one of them is a boolean.
data Var
  = BoolVar T.Text -- ^ A user @bool@ variable.
  | U8Var T.Text Int -- ^ Bit 0..7 of a user @u8@ variable; 0 is the LSB.
  | TmpVar Int -- ^ A boolean temporary introduced by desugaring.
  deriving (Eq, Ord, Show)

-- | What a u8 does when an arithmetic operation on it overflows (or, for
-- subtraction, underflows).
data OverflowSem
  = Wrap -- ^ Wrap around modulo 256.
  | Saturate -- ^ Clamp to 255 (addition) or 0 (subtraction).
  | Never -- ^ Condition on the overflow not having happened.
  deriving (Eq, Show)

prettySem :: OverflowSem -> String
prettySem Wrap = "wrap"
prettySem Saturate = "saturate"
prettySem Never = "never"

-- | The type checker's internal notion of a type. @u8[wrap]@, @u8[saturate]@
-- and @u8[never]@ are three distinct types.
--
-- @TyU8 Nothing@ is the hidden @u8[_]@, the supertype of the three. Only an
-- integer literal has it and it exists so that a literal fits on either side of
-- an operation without a cast.
data Ty
  = TyBool
  | TyU8 (Maybe OverflowSem)
  deriving (Eq, Show)

prettyTy :: Ty -> String
prettyTy TyBool = "bool"
prettyTy (TyU8 Nothing) = "u8[_]"
prettyTy (TyU8 (Just s)) = "u8[" ++ prettySem s ++ "]"

toSuperType :: Ty -> Ty
toSuperType TyBool = TyBool
toSuperType TyU8 {} = TyU8 Nothing

-- | The behavior an operation on these two operand flavors runs under, or
-- 'Nothing' if they disagree. An unspecified operand (a literal) takes the
-- other's; two unspecified operands leave the operation unspecified, which the
-- type checker resolves by folding it away.
unifySem :: Maybe OverflowSem -> Maybe OverflowSem -> Maybe (Maybe OverflowSem)
unifySem Nothing s = Just s
unifySem s Nothing = Just s
unifySem (Just a) (Just b)
  | a == b = Just (Just a)
  | otherwise = Nothing

data CmpOp
  = CmpEq
  | CmpNe
  | CmpLt
  | CmpLe
  | CmpGt
  | CmpGe
  deriving (Eq, Show)

-- | A single declaration in the preamble.
data Decl = Decl
  { dOffset :: Int
  , dName :: T.Text
  , dAnn :: Ty
  } deriving (Eq, Show)

-- | Surface expressions. Each expression has an "annotation" attached. When the
-- parser finishes, the annotation is @Int@ indicating the parse offset, for the
-- type checker to report any errors. When the type checker finishes, the
-- annotation is a @Ty@.
data SExpr a
  = SVar a T.Text
  | SBoolLit a Bool
  | SIntLit a Word8
  | SNot a (SExpr a)
  | SAnd a (SExpr a) (SExpr a)
  | SOr a (SExpr a) (SExpr a)
  | SXor a (SExpr a) (SExpr a)
  | SCmp a CmpOp (SExpr a) (SExpr a)
  | SAdd a (SExpr a) (SExpr a)
  | SSub a (SExpr a) (SExpr a)
  | SMul a (SExpr a) (SExpr a)
  | SCast a (SExpr a) Ty
  deriving (Eq, Show)

type ParsedSExpr = SExpr Int
type TyckedSExpr = SExpr Ty

sExprAnn :: SExpr a -> a
sExprAnn (SVar o _) = o
sExprAnn (SBoolLit o _) = o
sExprAnn (SIntLit o _) = o
sExprAnn (SNot o _) = o
sExprAnn (SAnd o _ _) = o
sExprAnn (SOr o _ _) = o
sExprAnn (SXor o _ _) = o
sExprAnn (SCmp o _ _ _) = o
sExprAnn (SAdd o _ _) = o
sExprAnn (SSub o _ _) = o
sExprAnn (SMul o _ _) = o
sExprAnn (SCast o _ _) = o

data SDist
  = SBernoulli Rational
  | SUniform (Word8, Word8)
  deriving (Eq, Show)

data SStmt a
  = SAssign Int T.Text (SExpr a)
  | SSample Int T.Text SDist
  | SObserve Int (SExpr a)
  | SIf (SExpr a) [SStmt a] [SStmt a]
  | SWhile Int (SExpr a) [SStmt a]
    -- ^ The 'Int' is the loop label.
  | SDoWhile Int [SStmt a] (SExpr a)
  deriving (Eq, Show)

type ParsedSStmt = SStmt Int
type TyckedSStmt = SStmt Ty

data SProgram a = SProgram
  { spDecls :: [Decl] -- ^ If empty, all variables are implicitly declared as bool.
  , spStmts :: [SStmt a]
  , spRet :: Maybe (NE.NonEmpty (SExpr a))
  } deriving (Eq, Show)

type ParsedSProgram = SProgram Int
type TyckedSProgram = SProgram Ty

-- | Every variable an expression reads.
exprVars :: SExpr a -> Set.Set T.Text
exprVars (SVar _ x) = Set.singleton x
exprVars (SBoolLit _ _) = Set.empty
exprVars (SIntLit _ _) = Set.empty
exprVars (SNot _ e) = exprVars e
exprVars (SAnd _ a b) = exprVars a `Set.union` exprVars b
exprVars (SOr _ a b) = exprVars a `Set.union` exprVars b
exprVars (SXor _ a b) = exprVars a `Set.union` exprVars b
exprVars (SCmp _ _ a b) = exprVars a `Set.union` exprVars b
exprVars (SAdd _ a b) = exprVars a `Set.union` exprVars b
exprVars (SSub _ a b) = exprVars a `Set.union` exprVars b
exprVars (SMul _ a b) = exprVars a `Set.union` exprVars b
exprVars (SCast _ e _) = exprVars e

-- | Every variable a statement touches, read or written.
stmtVars :: SStmt a -> Set.Set T.Text
stmtVars (SAssign _ x e) = Set.insert x (exprVars e)
stmtVars (SSample _ x _) = Set.singleton x
stmtVars (SObserve _ e) = exprVars e
stmtVars (SIf e s1 s2) =
  Set.unions (exprVars e : map stmtVars s1 ++ map stmtVars s2)
stmtVars (SWhile _ e s) = Set.unions (exprVars e : map stmtVars s)
stmtVars (SDoWhile _ s e) = Set.unions (exprVars e : map stmtVars s)

-- | What a program with no @return@ statement reports: every declared
-- variable and its declared type. In legacy programs, this is all variables
-- with an implicit bool type.
reportedVars :: SProgram a -> [(T.Text, Ty)]
reportedVars p
  | null (spDecls p) =
    [(x, TyBool) | x <- Set.toAscList (foldMap stmtVars (spStmts p))]
  | otherwise = M.toAscList (M.fromList [(dName d, dAnn d) | d <- spDecls p])
