{-# LANGUAGE OverloadedStrings #-}
module Prob.Parse
  ( ParseErrors
  , Program(..)
  , parseAndTypeCheckProgram
  , processText
  , processFile
  ) where

import Control.Monad
import qualified Control.Monad.Combinators.NonEmpty as NEC
import Control.Monad.Combinators.Expr
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Data.Word
import Prob.SurfaceAST
import Prob.TypeCheck
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

type ParseErrors = ParseErrorBundle T.Text Void

-- | Produce a delayed error. A delayed error registers an error without
-- stopping the parse, so the user may see multiple errors.
delayedFail :: Int -> String -> Parser ()
delayedFail offset = registerParseError . FancyError offset . Set.singleton . ErrorFail

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces -- consume whitespace after lexemes

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaces

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semi :: Parser ()
semi = void (symbol ";")

comma :: Parser ()
comma = void (symbol ",")

keywords :: [T.Text]
keywords =
  [ "if", "then", "else", "while", "do", "skip", "true", "false"
  , "not", "and" , "or", "xor", "bernoulli", "uniform", "return", "observe"
  , "as", "bool", "u8"
  ]

keyword :: T.Text -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser T.Text
identifier = (lexeme . try) (p >>= check) <?> "identifier"
  where
    p = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x =
      if x `elem` keywords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

-- | An integer literal, range-checked to fit in a 'Word8'.
intLit :: Parser Word8
intLit = do
  o <- getOffset
  n <- lexeme L.decimal <?> "integer"
  if n <= 255
    then pure (fromInteger n)
    else delayedFail o ("u8 literal " ++ show n ++ " out of range 0..255") >> pure 0

--------------------------------------------------------------------------------
-- The declaration preamble
--------------------------------------------------------------------------------

-- | The colon of a declaration, which must not be the @:@ of a @:=@. The
-- 'notFollowedBy' has to run before the trailing whitespace is consumed,
-- hence the explicit 'lexeme' placement.
declColon :: Parser ()
declColon = void (lexeme (char ':' <* notFollowedBy (char '=')))

-- | A type annotation as the user writes it, in a declaration or a cast.
typeAnn :: Parser Ty
typeAnn =
  (TyBool <$ keyword "bool") <|>
  (keyword "u8" *> (u8 <$> optional (brackets overflowSem))) <?> "type"
  where
    -- Notice that for a type annotation, an omitted overflow semantics is
    -- resolved to never.
    u8 sem = TyU8 (Just (fromMaybe Saturate sem))
    overflowSem =
      (Wrap <$ keyword "wrap") <|> (Saturate <$ keyword "saturate") <|>
      (Never <$ keyword "never") <?>
      "overflow semantics (wrap, saturate or never)"

decl :: Parser [Decl]
decl = do
  -- The identifier list and the colon are what distinguishes a declaration
  -- from an assignment statement, so only they need to backtrack; anything
  -- after the colon can only be a type, and its errors are reported as such.
  names <- try (sepBy1 ((,) <$> getOffset <*> identifier) (symbol ",") <* declColon)
  t <- typeAnn
  semi
  pure [Decl o n t | (o, n) <- names]

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

ifStmt :: Parser [ParsedSStmt]
ifStmt = do
  keyword "if"
  cond  <- expr
  keyword "then"
  stmt1 <- braces stmt
  stmt2 <- option [] $ do
    keyword "else"
    ifStmt <|> braces stmt
  pure [SIf cond stmt1 stmt2]

whileStmt :: Parser [ParsedSStmt]
whileStmt = do
  keyword "while"
  offset <- getOffset
  cond <- expr
  keyword "do"
  body <- braces stmt
  pure [SWhile offset cond body]

doWhileStmt :: Parser [ParsedSStmt]
doWhileStmt = do
  keyword "do"
  body <- braces stmt
  keyword "while"
  offset <- getOffset
  cond <- expr
  pure [SDoWhile offset body cond]

assignStmt :: Parser [ParsedSStmt]
assignStmt = do
  offset <- getOffset
  var <- identifier
  choice
    [ do void (symbol ":=")
         val <- expr
         pure [SAssign offset var val]
    , do void (symbol "~")
         d <- dist
         pure [SSample offset var d]
    ]

skipStmt :: Parser [ParsedSStmt]
skipStmt = keyword "skip" >> pure []

observeStmt :: Parser [ParsedSStmt]
observeStmt = do
  offset <- getOffset
  keyword "observe"
  e <- expr
  pure [SObserve offset e]

stmt :: Parser [ParsedSStmt]
stmt = option [] (blockStmt <|> simpleStmt)
  where
    -- The difference between blockStmt and simpleStmt is just semicolon
    -- handling; after a blockStmt a semicolon is optional. Otherwise it is
    -- required.
    blockStmt = do
      item <- ifStmt <|> whileStmt <|> braces stmt
      rest <- optional semi *> stmt
      pure (item ++ rest)
    simpleStmt = do
      item <- doWhileStmt <|> skipStmt <|> observeStmt <|> assignStmt
      rest <- option [] (semi *> stmt)
      pure (item ++ rest)

dist :: Parser SDist
dist = bernoulliDist <|> uniformDist
  where
    bernoulliDist = do
      keyword "bernoulli"
      o <- getOffset
      r <- lexeme L.scientific
      if 0 <= r && r <= 1
        then pure (SBernoulli (toRational r))
        else delayedFail o ("bernoulli p " ++ show r ++ " cannot be outside [0, 1]") >> pure (SBernoulli 0)
    uniformDist = do
      keyword "uniform"
      o <- getOffset
      bounds <- optional ((,) <$> intLit <*> intLit)
      case bounds of
        Nothing -> pure (SUniform (0, 255))
        Just (lo, hi)
          | lo <= hi -> pure (SUniform (lo, hi))
          | otherwise -> do
            delayedFail o $
              "uniform " ++
              show lo ++ " " ++ show hi ++ " requires lower <= upper"
            pure (SUniform (0, 255))

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

expr :: Parser ParsedSExpr
expr = makeExprParser terms operators
  where
    operators =
      [ [Postfix parseCast]
      , [InfixL (addOffset SAdd (symbol "+")), InfixL (addOffset SSub (symbol "-"))]
      , [ InfixN (addOffset (`SCmp` CmpEq) (symbol "=="))
        , InfixN (addOffset (`SCmp` CmpNe) (symbol "!="))
        , InfixN (addOffset (`SCmp` CmpLe) (symbol "<="))
        , InfixN (addOffset (`SCmp` CmpLt) (symbol "<"))
        , InfixN (addOffset (`SCmp` CmpGe) (symbol ">="))
        , InfixN (addOffset (`SCmp` CmpGt) (symbol ">"))
        ]
      , [Prefix (addOffset SNot (keyword "not")), Prefix (addOffset SNot (symbol "!"))]
      , [InfixL (addOffset SAnd (keyword "and")), InfixL (addOffset SAnd (symbol "&&"))]
      , [InfixL (addOffset SXor (keyword "xor")), InfixL (addOffset SXor (symbol "^"))]
      , [InfixL (addOffset SOr (keyword "or")), InfixL (addOffset SOr (symbol "||"))]
      ]
      where
        addOffset f p = f <$> (getOffset <* p)
    parseCast :: Parser (ParsedSExpr -> ParsedSExpr)
    parseCast = do
      o <- getOffset <* keyword "as"
      ty <- typeAnn
      pure (\e -> SCast o e ty)
    terms =
      parens expr <|> (SBoolLit <$> getOffset <*> (True <$ keyword "true")) <|>
      (SBoolLit <$> getOffset <*> (False <$ keyword "false")) <|>
      (SIntLit <$> getOffset <*> intLit) <|>
      (SVar <$> getOffset <*> identifier)

--------------------------------------------------------------------------------

program :: Parser ParsedSProgram
program = do
  spaces
  ds <- many decl
  s <- stmt
  r <- optional (keyword "return" *> NEC.sepBy1 expr comma <* semi)
  pure
    SProgram
      { spDecls = concat ds
      , spStmts = s
      , spRet = r
      }

parseAndTypeCheckProgram :: FilePath -> T.Text -> Either ParseErrors Checked
parseAndTypeCheckProgram = parse ((program <* eof) >>= typeCheck)

--------------------------------------------------------------------------------
-- The front end as a whole
--------------------------------------------------------------------------------

-- | A program that has made it through the entire front end and is ready to be
-- desugared.
data Program = Program
  { pgSurface :: TyckedSProgram
  , pgRetTy :: Maybe (NE.NonEmpty Ty)
  }

-- | Parse and type check. Errors from either phase are rendered in the
-- megaparsec style, ready to be written to stderr.
processText :: FilePath -> T.Text -> Either String Program
processText name input = do
  checked <-
    case parseAndTypeCheckProgram name input of
      Left e -> Left (errorBundlePretty e)
      Right p -> Right p
  pure
    Program
      { pgSurface = ckProgram checked
      , pgRetTy = ckRetTy checked
      }

processFile :: FilePath -> IO (Either String Program)
processFile name = processText name <$> TIO.readFile name
