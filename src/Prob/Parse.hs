{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Prob.Parse
  ( Expr
  , Stmt
  , Prog(..)
  , doParseFromFile
  , doParsePure
  ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import qualified Prob.CoreAST as Core
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

type Expr = Core.Expr T.Text
type Stmt = Core.Stmt T.Text
data Prog = forall r. Prog (Core.Prog r T.Text)

type Parser = Parsec Void T.Text

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

semi :: Parser ()
semi = void (symbol ";")

keywords :: [T.Text]
keywords = ["if", "then", "else", "while", "do", "skip", "true", "false", "not", "and", "or", "bernoulli", "return", "observe"]

keyword :: T.Text -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser T.Text
identifier = (lexeme . try) (p >>= check)
  where
    p = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x =
      if x `elem` keywords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

--------------------------------------------------------------------------------

ifStmt :: Parser [Stmt]
ifStmt = do
  keyword "if"
  cond  <- expr
  keyword "then"
  stmt1 <- braces stmt
  keyword "else"
  stmt2 <- ifStmt <|> braces stmt
  pure [Core.If cond (Core.Then stmt1) (Core.Else stmt2)]

whileStmt :: Parser [Stmt]
whileStmt = do
  keyword "while"
  cond <- expr
  keyword "do"
  body <- braces stmt
  pure [Core.While cond body]

assignStmt :: Parser [Stmt]
assignStmt = do
  var <- identifier
  deterministic <- (True <$ symbol ":=") <|> (False <$ symbol "~")
  if deterministic
    then do
      val <- expr
      pure [var Core.:= val]
    else do
      d <- dist
      pure [var Core.:~ d]

skipStmt :: Parser [Stmt]
skipStmt = keyword "skip" >> pure []

observeStmt :: Parser [Stmt]
observeStmt = do
  keyword "observe"
  e <- expr
  pure [Core.Observe e]

stmt :: Parser [Stmt]
stmt = concat <$> sepBy (ifStmt <|> whileStmt <|> skipStmt <|> observeStmt <|> assignStmt <|> braces stmt) semi

dist :: Parser Core.Dist
dist = keyword "bernoulli" >> Core.Bernoulli . toRational <$> lexeme L.scientific

expr :: Parser Expr
expr = makeExprParser terms operators
  where
    operators =
      [ [Prefix (Core.Not <$ keyword "not"), Prefix (Core.Not <$ symbol "!")]
      , [InfixL (Core.And <$ keyword "and"), InfixL (Core.And <$ symbol "&&")]
      , [InfixL (Core.Or <$ keyword "or"), InfixL (Core.Or <$ symbol "||")]
      ]
    terms =
      parens expr <|> (Core.Constant True <$ keyword "true") <|> (Core.Constant False <$ keyword "false") <|>
      Core.Var <$> identifier

prog :: Parser Prog
prog = do
  spaces
  s <- stmt
  r <- optional (keyword "return" >> expr)
  case r of
    Nothing -> pure (Prog (Core.ReturnAll s))
    Just e -> pure (Prog (Core.Return s e))

doParseFromFile :: FilePath -> IO (Maybe Prog)
doParseFromFile filename = do
  input <- TIO.readFile filename
  case parse (prog <* eof) filename input of
    Right x -> pure (Just x)
    Left e -> hPutStr stderr (parseErrorPretty' input e) >> pure Nothing

doParsePure :: T.Text -> Maybe Prog
doParsePure = parseMaybe prog
