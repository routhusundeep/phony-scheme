module PSParser
  (
    readExpr,
    readExprList,
  LispVal(..)
  ) where

import           Control.Monad
import           Control.Monad.Except
import           PSTypes
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  first <- endBy parseExpr space
  rest <- char '.' >> spaces >> parseExpr
  return $ DottedList first rest

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do _ <- char '('
         x <- try parseList <|> parseDottedList
         _ <- char ')'
         return x

parseExprList :: Parser [LispVal]
parseExprList = endBy parseExpr spaces

readOrThrow :: (Parser a) -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow parseExprList

