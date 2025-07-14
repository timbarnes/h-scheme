{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Scheme.Parser
Description: Lexical analysis and parsing for Scheme expressions
Purpose: This module handles the conversion of Scheme source code (as strings)
         into the internal Value representation. It includes tokenization,
         parsing of expressions, and handling of quoted expressions.
         The parser uses Parsec for robust error handling and supports
         multi-line input through the REPL.

Exports:
  - parse: Parse a Scheme expression from a string
  - tokenize: Convert a string into tokens (for debugging)
  - schemeFile: Parse a Scheme file with multiple top-level forms
  - parseMany: Parse multiple Scheme expressions from a string
-}

module Scheme.Parser
  ( parse
  , schemeFile
  , parseMany
  ) where

import Scheme.Core
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Monad (void)

-- | Inter-token space: whitespace or comments
interTokenSpace :: Parser ()
interTokenSpace = skipMany (skipMany1 space <|> comment)

-- | Parse and skip a comment
comment :: Parser ()
comment = do
  _ <- char ';'
  _ <- manyTill anyChar (void endOfLine <|> eof)
  return ()

-- | Lexeme combinator: parses p and skips trailing inter-token space
lexeme :: Parser a -> Parser a
lexeme p = p <* interTokenSpace

-- | Parse a Scheme expression from a string (now parses all forms and returns the last)
parse :: String -> Either SchemeError Value
parse input =
  case runParser schemeFile () "" input of
    Left err -> Left $ ParseError $ show err
    Right [] -> Right Nil
    Right exprs -> Right (last exprs)

-- | Parse a Scheme file with multiple top-level forms
schemeFile :: Parser [Value]
schemeFile = interTokenSpace *> sepEndBy schemeExpression interTokenSpace <* eof

-- | Parse multiple Scheme expressions from a string
parseMany :: String -> Either SchemeError [Value]
parseMany input =
  case runParser schemeFile () "" input of
    Left err -> Left $ ParseError $ show err
    Right exprs -> Right exprs

-- | Parse a Scheme expression
schemeExpression :: Parser Value
schemeExpression = choice
  [ lexeme number
  , lexeme stringLiteral
  , lexeme quoted
  , lexeme listExpression
  , lexeme symbol
  ]

-- | Parse a number for tokenization (returns Double)
numberToken :: Parser Double
numberToken = do
  sign <- option ' ' (char '-' <|> char '+')
  digits <- many1 digit
  decimal <- option "" (char '.' >> many1 digit)
  let numStr = case sign of
        '-' -> "-" ++ digits ++ decimal
        '+' -> digits ++ decimal
        ' ' -> digits ++ decimal
        _ -> digits ++ decimal
  return $ read numStr

-- | Parse a number
number :: Parser Value
number = Number <$> try numberToken



-- | Parse a string literal
stringLiteral :: Parser Value
stringLiteral = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return $ String $ T.pack content

-- | Parse a quoted expression
quoted :: Parser Value
quoted = do
  _ <- char '\''
  expr <- schemeExpression
  return $ Quote expr

-- | Parse a list expression (supports both () and [])
listExpression :: Parser Value
listExpression = parens <|> brackets

-- | Parse parentheses list
parens :: Parser Value
parens = do
  _ <- char '('
  interTokenSpace
  xs <- sepEndBy schemeExpression interTokenSpace
  _ <- char ')'
  return $ if null xs then Nil else List xs

-- | Parse bracket list
brackets :: Parser Value
brackets = do
  _ <- char '['
  interTokenSpace
  xs <- sepEndBy schemeExpression interTokenSpace
  _ <- char ']'
  return $ if null xs then Nil else List xs

-- | Parse a symbol (including #t and #f)
symbol :: Parser Value
symbol = try (do
  s <- many1 (letter <|> digit <|> oneOf "+-*/=<>!?#")
  return $ Symbol $ T.pack s)
  <|> (do
    c <- oneOf "+-*/=<>!?"
    notFollowedBy (letter <|> digit <|> oneOf "+-*/=<>!?" )
    return $ Symbol $ T.pack [c]) 