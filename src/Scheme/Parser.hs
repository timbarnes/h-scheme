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
  , tokenize
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

-- | Lexeme combinator: parses p and skips trailing whitespace/comments
lexeme :: Parser a -> Parser a
lexeme p = p <* skipWhitespaceOrComment

-- | Parse a Scheme expression from a string (now parses all forms and returns the last)
parse :: String -> Either SchemeError Value
parse input =
  case runParser schemeFile () "" input of
    Left err -> Left $ ParseError $ show err
    Right [] -> Right Nil
    Right exprs -> Right (last exprs)

-- | Parse a Scheme file with multiple top-level forms
schemeFile :: Parser [Value]
schemeFile = skipWhitespaceOrComment *> sepEndBy schemeExpression skipWhitespaceOrComment <* eof

-- | Parse multiple Scheme expressions from a string
parseMany :: String -> Either SchemeError [Value]
parseMany input =
  case runParser schemeFile () "" input of
    Left err -> Left $ ParseError $ show err
    Right exprs -> Right exprs

-- | Skip whitespace and comments
skipWhitespaceOrComment :: Parser ()
skipWhitespaceOrComment = skipMany (skipMany1 space <|> comment)

-- | Parse and skip a comment
comment :: Parser ()
comment = do
  _ <- char ';'
  _ <- manyTill anyChar (try (endOfLine >> return ()) <|> (eof >> return ()))
  return ()

-- | Parse a Scheme expression
schemeExpression :: Parser Value
schemeExpression = choice
  [ selfEvaluating
  , quotedExpression
  , listExpression
  , symbolExpression
  ]

-- | Parse self-evaluating expressions
selfEvaluating :: Parser Value
selfEvaluating = choice
  [ lexeme (Number <$> number)
  , lexeme (String <$> stringLiteral)
  , lexeme (Bool <$> boolean)
  ]

-- | Parse a quoted expression
quotedExpression :: Parser Value
quotedExpression = do
  _ <- char '\''
  skipWhitespaceOrComment
  expr <- schemeExpression
  return $ Quote expr

-- | Parse a list expression
listExpression :: Parser Value
listExpression = do
  _ <- char '('
  skipWhitespaceOrComment
  elements <- sepEndBy schemeExpression skipWhitespaceOrComment
  _ <- char ')'
  return $ case elements of
    [] -> Nil
    _ -> List elements

-- | Parse a symbol
symbolExpression :: Parser Value
symbolExpression = lexeme (Symbol <$> symbol)

-- | Parse a string literal
stringLiteral :: Parser Text
stringLiteral = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return $ T.pack content

-- | Parse a number
number :: Parser Double
number = do
  digits <- many1 digit
  decimal <- option "" (char '.' >> many1 digit)
  return $ read (digits ++ decimal)

-- | Parse a boolean literal
boolean :: Parser Bool
boolean = choice
  [ string "#t" >> return True
  , string "#f" >> return False
  ]

-- | Parse a symbol
symbol :: Parser Text
symbol = do
  first <- letter <|> char '+' <|> char '-' <|> char '*' <|> char '/' <|> char '=' <|> char '<' <|> char '>' <|> char '!'
  rest <- many (letter <|> digit <|> char '+' <|> char '-' <|> char '*' <|> char '/' <|> char '=' <|> char '<' <|> char '>' <|> char '!' <|> char '?')
  return $ T.pack (first:rest)

-- | Legacy tokenizer for debugging (kept for compatibility)
tokenize :: String -> Either SchemeError [Token]
tokenize input = case runParser tokenizer () "" input of
  Left err -> Left $ ParseError $ show err
  Right tokens -> Right tokens

-- | Token type for parsing (legacy)
data Token
  = TLParen
  | TRParen
  | TQuote
  | TString Text
  | TNumber Double
  | TBool Bool
  | TSymbol Text
  deriving (Show, Eq)

-- | Legacy tokenizer parser
tokenizer :: Parser [Token]
tokenizer = skipWhitespaceOrComment *> sepEndBy schemeToken skipWhitespaceOrComment <* eof

-- | Legacy single token parser
schemeToken :: Parser Token
schemeToken = choice
  [ char '(' >> return TLParen
  , char ')' >> return TRParen
  , char '\'' >> return TQuote
  , TString <$> stringLiteral
  , TNumber <$> number
  , TBool <$> boolean
  , TSymbol <$> symbol
  ]

-- | Legacy expression parser from tokens
parseTokens :: [Token] -> Either SchemeError Value
parseTokens tokens = case parseExpression tokens of
  Left err -> Left $ ParseError $ show err
  Right (expr, []) -> Right expr
  Right (expr, rest) -> Left $ ParseError $ "Unexpected tokens: " ++ show rest

-- | Legacy parse expression from tokens
parseExpression :: [Token] -> Either SchemeError (Value, [Token])
parseExpression [] = Left $ ParseError "Unexpected end of input"
parseExpression (TLParen:rest) = parseList rest
parseExpression (TQuote:rest) = parseQuote rest
parseExpression (TString s:rest) = Right (String s, rest)
parseExpression (TNumber n:rest) = Right (Number n, rest)
parseExpression (TBool b:rest) = Right (Bool b, rest)
parseExpression (TSymbol s:rest) = Right (Symbol s, rest)
parseExpression (t:rest) = Left $ ParseError $ "Unexpected token: " ++ show t

-- | Legacy parse list from tokens
parseList :: [Token] -> Either SchemeError (Value, [Token])
parseList [] = Left $ ParseError "Unclosed parenthesis"
parseList (TRParen:rest) = Right (Nil, rest)
parseList tokens = do
  (first, tokens') <- parseExpression tokens
  (rest, tokens'') <- parseListRest tokens'
  return (List (first:rest), tokens'')

-- | Legacy parse list rest from tokens
parseListRest :: [Token] -> Either SchemeError ([Value], [Token])
parseListRest [] = Left $ ParseError "Unclosed parenthesis"
parseListRest (TRParen:rest) = Right ([], rest)
parseListRest tokens = do
  (expr, tokens') <- parseExpression tokens
  (rest, tokens'') <- parseListRest tokens'
  return (expr:rest, tokens'')

-- | Legacy parse quote from tokens
parseQuote :: [Token] -> Either SchemeError (Value, [Token])
parseQuote tokens = do
  (expr, rest) <- parseExpression tokens
  return (Quote expr, rest) 