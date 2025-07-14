{-|
Module: Scheme.Builtins
Description: Built-in functions and primitives for the Scheme interpreter
Purpose: This module provides all the built-in functions that are available
         in the Scheme interpreter, including arithmetic operations, list
         processing functions, comparison operators, and I/O functions.
         These functions are implemented as Primitive values that can be
         called directly by the evaluator. The module exports a single
         'builtins' environment containing all available built-in functions.

Exports:
  - builtins: Environment containing all built-in functions
    - Arithmetic: +, -, *, /, =, <, >, <=, >=
    - List processing: car, cdr, cons, null?, list?, length, append, list
    - I/O: display, newline
-}

module Scheme.Builtins
  ( builtins
  ) where

import Scheme.Core (Value(..), SchemeError(..), Environment)
import Data.Text (Text)
import qualified Data.Text as T

-- | Built-in functions environment
builtins :: Environment
builtins = 
  [ (T.pack "+", Primitive (T.pack "+") add)
  , (T.pack "-", Primitive (T.pack "-") subtract')
  , (T.pack "*", Primitive (T.pack "*") multiply)
  , (T.pack "/", Primitive (T.pack "/") divide)
  , (T.pack "=", Primitive (T.pack "=") equal)
  , (T.pack "<", Primitive (T.pack "<") lessThan)
  , (T.pack ">", Primitive (T.pack ">") greaterThan)
  , (T.pack "<=", Primitive (T.pack "<=") lessEqual)
  , (T.pack ">=", Primitive (T.pack ">=") greaterEqual)
  , (T.pack "car", Primitive (T.pack "car") car)
  , (T.pack "cdr", Primitive (T.pack "cdr") cdr)
  , (T.pack "cons", Primitive (T.pack "cons") cons)
  , (T.pack "null?", Primitive (T.pack "null?") isNull)
  , (T.pack "list?", Primitive (T.pack "list?") isList)
  , (T.pack "length", Primitive (T.pack "length") length')
  , (T.pack "append", Primitive (T.pack "append") append)
  , (T.pack "list", Primitive (T.pack "list") list)
  , (T.pack "display", Primitive (T.pack "display") display)
  , (T.pack "newline", Primitive (T.pack "newline") newline)
  ]

-- | Arithmetic functions
add :: [Value] -> Either SchemeError Value
add args = do
  nums <- mapM toNumber args
  return $ Number $ sum nums

subtract' :: [Value] -> Either SchemeError Value
subtract' [] = Left $ WrongNumberOfArgs (T.pack "-") 0 1
subtract' [x] = do
  n <- toNumber x
  return $ Number (-n)
subtract' (x:xs) = do
  first <- toNumber x
  rest <- mapM toNumber xs
  return $ Number $ first - sum rest

multiply :: [Value] -> Either SchemeError Value
multiply args = do
  nums <- mapM toNumber args
  return $ Number $ product nums

divide :: [Value] -> Either SchemeError Value
divide [] = Left $ WrongNumberOfArgs (T.pack "/") 0 1
divide [x] = do
  n <- toNumber x
  if n == 0
    then Left DivisionByZero
    else return $ Number (1 / n)
divide (x:xs) = do
  first <- toNumber x
  rest <- mapM toNumber xs
  if any (== 0) rest
    then Left DivisionByZero
    else return $ Number $ first / product rest

-- | Comparison functions
equal :: [Value] -> Either SchemeError Value
equal args = do
  nums <- mapM toNumber args
  return $ Bool $ all (== head nums) (tail nums)

lessThan :: [Value] -> Either SchemeError Value
lessThan args = do
  nums <- mapM toNumber args
  return $ Bool $ and $ zipWith (<) nums (tail nums)

greaterThan :: [Value] -> Either SchemeError Value
greaterThan args = do
  nums <- mapM toNumber args
  return $ Bool $ and $ zipWith (>) nums (tail nums)

lessEqual :: [Value] -> Either SchemeError Value
lessEqual args = do
  nums <- mapM toNumber args
  return $ Bool $ and $ zipWith (<=) nums (tail nums)

greaterEqual :: [Value] -> Either SchemeError Value
greaterEqual args = do
  nums <- mapM toNumber args
  return $ Bool $ and $ zipWith (>=) nums (tail nums)

-- | List processing functions
car :: [Value] -> Either SchemeError Value
car [List (x:_)] = Right x
car [Nil] = Left $ EmptyList "car"
car [x] = Left $ TypeError $ "car: expected list, got " ++ show x
car args = Left $ WrongNumberOfArgs (T.pack "car") (length args) 1

cdr :: [Value] -> Either SchemeError Value
cdr [List (_:xs)] = Right $ List xs
cdr [List []] = Right Nil
cdr [Nil] = Left $ EmptyList "cdr"
cdr [x] = Left $ TypeError $ "cdr: expected list, got " ++ show x
cdr args = Left $ WrongNumberOfArgs (T.pack "cdr") (length args) 1

cons :: [Value] -> Either SchemeError Value
cons [x, List xs] = Right $ List (x:xs)
cons [x, Nil] = Right $ List [x]
cons [x, y] = Right $ List [x, y]
cons args = Left $ WrongNumberOfArgs (T.pack "cons") (length args) 2

isNull :: [Value] -> Either SchemeError Value
isNull [Nil] = Right $ Bool True
isNull [List []] = Right $ Bool True
isNull [_] = Right $ Bool False
isNull args = Left $ WrongNumberOfArgs (T.pack "null?") (length args) 1

isList :: [Value] -> Either SchemeError Value
isList [List _] = Right $ Bool True
isList [Nil] = Right $ Bool True
isList [_] = Right $ Bool False
isList args = Left $ WrongNumberOfArgs (T.pack "list?") (length args) 1

length' :: [Value] -> Either SchemeError Value
length' [List xs] = Right $ Number $ fromIntegral $ length xs
length' [Nil] = Right $ Number 0
length' [x] = Left $ TypeError $ "length: expected list, got " ++ show x
length' args = Left $ WrongNumberOfArgs (T.pack "length") (length args) 1

append :: [Value] -> Either SchemeError Value
append [] = Right Nil
append [x] = Right x
append (List xs:rest) = do
  restResult <- append rest
  case restResult of
    List ys -> Right $ List (xs ++ ys)
    Nil -> Right $ List xs
    _ -> Left $ TypeError "append: expected list"
append (Nil:rest) = append rest
append (x:_) = Left $ TypeError $ "append: expected list, got " ++ show x

list :: [Value] -> Either SchemeError Value
list args = Right $ List args

-- | I/O functions
display :: [Value] -> Either SchemeError Value
display [x] = Right $ String $ T.pack $ show x
display args = Left $ WrongNumberOfArgs (T.pack "display") (length args) 1

newline :: [Value] -> Either SchemeError Value
newline [] = Right $ String $ T.pack "\n"
newline args = Left $ WrongNumberOfArgs (T.pack "newline") (length args) 0

-- | Helper function to convert values to numbers
toNumber :: Value -> Either SchemeError Double
toNumber (Number n) = Right n
toNumber x = Left $ TypeError $ "Expected number, got " ++ show x 