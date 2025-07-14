{-|
Module: Scheme.Core
Description: Core data types and value representation for the Scheme interpreter
Purpose: This module defines the fundamental data structures used throughout
         the interpreter, including the Value type (which represents all Scheme
         values), error types, and the Environment type. It also provides
         pretty-printing functionality for Scheme values.

Exports:
  - Value: The main data type representing all Scheme values
  - SchemeError: Error types for the interpreter
  - Environment: Type alias for the environment (list of name-value pairs)
  - showValue: Pretty-printing function for Scheme values
-}

module Scheme.Core
  ( Value(..)
  , SchemeError(..)
  , showValue
  , Environment
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Environment is a list of (name, value) pairs
type Environment = [(Text, Value)]

-- | Core Scheme values
data Value
  = Number Double           -- ^ Numeric values
  | String Text             -- ^ String literals
  | Symbol Text             -- ^ Symbols (variables, function names)
  | Bool Bool               -- ^ Boolean values
  | List [Value]            -- ^ Lists (including function calls)
  | Nil                     -- ^ Empty list
  | Function { funcName :: Text, funcBody :: [Value], funcParams :: [Text], funcEnv :: Environment }
  | RecursiveFunction { recName :: Text, recBody :: [Value], recParams :: [Text], recEnv :: Environment }
  | OptimizedFunction { optName :: Text, optBody :: [Value], optParams :: [Text], optFreeVars :: Environment }
  | Primitive { primName :: Text, primFunc :: [Value] -> Either SchemeError Value }
  | Quote Value             -- ^ Quoted expressions
 
instance Eq Value where
  (Number a) == (Number b) = a == b
  (String a) == (String b) = a == b
  (Symbol a) == (Symbol b) = a == b
  (Bool a) == (Bool b) = a == b
  (List a) == (List b) = a == b
  Nil == Nil = True
  (Function n1 b1 p1 _) == (Function n2 b2 p2 _) = n1 == n2 && b1 == b2 && p1 == p2
  (RecursiveFunction n1 b1 p1 _) == (RecursiveFunction n2 b2 p2 _) = n1 == n2 && b1 == b2 && p1 == p2
  (OptimizedFunction n1 b1 p1 _) == (OptimizedFunction n2 b2 p2 _) = n1 == n2 && b1 == b2 && p1 == p2
  (Primitive n1 _) == (Primitive n2 _) = n1 == n2
  (Quote a) == (Quote b) = a == b
  _ == _ = False

-- | Error types for the interpreter
data SchemeError
  = ParseError String
  | TypeError String
  | RuntimeError String
  | UnboundVariable Text
  | WrongNumberOfArgs Text Int Int
  | DivisionByZero
  | EmptyList String
  deriving (Eq, Show)

instance Show Value where
  show = T.unpack . showValue

-- | Pretty print a Scheme value
showValue :: Value -> Text
showValue (Number n) = T.pack (show n)
showValue (String s) = T.pack $ "\"" ++ T.unpack s ++ "\""
showValue (Symbol s) = s
showValue (Bool True) = T.pack "#t"
showValue (Bool False) = T.pack "#f"
showValue Nil = T.pack "()"
showValue (List []) = T.pack "()"
showValue (List (x:xs)) = 
  case x of
    Symbol s | s == T.pack "quote" -> T.pack "'" <> showValue (head xs)
    _ -> T.pack "(" <> T.intercalate (T.pack " ") (map showValue (x:xs)) <> T.pack ")"
showValue (Function name _ _ _) = T.pack $ "<function:" ++ T.unpack name ++ ">"
showValue (RecursiveFunction name body params _) = 
  T.pack $ "<recursive-function:" ++ T.unpack name ++ " params:" ++ show params ++ " body:" ++ show body ++ ">"
showValue (OptimizedFunction name _ params _) = 
  T.pack $ "<optimized-function:" ++ T.unpack name ++ " params:" ++ show params ++ ">"
showValue (Primitive name _) = T.pack $ "<primitive:" ++ T.unpack name ++ ">"
showValue (Quote v) = T.pack "'" <> showValue v 