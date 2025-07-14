{-|
Module: Scheme
Description: Main module for the Scheme interpreter, providing the public API
Purpose: This module serves as the entry point and re-exports the core functionality
         of the Scheme interpreter, including evaluation, parsing, and environment
         management functions.

Exports:
  - Core types: Value, Environment, SchemeError
  - Evaluation functions: eval, evalString, evalFile
  - Environment management: emptyEnv, extendEnv, lookupVar
  - Free variable analysis: freeVars, freeVarsInBody, captureFreeVars
  - Built-in functions: builtins
  - Utilities: showValue
-}

module Scheme
  ( -- * Core types
    Value(..)
  , Environment
  , SchemeError(..)
  
  -- * Evaluation
  , eval
  , evalString
  , evalFile
  
  -- * Environment management
  , emptyEnv
  , extendEnv
  , lookupVar
  
  -- * Free variable analysis
  , freeVars
  , freeVarsInBody
  , captureFreeVars
  
  -- * Built-in functions
  , builtins
  
  -- * Utilities
  , showValue
  ) where

import Scheme.Core
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Environment
import Scheme.FreeVars
import Scheme.DefineHandler
import Scheme.Builtins
import Debug.Trace (trace)
import qualified Data.Text as T

-- Re-export main functionality
eval :: Environment -> Value -> Either SchemeError Value
eval = evaluate

evalString :: String -> Either SchemeError Value
evalString input = do
  exprs <- parseMany input
  evalMany builtins exprs

-- Evaluate a sequence of expressions, returning the last result
evalMany :: Environment -> [Value] -> Either SchemeError Value
evalMany _ [] = Right Nil
evalMany env [x] = evaluate env x
evalMany env (x:xs) = case evaluate env x of
  Left err -> Left err
  Right _ -> evalMany env xs

evalFile :: FilePath -> IO (Either SchemeError Value)
evalFile path = do
  content <- readFile path
  case parseMany content of
    Left err -> return $ Left err
    Right exprs -> return $ evalManyWithDefines builtins exprs
