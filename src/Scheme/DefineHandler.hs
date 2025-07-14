{-|
Module: Scheme.DefineHandler
Description: Handle define expressions with environment updates
Purpose: This module provides functions for handling define expressions
         in both the REPL and file execution contexts. It supports both
         variable and function definitions with proper environment updates.

Exports:
  - handleDefine: Handle define expressions by updating the environment
  - evalManyWithDefines: Evaluate multiple expressions with define handling
-}

module Scheme.DefineHandler
  ( handleDefine
  , evalManyWithDefines
  ) where

import Scheme.Core (Value(..), SchemeError(..), Environment)
import Scheme.Environment (defineVar, lookupVar)
import Scheme.Evaluator (evaluate, evaluateQuote, evaluateIf, evaluateLambda, evaluateLet, evaluateCond)
import Scheme.FreeVars (freeVarsInBody, captureFreeVars)
import Data.Text (Text)
import qualified Data.Text as T

-- | Handle define expressions by updating the environment
handleDefine :: Environment -> Value -> Either SchemeError (Environment, Value)
handleDefine env (List (Symbol s:args))
  | s == T.pack "define" = case args of
      [Symbol name, expr] -> do
        val <- evaluate env expr
        let newEnv = defineVar env name val
        return (newEnv, Symbol name)
      [List (Symbol name:params), body] -> do
        paramNames <- mapM extractSymbol params
        let freeVarSet = freeVarsInBody [body] paramNames
        let minimalEnv = captureFreeVars env freeVarSet
        let func = OptimizedFunction name [body] paramNames minimalEnv
        let newEnv = defineVar env name func
        return (newEnv, Symbol name)
      _ -> Left $ WrongNumberOfArgs (T.pack "define") (length args) 2
  | s == T.pack "quote" = (const evaluateQuote) env args >>= \val -> return (env, val)
  | s == T.pack "if" = evaluateIf env args >>= \val -> return (env, val)
  | s == T.pack "lambda" = evaluateLambda env args >>= \val -> return (env, val)
  | s == T.pack "let" = evaluateLet env args >>= \val -> return (env, val)
  | s == T.pack "cond" = evaluateCond env args >>= \val -> return (env, val)
  | otherwise = evaluate env (List (Symbol s:args)) >>= \val -> return (env, val)
handleDefine env expr = evaluate env expr >>= \val -> return (env, val)

-- | Evaluate a sequence of expressions with proper define handling
evalManyWithDefines :: Environment -> [Value] -> Either SchemeError Value
evalManyWithDefines _ [] = Right Nil
evalManyWithDefines env [x] = do
  (_, val) <- evalExpr env x
  return val
evalManyWithDefines env (x:xs) = case evalExpr env x of
  Left err -> Left err
  Right (newEnv, _) -> evalManyWithDefines newEnv xs

-- | Evaluate a single expression, using handleDefine for define expressions
evalExpr :: Environment -> Value -> Either SchemeError (Environment, Value)
evalExpr env (List (Symbol s:args))
  | s == T.pack "define" = handleDefine env (List (Symbol s:args))
evalExpr env expr = evaluate env expr >>= \val -> return (env, val)

-- | Helper function to extract symbol from a value
extractSymbol :: Value -> Either SchemeError Text
extractSymbol (Symbol name) = Right name
extractSymbol x = Left $ TypeError $ "Expected symbol, got " ++ show x 