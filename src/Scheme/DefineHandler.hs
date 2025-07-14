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
import Scheme.Environment (defineVar, lookupVar, setVar)
import Scheme.Evaluator (evaluate, evaluateQuote, evaluateIf, evaluateLambda, evaluateLet, evaluateCond)
import Scheme.FreeVars (freeVarsInBody, captureFreeVars)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

-- | Handle define expressions by updating the environment
handleDefine :: Environment -> Value -> Either SchemeError (Environment, Value)
handleDefine env (List (Symbol s:args))
  | s == T.pack "define" = case args of
      [Symbol name, List (Symbol lam:paramsList), bodyExpr] | lam == T.pack "lambda" -> do
        -- paramsList is a list of parameter symbols
        paramNames <- mapM extractSymbol paramsList
        let body = bodyExpr
        let recEnv = let closure = RecursiveFunction name [body] paramNames recEnv
                     in defineVar env name closure
        let closure = RecursiveFunction name [body] paramNames recEnv
        let newEnv = defineVar env name closure
        return (newEnv, Symbol name)
      [Symbol name, expr@(List (Symbol lam:paramListAndBody))] | lam == T.pack "lambda" -> do
        case paramListAndBody of
          (List ps : body) -> do
            paramNames <- mapM extractSymbol ps
            let bodyVal = if null body then Nil else if length body == 1 then head body else List (Symbol (T.pack "begin") : body)
            let recEnv = let closure = RecursiveFunction name [bodyVal] paramNames recEnv
                         in defineVar env name closure
            let closure = RecursiveFunction name [bodyVal] paramNames recEnv
            let newEnv = defineVar env name closure
            return (newEnv, Symbol name)
          _ -> Left $ TypeError "Malformed lambda in define"
      [Symbol name, expr] -> do
        val <- evaluate env expr
        let newEnv = defineVar env name val
        return (newEnv, Symbol name)
      (List (Symbol name:paramsList):bodyExprs) -> do
        paramNames <- mapM extractSymbol paramsList
        let body = if length bodyExprs == 1 
                   then head bodyExprs 
                   else List (Symbol (T.pack "begin") : bodyExprs)
        let recEnv = let closure = RecursiveFunction name [body] paramNames recEnv
                     in defineVar env name closure
        let closure = RecursiveFunction name [body] paramNames recEnv
        let newEnv = defineVar env name closure
        return (newEnv, Symbol name)
      _ -> Left $ WrongNumberOfArgs (T.pack "define") (length args) 2
  | otherwise = evaluate env (List (Symbol s:args)) >>= \val -> return (env, val)
handleDefine env expr = evaluate env expr >>= \val -> return (env, val)

-- | Evaluate a sequence of expressions with proper define handling
evalManyWithDefines :: Environment -> [Value] -> Either SchemeError (Environment, Value)
evalManyWithDefines env [] = Right (env, Nil)
evalManyWithDefines env [x] = do
  (newEnv, val) <- evalExpr env x
  return (newEnv, val)
evalManyWithDefines env (x:xs) = case evalExpr env x of
  Left err -> Left err
  Right (newEnv, _) -> evalManyWithDefines newEnv xs

-- | Evaluate a single expression, using handleDefine for define expressions and set! for mutation
evalExpr :: Environment -> Value -> Either SchemeError (Environment, Value)
evalExpr env (List (Symbol s:args))
  | s == T.pack "define" = handleDefine env (List (Symbol s:args))
  | s == T.pack "set!" = case args of
      [Symbol name, expr] -> do
        val <- evaluate env expr
        newEnv <- setVar env name val
        return (newEnv, val)
      _ -> Left $ WrongNumberOfArgs (T.pack "set!") (length args) 2
evalExpr env expr = evaluate env expr >>= \val -> return (env, val)

-- | Helper function to extract symbol from a value
extractSymbol :: Value -> Either SchemeError Text
extractSymbol (Symbol name) = Right name
extractSymbol x = Left $ TypeError $ "Expected symbol, got " ++ show x 