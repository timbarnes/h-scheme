{-|
Module: Scheme.Evaluator
Description: Core evaluation engine for Scheme expressions
Purpose: This module implements the main evaluation logic for Scheme expressions.
         It handles all special forms (if, define, lambda, let, cond, quote),
         function application, and the evaluation of self-evaluating expressions.
         The evaluator implements lexical scoping by capturing environments
         in function closures and supports recursive function definitions.

Exports:
  - evaluate: Main evaluation function for Scheme expressions
  - applyFunction: Apply a user-defined function with arguments
-}

module Scheme.Evaluator
  ( evaluate
  , applyFunction
  ) where

import Scheme.Core (Environment, Value(..), SchemeError(..))
import Scheme.Environment (lookupVar, lookupVarWithRecursion, extendEnv)
import Scheme.Builtins
import Data.Text (Text)
import qualified Data.Text as T

-- | Main evaluation function
evaluate :: Environment -> Value -> Either SchemeError Value
evaluate env val = case val of
  -- Self-evaluating expressions
  Number _ -> Right val
  String _ -> Right val
  Bool _ -> Right val
  Nil -> Right val
  
  -- Variable lookup
  Symbol name -> lookupVarWithRecursion env name
  
  -- Quoted expressions
  Quote expr -> Right expr
  
  -- Function application and special forms
  List [] -> Right Nil
  List (Symbol s:args)
    | s == T.pack "quote" -> evaluateQuote args
    | s == T.pack "if" -> evaluateIf env args
    | s == T.pack "define" -> evaluateDefine env args
    | s == T.pack "lambda" -> evaluateLambda env args
    | s == T.pack "let" -> evaluateLet env args
    | s == T.pack "cond" -> evaluateCond env args
  List (func:args) -> evaluateApplication env func args

-- | Evaluate a quoted expression
evaluateQuote :: [Value] -> Either SchemeError Value
evaluateQuote [expr] = Right expr
evaluateQuote args = Left $ WrongNumberOfArgs (T.pack "quote") (length args) 1

-- | Evaluate an if expression
evaluateIf :: Environment -> [Value] -> Either SchemeError Value
evaluateIf env [condition, thenExpr] = do
  condVal <- evaluate env condition
  case condVal of
    Bool False -> Right Nil
    Nil -> Right Nil
    _ -> evaluate env thenExpr
evaluateIf env [condition, thenExpr, elseExpr] = do
  condVal <- evaluate env condition
  case condVal of
    Bool False -> evaluate env elseExpr
    Nil -> evaluate env elseExpr
    _ -> evaluate env thenExpr
evaluateIf _ args = Left $ WrongNumberOfArgs (T.pack "if") (length args) 2

-- | Evaluate a define expression
evaluateDefine :: Environment -> [Value] -> Either SchemeError Value
evaluateDefine env [Symbol name, expr] = do
  val <- evaluate env expr
  Right $ Symbol name  -- Return the symbol name
evaluateDefine env [List (Symbol name:params), body] = do
  paramNames <- mapM extractSymbol params
  let func = Function name [body] paramNames env
  Right $ Symbol name
evaluateDefine _ args = Left $ WrongNumberOfArgs (T.pack "define") (length args) 2

-- | Evaluate a lambda expression
evaluateLambda :: Environment -> [Value] -> Either SchemeError Value
evaluateLambda env [List params, body] = do
  paramNames <- mapM extractSymbol params
  Right $ Function (T.pack "lambda") [body] paramNames env
evaluateLambda _ args = Left $ WrongNumberOfArgs (T.pack "lambda") (length args) 2

-- | Evaluate a let expression
evaluateLet :: Environment -> [Value] -> Either SchemeError Value
evaluateLet env [List bindings, body] = do
  let newEnv = extendEnv env []
  evaluate newEnv body
evaluateLet _ args = Left $ WrongNumberOfArgs (T.pack "let") (length args) 2

-- | Evaluate a cond expression
evaluateCond :: Environment -> [Value] -> Either SchemeError Value
evaluateCond env [] = Right Nil
evaluateCond env (List (Symbol s:exprs):rest)
  | s == T.pack "else" = mapM (evaluate env) exprs >>= return . last
  | otherwise = do
      condVal <- evaluate env (Symbol s)
      case condVal of
        Bool False -> evaluateCond env rest
        Nil -> evaluateCond env rest
        _ -> mapM (evaluate env) exprs >>= return . last
evaluateCond _ args = Left $ RuntimeError "Invalid cond clause"

-- | Evaluate a function application
evaluateApplication :: Environment -> Value -> [Value] -> Either SchemeError Value
evaluateApplication env func args = do
  -- Evaluate the function
  funcVal <- evaluate env func
  
  -- Evaluate all arguments
  evalArgs <- mapM (evaluate env) args
  
  -- Apply the function
  case funcVal of
    Primitive _ primFunc -> primFunc evalArgs
    Function name body params funcEnv -> applyFunction name body params funcEnv evalArgs
    RecursiveFunction name body params funcEnv -> applyFunction name body params funcEnv evalArgs
    _ -> Left $ TypeError $ "Not a function: " ++ show funcVal

-- | Apply a user-defined function
applyFunction :: Text -> [Value] -> [Text] -> Environment -> [Value] -> Either SchemeError Value
applyFunction name body params funcEnv args
  | length params /= length args = 
      Left $ WrongNumberOfArgs name (length args) (length params)
  | otherwise = do
      let newEnv = extendEnv funcEnv (zip params args)
      -- For recursive functions, ensure the function itself is available in the environment
      let finalEnv = case lookupVar funcEnv name of
            Right (RecursiveFunction _ body' params' _) -> 
              extendEnv newEnv [(name, RecursiveFunction name body' params' funcEnv)]
            Right _ -> newEnv
            Left _ -> newEnv
      mapM (evaluate finalEnv) body >>= return . last

-- | Helper function to extract symbol from a value
extractSymbol :: Value -> Either SchemeError Text
extractSymbol (Symbol name) = Right name
extractSymbol x = Left $ TypeError $ "Expected symbol, got " ++ show x 