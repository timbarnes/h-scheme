{-|
Module: Main
Description: Entry point and REPL for the Scheme interpreter
Purpose: This module provides the main entry point for the Scheme interpreter,
         including a Read-Eval-Print Loop (REPL) for interactive use and
         support for executing Scheme files. It handles multi-line input,
         environment management for the REPL, and special forms like 'define'
         that need to update the environment. The REPL supports recursive
         function definitions and provides debugging capabilities.

Exports:
  - main: Main entry point for the executable
  - runRepl: Start the interactive REPL
  - runFile: Execute a Scheme file
  - handleDefine: Handle define expressions with environment updates
  - readMultiLine: Read multi-line input until complete
  - inspect: Debug function to examine variable definitions
-}

module Main where

import Scheme
import Scheme.Parser
import Scheme.Core (Value(..), SchemeError(..), Environment)
import Scheme.Environment (defineVar, lookupVar)
import Scheme.Evaluator (applyFunction)
import System.IO
import System.Environment
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [filename] -> runFile filename
    _ -> putStrLn "Usage: scheme [filename]"

-- | Run the REPL
runRepl :: IO ()
runRepl = do
  putStrLn "Scheme Interpreter v1.0"
  putStrLn "Type (quit) to exit"
  putStrLn ""
  loop builtins

-- | Main REPL loop
loop :: Environment -> IO ()
loop env = do
  putStr "scheme> "
  hFlush stdout
  input <- readMultiLine ""
  case input of
    "(quit)" -> putStrLn "Goodbye!"
    "" -> loop env
    _ -> do
      case Scheme.Parser.parse input of
        Left err -> 
          if isIncompleteInput err
            then putStrLn ("Error: " ++ show err) >> loop env
            else putStrLn ("Error: " ++ show err) >> loop env
        Right expr ->
          case handleDefine env expr of
            Left err -> putStrLn ("Error: " ++ show err) >> loop env
            Right (newEnv, val) -> putStrLn (show val) >> loop newEnv

-- | Handle define expressions by updating the environment
handleDefine :: Environment -> Value -> Either SchemeError (Environment, Value)
handleDefine env (List (Symbol s:args))
  | s == T.pack "define" = case args of
      [Symbol name, expr] -> do
        -- Step 1: Insert a placeholder
        let placeholder = Nil
        let envWithPlaceholder = defineVar env name placeholder
        -- Step 2: Evaluate the lambda in the environment with the placeholder
        val <- eval envWithPlaceholder expr
        -- Step 3: If it's a function, create a recursive function
        let finalVal = case val of
              Function _ body params _ -> RecursiveFunction name body params envWithPlaceholder
              _ -> val
        -- Step 4: Replace the placeholder with the actual function in the same environment
        let newEnv = replaceVar envWithPlaceholder name finalVal
        return (newEnv, Symbol name)
      _ -> Left $ WrongNumberOfArgs (T.pack "define") (length args) 2
  | s == T.pack "inspect" = case args of
      [Symbol name] -> do
        case lookupVar env name of
          Left err -> return (env, String $ T.pack $ "Variable not found: " ++ show err)
          Right val -> return (env, val)
      _ -> Left $ WrongNumberOfArgs (T.pack "inspect") (length args) 1
  | otherwise = eval env (List (Symbol s:args)) >>= \val -> return (env, val)
handleDefine env expr = eval env expr >>= \val -> return (env, val)

-- | Replace a variable in the environment (assumes it exists)
replaceVar :: Environment -> Text -> Value -> Environment
replaceVar [] _ _ = []
replaceVar ((var, val):rest) name newVal
  | var == name = (var, newVal) : rest
  | otherwise = (var, val) : replaceVar rest name newVal

-- | Create a placeholder function that can be called recursively
createRecursivePlaceholder :: Text -> Environment -> Value
createRecursivePlaceholder name env = 
  Primitive (T.pack $ "recursive-" ++ T.unpack name) 
    (\args -> 
      -- Look up the actual function in the environment
      case lookupVar env name of
        Left _ -> Left $ RuntimeError $ "Function " ++ T.unpack name ++ " not yet defined"
        Right (Function _ body params funcEnv) -> 
          -- Apply the actual function
          applyFunction name body params funcEnv args
        Right _ -> Left $ TypeError $ T.unpack name ++ " is not a function"
    )

-- | Helper function to extract symbol from a value
extractSymbol :: Value -> Either SchemeError Text
extractSymbol (Symbol name) = Right name
extractSymbol x = Left $ TypeError $ "Expected symbol, got " ++ show x

-- | Run a Scheme file
runFile :: FilePath -> IO ()
runFile filename = do
  result <- evalFile filename
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> putStrLn $ show val 

-- | Read multi-line input, continuing until input is complete
readMultiLine :: String -> IO String
readMultiLine acc = do
  line <- getLine
  let fullInput = acc ++ line
  case Scheme.Parser.parse fullInput of
    Right _ -> return fullInput  -- Successfully parsed, input is complete
    Left err -> 
      if isIncompleteInput err
        then do
          putStr "  "  -- Indent continuation lines
          readMultiLine (fullInput ++ "\n")
        else return fullInput  -- Real error, return what we have

-- | Check if a parse error indicates incomplete input
isIncompleteInput :: SchemeError -> Bool
isIncompleteInput (ParseError msg) = 
  "Unclosed parenthesis" `isInfixOf` msg ||
  "Unexpected end of input" `isInfixOf` msg ||
  "Unexpected tokens" `isInfixOf` msg
isIncompleteInput _ = False 