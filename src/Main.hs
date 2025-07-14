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
  - handleEOF: Handle EOF errors during input
  - readCompleteInput: Read multi-line input until complete
  - loop: Main REPL loop (now uses unified input accumulation)
  - replaceVar: Replace a variable in the environment (assumes it exists)
  - extractSymbol: Helper function to extract symbol from a value
  - isIncompleteInput: Check if a parse error indicates incomplete input
-}

module Main (main, runRepl, runFile, handleEOF, readCompleteInput, loop, replaceVar, extractSymbol, isIncompleteInput) where

import Scheme
import Scheme.Parser
import Scheme.Builtins (displayValue)
import Scheme.DefineHandler (evalManyWithDefines)
import System.IO
import System.Environment
import Data.Text (Text)
import Data.List (isInfixOf)
import System.IO.Error (isEOFError)
import Control.Exception (catch, IOException)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> catch runRepl handleEOF
    [filename] -> catch (runFile filename) handleEOF
    _ -> putStrLn "Usage: scheme [filename]"

handleEOF :: IOException -> IO ()
handleEOF e
  | isEOFError e = return ()
  | otherwise    = ioError e

-- | Run the REPL
runRepl :: IO ()
runRepl = do
  putStrLn "Scheme Interpreter v1.0"
  putStrLn "Type (quit) to exit"
  putStrLn ""
  loop builtins

-- | Unified input accumulation: reads lines/chunks until parentheses are balanced
readCompleteInput :: IO String
readCompleteInput = go "" True 0
  where
    go acc isFirst parenCount = do
      if isFirst
        then putStr "scheme> "
        else putStr "  "
      hFlush stdout
      line <- getLine
      let fullInput = if null acc then line else acc ++ "\n" ++ line
          newParenCount = parenCount + countParens line
      if newParenCount > 0 || (newParenCount == 0 && null fullInput)
        then go fullInput False newParenCount
        else if newParenCount < 0
          then do
            putStrLn "Error: Too many closing parentheses."
            return ""
          else return fullInput
    
    countParens :: String -> Int
    countParens = foldl update 0
      where
        update n '(' = n + 1
        update n ')' = n - 1
        update n  _  = n

-- | Main REPL loop (now uses unified input accumulation)
loop :: Environment -> IO ()
loop env = do
  input <- readCompleteInput
  case input of
    "(quit)" -> putStrLn "Goodbye!"
    "" -> loop env
    _ -> do
      case Scheme.Parser.parseMany input of
        Left err -> putStrLn ("Error: " ++ show err) >> loop env
        Right exprs ->
          case Scheme.DefineHandler.evalManyWithDefines env exprs of
            Left err -> putStrLn ("Error: " ++ show err) >> loop env
            Right (newEnv, val) -> do
              putStrLn (displayValue val)
              loop newEnv

-- | Handle define expressions by updating the environment
-- handleDefine env (List (Symbol s:args))
--   | s == T.pack "define" = case args of
--       [Symbol name, expr] -> do
--         -- Step 1: Insert a placeholder
--         let placeholder = Nil
--         let envWithPlaceholder = defineVar env name placeholder
--         -- Step 2: Evaluate the lambda in the environment with the placeholder
--         val <- eval envWithPlaceholder expr
--         -- Step 3: If it's a function, create a recursive function
--         let finalVal = case val of
--               Function _ body params _ -> Function name body params envWithPlaceholder
--               OptimizedFunction _ body params freeVars -> 
--                 -- For recursive optimized functions, we need to include the function itself
--                 let recursiveFreeVars = (name, Function name body params envWithPlaceholder) : freeVars
--                 in OptimizedFunction name body params recursiveFreeVars
--               _ -> val
--         -- Step 4: Replace the placeholder with the actual function in the same environment
--         let newEnv = replaceVar envWithPlaceholder name finalVal
--         return (newEnv, Symbol name)
--       [List (Symbol name:params), body] -> do
--         -- Function definition: (define (name params) body)
--         paramNames <- mapM extractSymbol params
--         -- Find free variables in the function body
--         let freeVarSet = freeVarsInBody [body] paramNames
--         -- Create minimal environment with only referenced variables
--         let minimalEnv = captureFreeVars env freeVarSet
--         -- Create the optimized function
--         let func = OptimizedFunction name [body] paramNames minimalEnv
--         -- Add to environment
--         let newEnv = defineVar env name func
--         return (newEnv, Symbol name)
--       _ -> Left $ WrongNumberOfArgs (T.pack "define") (length args) 2
--   | s == T.pack "inspect" = case args of
--       [Symbol name] -> do
--         case lookupVar env name of
--           Left err -> return (env, String $ T.pack $ "Variable not found: " ++ show err)
--           Right val -> return (env, val)
--       _ -> Left $ WrongNumberOfArgs (T.pack "inspect") (length args) 1
--   | s == T.pack "tokens" = case args of
--       [String input] -> do
--         case tokenize (T.unpack input) of
--           Left err -> return (env, String $ T.pack $ "Tokenization error: " ++ show err)
--           Right tokens -> return (env, String $ T.pack $ "Tokens: " ++ show tokens)
--       _ -> Left $ WrongNumberOfArgs (T.pack "tokens") (length args) 1
--   | otherwise = eval env (List (Symbol s:args)) >>= \val -> return (env, val)
-- handleDefine env expr = eval env expr >>= \val -> return (env, val)

-- | Replace a variable in the environment (assumes it exists)
replaceVar :: Environment -> Text -> Value -> Environment
replaceVar [] _ _ = []
replaceVar ((var, val):rest) name newVal
  | var == name = (var, newVal) : rest
  | otherwise = (var, val) : replaceVar rest name newVal

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
    Right val -> putStrLn $ displayValue val 

-- | Check if a parse error indicates incomplete input
isIncompleteInput :: SchemeError -> Bool
isIncompleteInput (ParseError msg) = 
  "Unclosed parenthesis" `isInfixOf` msg ||
  "Unexpected end of input" `isInfixOf` msg ||
  "Unexpected tokens" `isInfixOf` msg
isIncompleteInput _ = False 