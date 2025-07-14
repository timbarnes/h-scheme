#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Test Runner for Scheme Interpreter
-- This script loads and executes the regression test suite

import System.IO (readFile)
import System.Process (readProcess)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (when)

main :: IO ()
main = do
  putStrLn "Running Scheme Interpreter Regression Tests"
  putStrLn "==========================================="
  putStrLn ""
  
  -- Check if the test file exists
  testContent <- readFile "tests/regression_tests.scm"
  
  -- Run the tests by piping the content to the scheme interpreter
  (exitCode, stdout, stderr) <- readProcess "./dist-newstyle/build/x86_64-osx/ghc-9.4.7/scheme-0.1.0.0/x/scheme/build/scheme/scheme" [] testContent
  
  -- Print the output
  putStrLn stdout
  when (not (null stderr)) $ do
    putStrLn "STDERR:"
    putStrLn stderr
  
  -- Exit with appropriate code
  case exitCode of
    ExitSuccess -> do
      putStrLn "\n✅ All tests completed successfully!"
      exitWith ExitSuccess
    ExitFailure code -> do
      putStrLn $ "\n❌ Tests failed with exit code: " ++ show code
      exitWith (ExitFailure code) 