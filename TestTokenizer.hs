{-# LANGUAGE OverloadedStrings #-}
import Scheme.Parser (tokenize)
import System.IO (readFile)

main :: IO ()
main = do
  putStrLn "Testing tokenizer on tests/comment_test.scm..."
  input <- readFile "tests/comment_test.scm"
  case tokenize input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right tokens -> putStrLn $ "Tokens: " ++ show tokens 