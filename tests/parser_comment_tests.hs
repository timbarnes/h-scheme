{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Scheme.Parser (parseMany)
import Scheme.Core (Value(..))
import Data.Text (pack)
import System.IO (readFile)

main :: IO ()
main = do
  src <- readFile "tests/parser_comment_tests.scm"
  let parsed = parseMany src
  case parsed of
    Left err -> assertFailure ("Parse error: " ++ show err)
    Right exprs -> do
      _ <- runTestTT (commentParserTests exprs)
      return ()

commentParserTests :: [Value] -> Test
commentParserTests exprs = TestList
  [ "x defined" ~: elem (List [Symbol "define", Symbol "x", Number 42]) exprs ~? "x definition present"
  , "y defined" ~: elem (List [Symbol "define", Symbol "y", Number 100]) exprs ~? "y definition present"
  , "z defined as list" ~: elem (List [Symbol "define", Symbol "z", List [Symbol "list", Number 1, Number 2, Number 3]]) exprs ~? "z definition present"
  , "add function defined" ~: elem (List [Symbol "define", List [Symbol "add", Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]) exprs ~? "add function present"
  , "quoted defined" ~: elem (List [Symbol "define", Symbol "quoted", Quote (List [Number 1, Number 2, Number 3])]) exprs ~? "quoted definition present"
  ] 