{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Scheme.Parser (parse, parseMany)
import Scheme.Core (Value(..))
import Data.Text (Text, pack)

main :: IO ()
main = do
  _ <- runTestTT parserTests
  return ()

parserTests :: Test
parserTests = TestList
  [ "number" ~: parse "42" ~?= Right (Number 42)
  , "negative number" ~: parse "-5" ~?= Right (Number (-5))
  , "string" ~: parse "\"hello\"" ~?= Right (String "hello")
  , "boolean true" ~: parse "#t" ~?= Right (Symbol "#t")
  , "boolean false" ~: parse "#f" ~?= Right (Symbol "#f")
  , "symbol" ~: parse "foo" ~?= Right (Symbol "foo")
  , "operator symbol" ~: parse "+" ~?= Right (Symbol "+")
  , "list" ~: parse "(1 2 3)" ~?= Right (List [Number 1, Number 2, Number 3])
  , "empty list" ~: parse "()" ~?= Right Nil
  , "nested list" ~: parse "(a (b c) d)" ~?= Right (List [Symbol "a", List [Symbol "b", Symbol "c"], Symbol "d"])
  , "quoted" ~: parse "'(1 2 3)" ~?= Right (Quote (List [Number 1, Number 2, Number 3]))
  , "define variable" ~: parse "(define x 42)" ~?= Right (List [Symbol "define", Symbol "x", Number 42])
  , "define function (shorthand)" ~: parse "(define (f x) (+ x 1))" ~?= Right (List [Symbol "define", List [Symbol "f", Symbol "x"], List [Symbol "+", Symbol "x", Number 1]])
  , "define function (lambda)" ~: parse "(define f (lambda (x) (+ x 1)))" ~?= Right (List [Symbol "define", Symbol "f", List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Symbol "x", Number 1]]])
  , "multiple expressions" ~: parseMany "(define x 1) (define y 2)" ~?= Right [List [Symbol "define", Symbol "x", Number 1], List [Symbol "define", Symbol "y", Number 2]]
  ] 