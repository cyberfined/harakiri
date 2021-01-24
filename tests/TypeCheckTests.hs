module TypeCheckTests (tests) where

import Data.Text (Text, unpack, unlines)
import Test.HUnit
import Prelude hiding (unlines, lines)

import Common
import Harakiri

tests :: Test
tests = mkTestLabel "type checker tests"
    [ assertTypeCheck
        [ "def fib(n) {"
        , "if n < 2 {"
        , "return 1"
        , "}"
        , "return fib(n-1) + fib(n-2)"
        , "}"
        , "def main() {"
        , "echo(fib(5))"
        , "}"
        ]
    , assertTypeCheck
        [ "def fun(n) {"
        , "if n < 7 || n > 10 {"
        , "return"
        , "}"
        , "echo(n)"
        , "}"
        , "def main() {"
        , "fun(1)"
        , "}"
        ]
    , assertTypeCheck
        [ "def main() {"
        , "n = input()"
        , "if n < 5 {"
        , "while n > 0 {"
        , "echo(n)"
        , "n = n / 2"
        , "}"
        , "} else {"
        , "echo(n * 7)"
        , "}"
        , "}"
        ]
    , assertTypeCheckFail
        [ "def main() {"
        , "break"
        , "}"
        ]
    , assertTypeCheckFail
        [ "def main() {"
        , "fib()"
        , "}"
        ]
    , assertTypeCheckFail
        [ "def fun(n) {"
        , "if n < 1 {"
        , "return"
        , "}"
        , "return 1"
        , "}"
        , "def main() {"
        , "fun(1)"
        , "}"
        ]
    , assertTypeCheckFail
        [ "def fun(n) {"
        , "if n < 1 {"
        , "return 7 * 9 + n"
        , "} else {"
        , "return"
        , "}"
        , "return 6"
        , "}"
        , "def main() {"
        , "fun(1)"
        , "}"
        ]
    , assertTypeCheckFail
        [ "def main(a, b) {"
        , "echo(a, b)"
        , "}"
        ]
    , assertTypeCheckFail
        [ "def fun(n) {"
        , "a = b - 7"
        , "}"
        ]
    ]

assertTypeCheck :: [Text] -> Assertion
assertTypeCheck lines = case parseFromText "<string>" src of
    Left parseErr -> assertFailure $
        "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ unpack parseErr
    Right funcs -> case typeCheck src funcs of
        Just checkErr -> assertFailure $
            "Unexpected error type checking `" ++ unpack src ++ "`:\n" ++ unpack checkErr
        Nothing -> return ()
  where src = unlines lines

assertTypeCheckFail :: [Text] -> Assertion
assertTypeCheckFail lines = case parseFromText "<string>" src of
    Left err -> assertFailure $
        "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ unpack err
    Right funcs -> case typeCheck src funcs of
        Nothing -> assertFailure $
            "Unexpected success type checking `" ++ unpack src ++ "`:\n"
        Just{} -> return ()
  where src = unlines lines
