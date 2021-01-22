{-# LANGUAGE OverloadedStrings #-}

module ParserTests (tests) where

import Data.Text (Text, unpack, unlines)
import Test.HUnit
import Prelude hiding (unlines)

import Common
import Harakiri.Expr
import Harakiri.Parser

tests :: Test
tests = mkTestLabel "parser tests"
    [ assertParseML
        [ "def main() {"
        , "a = input()"
        , "b = input()"
        , "echo(\"a: \", a, \"b: \", b)"
        , "}"
        ]
        [ Function "main" [] TVoid $ mkSeq
            [ mkAssign "a" mkInput
            , mkAssign "b" mkInput
            , mkEcho [ StrArg "a: "
                     , ExprArg (mkVar "a")
                     , StrArg "b: "
                     , ExprArg (mkVar "b")
                     ]
            ]
        ]
    , assertParseML
        [ "def print_interval(from, to) {"
        , "while from < to {"
        , "echo(from)"
        , "from = from + 1"
        , "}"
        , "}"
        , "def main() {"
        , "echo(\"enter a\")"
        , "a = input()"
        , "echo(\"enter b\")"
        , "b = input()"
        , "print_interval(a-2, b)"
        , "echo((a + b) * b)"
        , "echo(a + b * b)"
        , "}"
        ]
        [ Function "print_interval" ["from", "to"] TVoid $
            mkWhile (mkVar "from" $< mkVar "to") $
                mkSeq [ mkEcho [ExprArg (mkVar "from")]
                      , mkAssign "from" (mkVar "from" $+ mkIntLit 1)
                      ]
        , Function "main" [] TVoid $
            mkSeq [ mkEcho [StrArg "enter a"]
                  , mkAssign "a" mkInput
                  , mkEcho [StrArg "enter b"]
                  , mkAssign "b" mkInput
                  , mkCall "print_interval" [mkVar "a" $- mkIntLit 2, mkVar "b"]
                  , mkEcho [ExprArg ((mkVar "a" $+ mkVar "b") $* mkVar "b")]
                  , mkEcho [ExprArg (mkVar "a" $+ (mkVar "b" $* mkVar "b"))]
                  ]
        ]
    , assertParseML
        [ "def main() {"
        , "a = 1"
        , "a = -a + 8"
        , "if a == 8 || a == 9 {"
        , "a = 90"
        , "} else { "
        , "if a == 10 {"
        , "a = 100"
        , "}"
        , "}"
        , "echo(a)"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkSeq [ mkAssign "a" (mkIntLit 1)
                  , mkAssign "a" (mkNeg (mkVar "a") $+ mkIntLit 8)
                  , mkIf [ (mkVar "a" $== mkIntLit 8) $|| (mkVar "a" $== mkIntLit 9)
                         , mkAssign "a" (mkIntLit 90)
                         , mkIf [ mkVar "a" $== mkIntLit 10
                                , mkAssign "a" (mkIntLit 100)
                                ]
                         ]
                  , mkEcho [ExprArg (mkVar "a")]
                  ]
        ]
    , assertParseML
        [ "def main() {"
        , "while 1 {"
        , "a = input()"
        , "if a == 1 {"
        , "break"
        , "}"
        , "}"
        , "}"
        ]
        [ Function "main" [] TVoid $
            mkWhile (mkIntLit 1) $
                mkSeq [ mkAssign "a" mkInput
                      , mkIf [ mkVar "a" $== mkIntLit 1
                             , mkBreak
                             ]
                      ]
        ]
    , assertParseFailML
        [ "def main() {"
        , "123 = a"
        , "}"
        ]
    , assertParseFailML
        [ "def main() {"
        , "if a = 123 {"
        , "a = 5"
        , "}"
        , "}"
        ]
    , assertParseFailML
        [ "def main() {"
        , "input()"
        , "}"
        ]
    , assertParseFailML
        [ "def main() {"
        , "a = input(1, 2)"
        , "}"
        ]
    , assertParseFailML
        [ "def main() {"
        , "a = break"
        , "}"
        ]
    ]

assertParseML :: [Text] -> [Function Expr] -> Assertion
assertParseML src = assertParse (unlines src)

assertParse :: Text -> [Function Expr] -> Assertion
assertParse src expected = case parseFromText "<string>" src of
    Left err ->
        assertFailure $ "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ err
    Right actual ->
        assertEqual ("When parsing " ++ unpack src)
            (ShowFunctions expected) (ShowFunctions $ map (fmap stripAnnotation) actual)

assertParseFailML :: [Text] -> Assertion
assertParseFailML = assertParseFail . unlines

assertParseFail :: Text -> Assertion
assertParseFail src = case parseFromText "<string>" src of
    Left _ -> return ()
    Right res ->
        assertFailure $ "Unexpected success parsing `"
            ++ unpack src ++ "`:\nParsed value "
            ++ show (ShowFunctions $ map (fmap stripAnnotation) res)
