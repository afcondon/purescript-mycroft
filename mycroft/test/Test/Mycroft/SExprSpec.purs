module Test.Mycroft.SExprSpec (run) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Effect (Effect)
import Effect.Console (log)
import Mycroft.SExpr (SExpr(..), feed, finish, initialState, parseAll, print)
import Test.Assert (assertEqual)
import Test.QuickCheck (Result, quickCheckGen', (===))
import Test.QuickCheck.Gen (Gen, chooseInt, elements, oneOf, vectorOf)

run :: Effect Unit
run = do
  log "SExpr: golden prints"
  assertEqual
    { actual: print (List [ Atom "assert", List [ Atom ">", Atom "x", Atom "5" ] ])
    , expected: "(assert (> x 5))"
    }
  assertEqual
    { actual: print (List [])
    , expected: "()"
    }

  log "SExpr: golden parses"
  assertEqual
    { actual: parseAll "(declare-const x Int)"
    , expected: Right [ List [ Atom "declare-const", Atom "x", Atom "Int" ] ]
    }
  assertEqual
    { actual: parseAll "sat\n"
    , expected: Right [ Atom "sat" ]
    }
  assertEqual
    { actual: parseAll "; a comment\nunsat"
    , expected: Right [ Atom "unsat" ]
    }
  assertEqual
    { actual: parseAll "(a \"b c\" |d e|)"
    , expected: Right [ List [ Atom "a", Atom "\"b c\"", Atom "|d e|" ] ]
    }
  assertEqual
    { actual: parseAll "(= x \"he said \"\"hi\"\"\")"
    , expected: Right [ List [ Atom "=", Atom "x", Atom "\"he said \"\"hi\"\"\"" ] ]
    }
  assertEqual
    { actual: parseAll "(a)(b) c"
    , expected: Right [ List [ Atom "a" ], List [ Atom "b" ], Atom "c" ]
    }
  assertEqual
    { actual: parseAll "((f x) (g (h)))"
    , expected: Right
        [ List
            [ List [ Atom "f", Atom "x" ]
            , List [ Atom "g", List [ Atom "h" ] ]
            ]
        ]
    }

  log "SExpr: parse errors"
  assertEqual { actual: parseAll "(a b", expected: Left "unterminated list" }
  assertEqual { actual: parseAll "a)", expected: Left "unbalanced ')'" }
  assertEqual { actual: parseAll "\"oops", expected: Left "unterminated string literal" }

  log "SExpr: round-trip property"
  quickCheckGen' 200 roundTrip

  log "SExpr: chunked-feed property (one char at a time)"
  quickCheckGen' 200 chunkedAgrees

roundTrip :: Gen Result
roundTrip = do
  e <- genSExpr 3
  pure (parseAll (print e) === Right [ e ])

-- Feeding the printed form one character at a time must agree with
-- parsing it whole — the streaming states are the thing under test.
chunkedAgrees :: Gen Result
chunkedAgrees = do
  e <- genSExpr 3
  pure (parseChunked (print e <> "\n") === Right [ e ])

parseChunked :: String -> Either String (Array SExpr)
parseChunked s = do
  let pieces = map singleton (toCharArray s)
  r <- foldM
    ( \acc c -> feed acc.state c <#> \fed ->
        { state: fed.state, exprs: acc.exprs <> fed.exprs }
    )
    { state: initialState, exprs: [] }
    pieces
  rest <- finish r.state
  pure (r.exprs <> rest)

genAtomString :: Gen String
genAtomString = do
  n <- chooseInt 1 6
  cs <- vectorOf n (elements atomChars)
  pure (fromCharArray cs)
  where
  atomChars = NEA.cons' 'a'
    [ 'b', 'c', 'x', 'y', 'z', '-', '+', '<', '=', '>', '!', '.', '0', '1', '9' ]

genSExpr :: Int -> Gen SExpr
genSExpr depth =
  if depth <= 0 then Atom <$> genAtomString
  else oneOf
    ( NEA.cons' (Atom <$> genAtomString)
        [ do
            n <- chooseInt 0 4
            List <$> vectorOf n (genSExpr (depth - 1))
        ]
    )
