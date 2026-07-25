-- | SMTLib2 surface syntax: atoms and lists, nothing else. Atoms are
-- | kept raw — a string literal is an atom that includes its quotes,
-- | a |quoted symbol| includes its pipes. `print` is the wire format;
-- | `feed`/`finish` is a streaming parser for chunked solver stdout.
module Mycroft.SExpr
  ( SExpr(..)
  , print
  , Mode
  , ParseState
  , initialState
  , feed
  , finish
  , parseAll
  ) where

import Prelude

import Data.Array (snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray, toCharArray)

data SExpr
  = Atom String
  | List (Array SExpr)

derive instance Eq SExpr
derive instance Ord SExpr

instance Show SExpr where
  show = case _ of
    Atom s -> "(Atom " <> show s <> ")"
    List xs -> "(List " <> show xs <> ")"

print :: SExpr -> String
print = case _ of
  Atom s -> s
  List xs -> "(" <> joinWith " " (map print xs) <> ")"

data Mode
  = Normal
  | InString
  | InStringEscape
  | InQuoted
  | InComment

-- | Streaming parser state. Feed chunks as they arrive from the
-- | solver; completed toplevel forms fall out of each `feed`.
type ParseState =
  { stack :: List (List SExpr) -- open lists, innermost first, elements reversed
  , atom :: List Char -- current atom, reversed
  , mode :: Mode
  }

initialState :: ParseState
initialState = { stack: Nil, atom: Nil, mode: Normal }

feed :: ParseState -> String -> Either String { state :: ParseState, exprs :: Array SExpr }
feed st0 chunk = foldM step { state: st0, exprs: [] } (toCharArray chunk)
  where
  step acc c = case acc.state.mode of
    Normal -> normal acc c
    InString
      | c == '\\' -> Right (setMode InStringEscape (consAtom acc c))
      | c == '"' -> Right (setMode Normal (consAtom acc c))
      | otherwise -> Right (consAtom acc c)
    InStringEscape -> Right (setMode InString (consAtom acc c))
    InQuoted
      | c == '|' -> Right (setMode Normal (consAtom acc c))
      | otherwise -> Right (consAtom acc c)
    InComment
      | c == '\n' -> Right (setMode Normal acc)
      | otherwise -> Right acc

  normal acc c
    | c == '(' = Right (open (flushAtom acc))
    | c == ')' = close (flushAtom acc)
    | isSpace c = Right (flushAtom acc)
    | c == '"' = Right (setMode InString (consAtom acc c))
    | c == '|' = Right (setMode InQuoted (consAtom acc c))
    | c == ';' = Right (setMode InComment (flushAtom acc))
    | otherwise = Right (consAtom acc c)

  isSpace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

  setMode m acc = acc { state = acc.state { mode = m } }

  consAtom acc c = acc { state = acc.state { atom = c : acc.state.atom } }

  flushAtom acc = case acc.state.atom of
    Nil -> acc
    cs -> deliver (Atom (revString cs)) (acc { state = acc.state { atom = Nil } })

  deliver e acc = case acc.state.stack of
    Nil -> acc { exprs = snoc acc.exprs e }
    top : rest -> acc { state = acc.state { stack = (e : top) : rest } }

  open acc = acc { state = acc.state { stack = Nil : acc.state.stack } }

  close acc = case acc.state.stack of
    Nil -> Left "unbalanced ')'"
    top : rest ->
      Right
        ( deliver (List (Array.reverse (Array.fromFoldable top)))
            (acc { state = acc.state { stack = rest } })
        )

-- | End of input: flush a trailing toplevel atom, reject anything
-- | left half-open.
finish :: ParseState -> Either String (Array SExpr)
finish st = case st.mode of
  InString -> Left "unterminated string literal"
  InStringEscape -> Left "unterminated string literal"
  InQuoted -> Left "unterminated quoted symbol"
  _ -> case st.stack of
    _ : _ -> Left "unterminated list"
    Nil -> case st.atom of
      Nil -> Right []
      cs -> Right [ Atom (revString cs) ]

parseAll :: String -> Either String (Array SExpr)
parseAll s = do
  r <- feed initialState s
  rest <- finish r.state
  pure (r.exprs <> rest)

revString :: List Char -> String
revString cs = fromCharArray (Array.reverse (Array.fromFoldable cs))
