-- | The typed veneer: untyped `Term`s built by smart constructors,
-- | simple-smt style. Enough SMTLib2 for finite-domain work — enum
-- | sorts, Int arithmetic for cardinality, uninterpreted functions,
-- | named assertions, models, unsat cores.
module Mycroft.SmtLib
  ( Sort(..)
  , Term(..)
  , Result(..)
  , Model
  , setOption
  , produceModels
  , produceUnsatCores
  , declareEnum
  , declareConst
  , declareFun
  , assert
  , assertNamed
  , checkSat
  , getModel
  , getUnsatCore
  , push
  , pop
  , inNewScope
  , tBool
  , tInt
  , var
  , app
  , eqT
  , neqT
  , distinct
  , andT
  , orT
  , notT
  , implies
  , ite
  , boolLit
  , intLit
  , addT
  , countEq
  , lookupAtom
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, finally)
import Effect.Exception (error)
import Mycroft.SExpr (SExpr(..))
import Mycroft.SExpr as SExpr
import Mycroft.Solver (Solver, ackCommand, command, simpleCommand)

newtype Sort = Sort SExpr

newtype Term = Term SExpr

derive instance Eq Term

data Result = Sat | Unsat | Unknown

derive instance Eq Result

instance Show Result where
  show = case _ of
    Sat -> "Sat"
    Unsat -> "Unsat"
    Unknown -> "Unknown"

-- | Constant assignments from `get-model`: name ↦ value s-expression.
type Model = Map String SExpr

setOption :: Solver -> String -> String -> Aff Unit
setOption s k v = simpleCommand s [ "set-option", k, v ]

produceModels :: Solver -> Aff Unit
produceModels s = setOption s ":produce-models" "true"

produceUnsatCores :: Solver -> Aff Unit
produceUnsatCores s = setOption s ":produce-unsat-cores" "true"

-- | (declare-datatypes ((Name 0)) (((c1) (c2) …))) — a nullary
-- | datatype, i.e. an enumeration.
declareEnum :: Solver -> { name :: String, ctors :: Array String } -> Aff Sort
declareEnum s { name, ctors } = do
  ackCommand s
    ( List
        [ Atom "declare-datatypes"
        , List [ List [ Atom name, Atom "0" ] ]
        , List [ List (map (\c -> List [ Atom c ]) ctors) ]
        ]
    )
  pure (Sort (Atom name))

declareConst :: Solver -> String -> Sort -> Aff Term
declareConst s name (Sort sort) = do
  ackCommand s (List [ Atom "declare-const", Atom name, sort ])
  pure (Term (Atom name))

declareFun :: Solver -> String -> Array Sort -> Sort -> Aff Term
declareFun s name args (Sort ret) = do
  ackCommand s
    (List [ Atom "declare-fun", Atom name, List (map (\(Sort a) -> a) args), ret ])
  pure (Term (Atom name))

assert :: Solver -> Term -> Aff Unit
assert s (Term t) = ackCommand s (List [ Atom "assert", t ])

-- | (assert (! t :named n)) — named assertions are what unsat cores
-- | are made of.
assertNamed :: Solver -> String -> Term -> Aff Unit
assertNamed s n (Term t) =
  ackCommand s (List [ Atom "assert", List [ Atom "!", t, Atom ":named", Atom n ] ])

checkSat :: Solver -> Aff Result
checkSat s = do
  r <- command s (List [ Atom "check-sat" ])
  case r of
    Atom "sat" -> pure Sat
    Atom "unsat" -> pure Unsat
    Atom "unknown" -> pure Unknown
    other -> throwError (error ("check-sat: unexpected response: " <> SExpr.print other))

getModel :: Solver -> Aff Model
getModel s = do
  r <- command s (List [ Atom "get-model" ])
  case parseModel r of
    Right m -> pure m
    Left err -> throwError (error ("get-model: " <> err <> " in: " <> SExpr.print r))

parseModel :: SExpr -> Either String Model
parseModel = case _ of
  List items ->
    -- older z3 wraps the list in a `model` keyword; newer doesn't
    let
      defs = case Array.uncons items of
        Just { head: Atom "model", tail } -> tail
        _ -> items
    in
      Right (Map.fromFoldable (Array.mapMaybe constDef defs))
  _ -> Left "expected a list"
  where
  constDef = case _ of
    List [ Atom "define-fun", Atom name, List [], _sort, value ] -> Just (Tuple name value)
    _ -> Nothing

getUnsatCore :: Solver -> Aff (Array String)
getUnsatCore s = do
  r <- command s (List [ Atom "get-unsat-core" ])
  case r of
    List names -> for names case _ of
      Atom n -> pure n
      other -> throwError (error ("get-unsat-core: unexpected " <> SExpr.print other))
    other -> throwError (error ("get-unsat-core: unexpected response: " <> SExpr.print other))

push :: Solver -> Aff Unit
push s = simpleCommand s [ "push", "1" ]

pop :: Solver -> Aff Unit
pop s = simpleCommand s [ "pop", "1" ]

-- | Run an action inside a push/pop scope; the pop happens even if
-- | the action throws.
inNewScope :: forall a. Solver -> Aff a -> Aff a
inNewScope s act = push s *> finally (pop s) act

tBool :: Sort
tBool = Sort (Atom "Bool")

tInt :: Sort
tInt = Sort (Atom "Int")

var :: String -> Term
var = Term <<< Atom

app :: String -> Array Term -> Term
app f args = Term (List ([ Atom f ] <> map (\(Term t) -> t) args))

eqT :: Term -> Term -> Term
eqT a b = app "=" [ a, b ]

neqT :: Term -> Term -> Term
neqT a b = notT (eqT a b)

distinct :: Array Term -> Term
distinct = app "distinct"

andT :: Array Term -> Term
andT = app "and"

orT :: Array Term -> Term
orT = app "or"

notT :: Term -> Term
notT a = app "not" [ a ]

implies :: Term -> Term -> Term
implies a b = app "=>" [ a, b ]

ite :: Term -> Term -> Term -> Term
ite c t e = app "ite" [ c, t, e ]

boolLit :: Boolean -> Term
boolLit b = Term (Atom (if b then "true" else "false"))

intLit :: Int -> Term
intLit n = if n < 0 then app "-" [ intLit (negate n) ] else Term (Atom (show n))

addT :: Array Term -> Term
addT = app "+"

-- | How many of the given terms equal the target — the sum-of-ite
-- | cardinality encoding, for hand-size style constraints.
countEq :: Array Term -> Term -> Term
countEq ts target = addT (map (\t -> ite (eqT t target) (intLit 1) (intLit 0)) ts)

-- | Read a model value that should be a bare atom (an Int literal or
-- | an enum constructor).
lookupAtom :: Model -> String -> Maybe String
lookupAtom m name = case Map.lookup name m of
  Just (Atom v) -> Just v
  _ -> Nothing
