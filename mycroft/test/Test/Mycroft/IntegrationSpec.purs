-- | Integration against a real z3: declare/assert/check-sat/get-model,
-- | scopes, enums, named assertions, unsat cores. Needs `z3` on PATH.
module Test.Mycroft.IntegrationSpec (run) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Mycroft.SmtLib (Result(..), addT, andT, app, assertNamed, checkSat, declareConst, declareEnum, distinct, getModel, getUnsatCore, inNewScope, intLit, lookupAtom, neqT, notT, produceModels, produceUnsatCores, tBool, tInt, var)
import Mycroft.SmtLib as Smt
import Mycroft.Solver (newSolver, stop, z3Config)
import Test.Assert (assertEqual, assertTrue')

run :: Aff Unit
run = do
  s <- newSolver z3Config
  produceModels s
  produceUnsatCores s

  liftEffect (log "Solver: sat with a model")
  x <- declareConst s "x" tInt
  Smt.assert s (app ">" [ x, intLit 5 ])
  r1 <- checkSat s
  liftEffect (assertEqual { actual: r1, expected: Sat })
  m1 <- getModel s
  liftEffect (assertTrue' "model binds x" (Map.member "x" m1))

  liftEffect (log "Solver: push/pop scoping")
  inNewScope s do
    Smt.assert s (app "<" [ x, intLit 3 ])
    r <- checkSat s
    liftEffect (assertEqual { actual: r, expected: Unsat })
  r2 <- checkSat s
  liftEffect (assertEqual { actual: r2, expected: Sat })

  liftEffect (log "SmtLib: enum sorts decode from the model")
  color <- declareEnum s { name: "Color", ctors: [ "red", "green", "blue" ] }
  c <- declareConst s "c" color
  Smt.assert s (neqT c (var "red"))
  Smt.assert s (neqT c (var "green"))
  r3 <- checkSat s
  liftEffect (assertEqual { actual: r3, expected: Sat })
  m2 <- getModel s
  liftEffect (assertEqual { actual: lookupAtom m2 "c", expected: Just "blue" })

  liftEffect (log "SmtLib: named assertions give a deterministic core")
  inNewScope s do
    p <- declareConst s "p" tBool
    assertNamed s "a" p
    assertNamed s "b" (notT p)
    r <- checkSat s
    liftEffect (assertEqual { actual: r, expected: Unsat })
    core <- getUnsatCore s
    liftEffect (assertEqual { actual: Array.sort core, expected: [ "a", "b" ] })

  liftEffect (log "SmtLib: pigeonhole is unsat with a core drawn from the names")
  inNewScope s do
    pigeons <- traverse (\i -> declareConst s ("pg" <> show i) tInt) [ 1, 2, 3 ]
    for_ (Array.zip [ 1, 2, 3 ] pigeons) \(Tuple i pg) ->
      assertNamed s ("dom" <> show i)
        ( andT
            [ app "<=" [ intLit 0, pg ]
            , app "<" [ pg, intLit 2 ]
            ]
        )
    assertNamed s "alldiff" (distinct pigeons)
    r <- checkSat s
    liftEffect (assertEqual { actual: r, expected: Unsat })
    core <- getUnsatCore s
    let names = [ "dom1", "dom2", "dom3", "alldiff" ]
    liftEffect (assertTrue' "core is nonempty" (not (Array.null core)))
    liftEffect
      ( assertTrue' "core only mentions our names"
          (Array.all (\n -> Array.elem n names) core)
      )

  liftEffect (log "SmtLib: countEq cardinality")
  inNewScope s do
    ys <- traverse (\i -> declareConst s ("y" <> show i) tInt) [ 1, 2, 3, 4 ]
    for_ ys \y -> Smt.assert s (andT [ app "<=" [ intLit 0, y ], app "<=" [ y, intLit 1 ] ])
    Smt.assert s (Smt.eqT (Smt.countEq ys (intLit 1)) (intLit 2))
    Smt.assert s (Smt.eqT (addT ys) (intLit 2))
    r <- checkSat s
    liftEffect (assertEqual { actual: r, expected: Sat })

  stop s
