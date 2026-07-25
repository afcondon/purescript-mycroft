-- | The subprocess integration beats, replayed against the WASM
-- | backend: same veneer, same assertions, no z3 binary anywhere.
module Test.Z3Wasm.Main (main) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Mycroft.SmtLib (Result(..), app, assertNamed, checkSat, declareConst, declareEnum, getModel, getUnsatCore, inNewScope, intLit, lookupAtom, neqT, notT, produceModels, produceUnsatCores, tBool, tInt, var)
import Mycroft.SmtLib as Smt
import Mycroft.Solver (stop)
import Mycroft.Solver.Z3Wasm (newZ3WasmSolver, shutdownZ3Wasm)
import Test.Assert (assertEqual, assertTrue')

main :: Effect Unit
main = launchAff_ do
  s <- newZ3WasmSolver { log: Nothing }
  produceModels s
  produceUnsatCores s

  liftEffect (log "Z3Wasm: sat with a model")
  x <- declareConst s "x" tInt
  Smt.assert s (app ">" [ x, intLit 5 ])
  r1 <- checkSat s
  liftEffect (assertEqual { actual: r1, expected: Sat })
  m1 <- getModel s
  liftEffect (assertTrue' "model binds x" (Map.member "x" m1))

  liftEffect (log "Z3Wasm: push/pop scoping")
  inNewScope s do
    Smt.assert s (app "<" [ x, intLit 3 ])
    r <- checkSat s
    liftEffect (assertEqual { actual: r, expected: Unsat })
  r2 <- checkSat s
  liftEffect (assertEqual { actual: r2, expected: Sat })

  liftEffect (log "Z3Wasm: enum sorts decode from the model")
  color <- declareEnum s { name: "Color", ctors: [ "red", "green", "blue" ] }
  c <- declareConst s "c" color
  Smt.assert s (neqT c (var "red"))
  Smt.assert s (neqT c (var "green"))
  r3 <- checkSat s
  liftEffect (assertEqual { actual: r3, expected: Sat })
  m2 <- getModel s
  liftEffect (assertEqual { actual: lookupAtom m2 "c", expected: Just "blue" })

  liftEffect (log "Z3Wasm: named assertions give a deterministic core")
  inNewScope s do
    p <- declareConst s "p" tBool
    assertNamed s "a" p
    assertNamed s "b" (notT p)
    r <- checkSat s
    liftEffect (assertEqual { actual: r, expected: Unsat })
    core <- getUnsatCore s
    liftEffect (assertEqual { actual: Array.sort core, expected: [ "a", "b" ] })

  stop s
  shutdownZ3Wasm
  liftEffect (log "All mycroft-z3-wasm tests passed")
