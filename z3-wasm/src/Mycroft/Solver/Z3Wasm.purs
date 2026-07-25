-- | The WASM backend: Z3 compiled to WebAssembly (the official
-- | `z3-solver` npm package) behind the same `Solver` interface — no
-- | native binary, no subprocess. Works in Node; works in the browser
-- | on a cross-origin-isolated page (the WASM build uses pthreads, so
-- | `SharedArrayBuffer` — and therefore COOP/COEP headers — is
-- | required there).
-- |
-- | Transport: `Z3_eval_smtlib2_string` against one persistent
-- | context. State carries across calls, so the lockstep
-- | write-one-read-one discipline maps onto exactly one eval per
-- | command; the response string feeds the same streaming parser the
-- | subprocess backend uses.
-- |
-- | The WASM module and its worker pool are initialised once per
-- | process and shared by every solver; each solver owns its own Z3
-- | context. `stop` releases a solver's context; `shutdownZ3Wasm`
-- | tears down the shared worker pool (call it last — Node won't exit
-- | while the workers live, and no further WASM solvers can be created
-- | after it).
module Mycroft.Solver.Z3Wasm
  ( newZ3WasmSolver
  , shutdownZ3Wasm
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Mycroft.Solver (Solver, Transport, newSolverWith)

foreign import startImpl
  :: (String -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect (Promise Transport)

foreign import shutdownImpl :: Effect (Promise Unit)

newZ3WasmSolver :: { log :: Maybe (String -> Effect Unit) } -> Aff Solver
newZ3WasmSolver cfg = newSolverWith
  { start: \cbs -> toAffE (startImpl cbs.onChunk cbs.onFailure)
  , log: cfg.log
  }

shutdownZ3Wasm :: Aff Unit
shutdownZ3Wasm = toAffE shutdownImpl
