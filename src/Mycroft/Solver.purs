-- | The session layer: a solver kept in lockstep. The simple-smt trick
-- | — set `:print-success` immediately, so every command produces
-- | exactly one response s-expression and framing is trivial: write one
-- | form, read one form.
-- |
-- | The transport is abstracted: `newSolver` speaks to a subprocess
-- | (`z3 -in -smt2` by default); other backends (the WASM build, a
-- | remote solver) supply their own `Transport` via `newSolverWith`
-- | and inherit the whole veneer above this module unchanged.
module Mycroft.Solver
  ( Solver
  , SolverConfig
  , Callbacks
  , Transport
  , z3Config
  , newSolver
  , newSolverWith
  , command
  , ackCommand
  , simpleCommand
  , stop
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn4)
import Mycroft.SExpr (SExpr(..))
import Mycroft.SExpr as SExpr

foreign import data SolverHandle :: Type

-- cmd, args, onStdoutChunk, onFailure (spawn error / unexpected exit)
foreign import spawnImpl
  :: EffectFn4 String (Array String) (EffectFn1 String Unit) (EffectFn1 String Unit) SolverHandle

foreign import writeImpl :: EffectFn2 SolverHandle String Unit

foreign import killImpl :: EffectFn1 SolverHandle Unit

-- | What a backend receives: where to deliver solver output (chunked,
-- | any framing) and where to report a dead backend.
type Callbacks =
  { onChunk :: String -> Effect Unit
  , onFailure :: String -> Effect Unit
  }

-- | What a backend provides: write one line of SMTLib2, and tear the
-- | backend down (called at most once, by `stop`).
type Transport =
  { send :: String -> Effect Unit
  , close :: Effect Unit
  }

type SolverConfig =
  { cmd :: String
  , args :: Array String
  , log :: Maybe (String -> Effect Unit)
  }

z3Config :: SolverConfig
z3Config = { cmd: "z3", args: [ "-in", "-smt2" ], log: Nothing }

newtype Solver = Solver
  { transport :: Transport
  , parse :: Ref SExpr.ParseState
  , ready :: Ref (Array SExpr)
  , waiter :: Ref (Maybe (Either Error SExpr -> Effect Unit))
  , dead :: Ref (Maybe String)
  , log :: Maybe (String -> Effect Unit)
  }

-- | The subprocess backend.
newSolver :: SolverConfig -> Aff Solver
newSolver cfg = newSolverWith
  { start: \cbs -> liftEffect do
      handle <- runEffectFn4 spawnImpl cfg.cmd cfg.args
        (mkEffectFn1 cbs.onChunk)
        (mkEffectFn1 cbs.onFailure)
      pure
        { send: \s -> runEffectFn2 writeImpl handle s
        , close: do
            runEffectFn2 writeImpl handle "(exit)\n"
            runEffectFn1 killImpl handle
        }
  , log: cfg.log
  }

-- | Bring up a solver over any transport and put it in lockstep mode.
newSolverWith
  :: { start :: Callbacks -> Aff Transport
     , log :: Maybe (String -> Effect Unit)
     }
  -> Aff Solver
newSolverWith cfg = do
  refs <- liftEffect do
    parse <- Ref.new SExpr.initialState
    ready <- Ref.new []
    waiter <- Ref.new Nothing
    dead <- Ref.new Nothing
    pure { parse, ready, waiter, dead }
  let
    failWith msg = do
      d <- Ref.read refs.dead
      when (isNothing d) (Ref.write (Just msg) refs.dead)
      w <- Ref.read refs.waiter
      Ref.write Nothing refs.waiter
      for_ w \k -> k (Left (error msg))

    emit e = do
      w <- Ref.read refs.waiter
      case w of
        Just k -> do
          Ref.write Nothing refs.waiter
          k (Right e)
        Nothing -> Ref.modify_ (_ <> [ e ]) refs.ready

    onChunk chunk = do
      for_ cfg.log \l -> l ("[recv] " <> chunk)
      st <- Ref.read refs.parse
      case SExpr.feed st chunk of
        Left err -> failWith ("response parse error: " <> err)
        Right r -> do
          Ref.write r.state refs.parse
          for_ r.exprs emit

    onFailure msg = failWith ("solver backend: " <> msg)

  transport <- cfg.start { onChunk, onFailure }
  let
    solver = Solver
      { transport
      , parse: refs.parse
      , ready: refs.ready
      , waiter: refs.waiter
      , dead: refs.dead
      , log: cfg.log
      }
  ackCommand solver (List [ Atom "set-option", Atom ":print-success", Atom "true" ])
  pure solver

-- | Send one command, await its one response. Lockstep discipline:
-- | callers issue one command at a time (all the veneer functions do).
command :: Solver -> SExpr -> Aff SExpr
command (Solver s) e = makeAff \k -> do
  d <- Ref.read s.dead
  case d of
    Just msg -> k (Left (error ("solver unavailable: " <> msg)))
    Nothing -> do
      let line = SExpr.print e <> "\n"
      for_ s.log \l -> l ("[send] " <> line)
      s.transport.send line
      buffered <- Ref.read s.ready
      case Array.uncons buffered of
        Just { head, tail } -> do
          Ref.write tail s.ready
          k (Right head)
        Nothing -> Ref.write (Just k) s.waiter
  pure nonCanceler

-- | A command whose only acceptable response is the atom `success`.
ackCommand :: Solver -> SExpr -> Aff Unit
ackCommand solver e = do
  r <- command solver e
  case r of
    Atom "success" -> pure unit
    other -> throwError
      (error ("expected success for " <> SExpr.print e <> ", got: " <> SExpr.print other))

simpleCommand :: Solver -> Array String -> Aff Unit
simpleCommand solver atoms = ackCommand solver (List (map Atom atoms))

stop :: Solver -> Aff Unit
stop (Solver s) = liftEffect do
  d <- Ref.read s.dead
  when (isNothing d) do
    Ref.write (Just "stopped") s.dead
    s.transport.close
