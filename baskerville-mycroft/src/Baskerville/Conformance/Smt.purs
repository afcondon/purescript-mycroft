-- | The SMT-backed oracle: the ⊨ side of Baskerville's seam when the
-- | world space is too large to enumerate. The founding restriction —
-- | the claim universe stays finite — survives; only world *enumeration*
-- | is replaced by the solver. So the instruments mirror
-- | `Baskerville.Conformance` one for one, each claim asked with one query
-- | inside a push/pop scope, plus one the counting oracle could never
-- | offer: `whyEntailed`, the unsat core over the named observations —
-- | the ⊨-side twin of ⊢'s `axiomsBehind`.
module Baskerville.Conformance.Smt
  ( SmtTheory
  , Session
  , newSession
  , consistent
  , entailed
  , countermodel
  , unsound
  , gap
  , explainOr
  , whyEntailed
  ) where

import Prelude

import Baskerville.Explain (DerivationDag, explain)
import Baskerville.Kernel (KB, isKnown, knownClaims)
import Baskerville.Conformance (Refutation(..))
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Exception (error)
import Mycroft.SExpr (SExpr)
import Mycroft.SmtLib (Model, Result(..), Term, assertNamed, checkSat, getModel, getUnsatCore, inNewScope, notT, produceModels, produceUnsatCores)
import Mycroft.SmtLib as Smt
import Mycroft.Solver (Solver, ackCommand)

-- | A client's theory: raw declarations and domain axioms (sent
-- | verbatim), an encoding of claims as formulas, the observations as
-- | *named* assertions (names are what unsat cores are made of), and
-- | the finite claim universe — still finite, still yours.
type SmtTheory claim =
  { declare :: Array SExpr
  , encode :: claim -> Term
  , observations :: Array { name :: String, term :: Term }
  , universe :: Array claim
  }

newtype Session claim = Session
  { solver :: Solver
  , theory :: SmtTheory claim
  , consistent :: Boolean
  }

-- | Load a theory into a solver: options, declarations, named
-- | observations, and one up-front consistency check — the analogue of
-- | asking whether any world survives the observations.
newSession :: forall claim. Solver -> SmtTheory claim -> Aff (Session claim)
newSession solver theory = do
  produceModels solver
  produceUnsatCores solver
  for_ theory.declare (ackCommand solver)
  for_ theory.observations \o -> assertNamed solver o.name o.term
  ok <- checkSat solver >>= case _ of
    Sat -> pure true
    Unsat -> pure false
    Unknown -> throwError (error "newSession: solver returned unknown on the observations")
  pure (Session { solver, theory, consistent: ok })

consistent :: forall claim. Session claim -> Boolean
consistent (Session s) = s.consistent

-- | True in every world consistent with the observations. Mirrors the
-- | counting oracle's stance on contradiction: an empty world set
-- | entails nothing, rather than everything.
entailed :: forall claim. Session claim -> claim -> Aff Boolean
entailed (Session s) claim =
  if not s.consistent then pure false
  else inNewScope s.solver do
    Smt.assert s.solver (notT (s.theory.encode claim))
    checkSat s.solver >>= case _ of
      Unsat -> pure true
      Sat -> pure false
      Unknown -> throwError (error "entailed: solver returned unknown")

-- | A world where the claim fails, if one survives — as a solver model.
countermodel :: forall claim. Session claim -> claim -> Aff (Maybe Model)
countermodel (Session s) claim = inNewScope s.solver do
  Smt.assert s.solver (notT (s.theory.encode claim))
  checkSat s.solver >>= case _ of
    Sat -> Just <$> getModel s.solver
    Unsat -> pure Nothing
    Unknown -> throwError (error "countermodel: solver returned unknown")

-- | Claims the engine believes that the oracle refutes. Soundness
-- | means this is empty, on every stream, at every position.
unsound
  :: forall residue claim rule axiom
   . Ord claim
  => Session claim
  -> KB residue claim rule axiom
  -> Aff (Array claim)
unsound session kb =
  Array.filterA (map not <<< entailed session)
    (Set.toUnfoldable (knownClaims kb))

-- | Claims true in every world that the engine has not derived — the
-- | completeness gap, now measurable where enumeration can't reach.
gap
  :: forall residue claim rule axiom
   . Ord claim
  => Session claim
  -> KB residue claim rule axiom
  -> Aff (Array claim)
gap session@(Session s) kb =
  Array.filterA
    (\c -> if isKnown c kb then pure false else entailed session c)
    s.theory.universe

-- | The seam: ⊢ if the engine can, else what ⊨ has to say about it.
explainOr
  :: forall residue claim rule axiom
   . Ord claim
  => Session claim
  -> KB residue claim rule axiom
  -> claim
  -> Aff (Either (Refutation Model) (DerivationDag claim rule axiom))
explainOr session@(Session s) kb claim = case explain claim kb of
  Just dag -> pure (Right dag)
  Nothing
    | not s.consistent -> pure (Left NoWorlds)
    | otherwise -> countermodel session claim <#> case _ of
        Just m -> Left (Countermodel m)
        Nothing -> Left Underived

-- | Which observations force this claim: the unsat core over the named
-- | observations when the negated claim is refuted. `Nothing` when the
-- | claim isn't entailed at all.
whyEntailed :: forall claim. Session claim -> claim -> Aff (Maybe (Array String))
whyEntailed (Session s) claim =
  if not s.consistent then pure Nothing
  else inNewScope s.solver do
    Smt.assert s.solver (notT (s.theory.encode claim))
    checkSat s.solver >>= case _ of
      Unsat -> Just <$> getUnsatCore s.solver
      Sat -> pure Nothing
      Unknown -> throwError (error "whyEntailed: solver returned unknown")
