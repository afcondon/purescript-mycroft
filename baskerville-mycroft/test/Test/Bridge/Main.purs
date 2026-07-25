-- | The agreement law — the whole trust story of the SMT oracle: on
-- | fixtures small enough to enumerate, the SMT oracle and the counting
-- | oracle must agree, claim for claim. Plus the seam beats the counting
-- | oracle can't do alone: countermodels as models, unsat cores as
-- | "which observations force this".
module Test.Bridge.Main (main) where

import Prelude

import Baskerville.Conformance.Smt (Session, consistent, entailed, explainOr, gap, newSession, unsound, whyEntailed)
import Baskerville.Conformance (Refutation(..))
import Baskerville.Conformance as Conformance
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Map as Map
import Mycroft.Solver (newSolver, stop, z3Config)
import Sudoku.Board (Cell(..), Digit(..), Placement)
import Sudoku.Fixtures (blankColumnsPuzzle, chainPuzzle, gapPuzzle)
import Sudoku.Oracle (completions, oracleFor)
import Sudoku.Rules (SudokuClaim(..), SudokuKB)
import Sudoku.Solve (solve)
import Test.Assert (assertEqual, assertTrue')
import Test.Bridge.SudokuTheory (givenName, sudokuTheory)

main :: Effect Unit
main = launchAff_ do
  chainBeats
  blankColumnsBeats
  gapPuzzleBeats
  liftEffect (log "All baskerville-mycroft tests passed")

withSession :: forall a. Array Placement -> (Session SudokuClaim -> SudokuKB -> Aff a) -> Aff a
withSession givens act = do
  solver <- newSolver z3Config
  session <- newSession solver (sudokuTheory givens)
  r <- act session (solve givens)
  stop solver
  pure r

-- The chain puzzle: one world, fully solved by the engine. The
-- agreement law runs over the entire 1458-claim universe.
chainBeats :: Aff Unit
chainBeats = withSession chainPuzzle \session kb -> do
  let enum = completions 2000 chainPuzzle
  liftEffect (assertTrue' "chain enumeration is exhaustive" enum.exhaustive)
  liftEffect (assertEqual { actual: Array.length enum.grids, expected: 1 })
  let counting = oracleFor enum.grids
  let theory = sudokuTheory chainPuzzle

  liftEffect (log "Bridge: observations are consistent")
  liftEffect (assertTrue' "consistent" (consistent session))

  liftEffect (log "Bridge: agreement law — entailed, claim for claim (1458 claims)")
  mismatches <- Array.filterA
    ( \c -> do
        smt <- entailed session c
        pure (smt /= Conformance.entailed counting c)
    )
    theory.universe
  liftEffect (assertEqual { actual: mismatches, expected: [] })

  liftEffect (log "Bridge: agreement law — gap, claim for claim")
  smtGap <- gap session kb
  liftEffect
    ( assertEqual
        { actual: Array.sort smtGap
        , expected: Array.sort (Conformance.gap counting kb)
        }
    )

  liftEffect (log "Bridge: soundness — unsound is empty")
  smtUnsound <- unsound session kb
  liftEffect (assertEqual { actual: smtUnsound, expected: [] })

  liftEffect (log "Bridge: whyEntailed — a given is forced by (at least) itself")
  case Array.head chainPuzzle of
    Nothing -> liftEffect (assertTrue' "chain has givens" false)
    Just g -> do
      core <- whyEntailed session (Is g.cell g.digit)
      case core of
        Nothing -> liftEffect (assertTrue' "given is entailed" false)
        Just names -> liftEffect
          ( assertTrue' "core mentions the given itself"
              (Array.elem (givenName g) names)
          )

  liftEffect (log "Bridge: explainOr — a derived claim gets its dag")
  case Array.head chainPuzzle of
    Nothing -> pure unit
    Just g -> do
      r <- explainOr session kb (Is g.cell g.digit)
      liftEffect (assertTrue' "derived claim explains via ⊢" (isRight r))

-- Blank columns: genuinely underdetermined, so an unresolved cell's
-- placement claim must draw a countermodel — a full alternate grid.
blankColumnsBeats :: Aff Unit
blankColumnsBeats = withSession blankColumnsPuzzle \session kb -> do
  liftEffect (log "Bridge: explainOr — an open claim draws a countermodel")
  let claim = Is (Cell 0) (Digit 1)
  e <- entailed session claim
  liftEffect (assertEqual { actual: e, expected: false })
  r <- explainOr session kb claim
  case r of
    Left (Countermodel m) ->
      liftEffect (assertTrue' "countermodel is a full assignment" (not (Map.isEmpty m)))
    _ -> liftEffect (assertTrue' "expected a countermodel" false)

-- The gap puzzle: the singles engine stalls, but the truth is still
-- entailed — explainOr must say Underived, not invent a countermodel.
gapPuzzleBeats :: Aff Unit
gapPuzzleBeats = withSession gapPuzzle \session kb -> do
  liftEffect (log "Bridge: explainOr — an entailed-but-underived claim is Underived")
  let enum = completions 2000 gapPuzzle
  liftEffect (assertTrue' "gap enumeration is exhaustive" enum.exhaustive)
  let counting = oracleFor enum.grids
  case Array.head (Conformance.gap counting kb) of
    Nothing -> liftEffect (assertTrue' "gap puzzle has a gap" false)
    Just claim -> do
      r <- explainOr session kb claim
      case r of
        Left Underived -> pure unit
        _ -> liftEffect (assertTrue' "expected Underived" false)
      core <- whyEntailed session claim
      liftEffect (assertTrue' "the underived truth still has a ⊨ core" (isJust core))

isRight :: forall a b. Either a b -> Boolean
isRight = case _ of
  Right _ -> true
  Left _ -> false
