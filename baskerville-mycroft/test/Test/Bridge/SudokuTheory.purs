-- | Sudoku as an `SmtTheory`: one Int constant per cell, range and
-- | alldifferent axioms, the givens as named observations. The same
-- | domain the counting oracle enumerates — which is the point: two
-- | independent ⊨ implementations that must agree.
module Test.Bridge.SudokuTheory
  ( sudokuTheory
  , givenName
  ) where

import Prelude

import Baskerville.Conformance.Smt (SmtTheory)
import Mycroft.SExpr (SExpr(..))
import Mycroft.SmtLib (Term(..), andT, app, distinct, eqT, intLit, notT, var)
import Sudoku.Board (Cell(..), Digit(..), Placement, allCells, allDigits, units)
import Sudoku.Rules (SudokuClaim(..))

cellVar :: Cell -> Term
cellVar (Cell i) = var ("cell" <> show i)

givenName :: Placement -> String
givenName { cell: Cell i, digit: Digit d } = "g" <> show i <> "d" <> show d

assertCmd :: Term -> SExpr
assertCmd (Term t) = List [ Atom "assert", t ]

declareCmd :: Cell -> SExpr
declareCmd (Cell i) = List [ Atom "declare-const", Atom ("cell" <> show i), Atom "Int" ]

sudokuTheory :: Array Placement -> SmtTheory SudokuClaim
sudokuTheory givens =
  { declare:
      map declareCmd allCells
        <> map (assertCmd <<< inRange) allCells
        <> map (assertCmd <<< distinct <<< map cellVar) units
  , encode
  , observations: givens <#> \g ->
      { name: givenName g, term: encode (Is g.cell g.digit) }
  , universe: do
      c <- allCells
      d <- allDigits
      [ Is c d, Not c d ]
  }
  where
  inRange c = andT
    [ app "<=" [ intLit 1, cellVar c ]
    , app "<=" [ cellVar c, intLit 9 ]
    ]

  encode = case _ of
    Is c (Digit d) -> eqT (cellVar c) (intLit d)
    Not c (Digit d) -> notT (eqT (cellVar c) (intLit d))
