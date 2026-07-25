# Mycroft

A thin SMT solver wrapper for PureScript, in the style of Haskell's
[`simple-smt`](https://hackage.haskell.org/package/simple-smt): a
solver subprocess kept in lockstep, s-expressions in, s-expressions
out, and a small typed veneer over SMTLib2. Explicitly *not* an SBV
port — no symbolic-value EDSL, no overloading tricks; smart
constructors and one process, small enough to hold in your head.

Named for the smarter, sedentary brother: he does no legwork — you
bring him the facts and he pronounces.

## This package

**`mycroft`** — the wrapper proper. Three layers:

- `Mycroft.SExpr` — atoms and lists, a printer, and a streaming
  parser for chunked solver stdout. Pure, golden-tested, no solver
  needed.
- `Mycroft.Solver` — the session. Spawns the solver (`z3 -in -smt2`
  by default), sets `:print-success` immediately so every command
  yields exactly one response, and framing is trivial: write one
  form, read one form. The `Transport` / `newSolverWith` seam lets an
  alternate wire (see `mycroft-z3-wasm`) reuse the whole session.
- `Mycroft.SmtLib` — the veneer: `declareEnum`, `declareConst`,
  `declareFun`, `assert`/`assertNamed`, `checkSat`, `getModel`,
  `getUnsatCore`, `push`/`pop`/`inNewScope`, term builders, and a
  sum-of-ite cardinality helper (`countEq`).

## Companion packages (their own repos, published separately)

- **[`mycroft-z3-wasm`](https://github.com/afcondon/purescript-mycroft-z3-wasm)**
  — the WASM backend: the official `z3-solver` npm package (Z3 compiled
  to WebAssembly) behind the same `Solver` interface, via `newSolverWith`
  and a persistent `Z3_eval_smtlib2_string` context. No native binary;
  Node today, browser (cross-origin-isolated) tomorrow.
- **[`baskerville-mycroft`](https://github.com/afcondon/purescript-baskerville-mycroft)**
  — the bridge to [Baskerville](https://github.com/afcondon/purescript-baskerville)'s
  oracle seam, `Baskerville.Conformance.Smt`. The solver replaces world
  *enumeration* while claim *iteration* survives; the instruments mirror
  `Baskerville.Conformance` one for one — `entailed`, `countermodel`,
  `unsound`, `gap`, `explainOr` — plus `whyEntailed`, the unsat core over
  the named observations.

## Quick taste

```purescript
main = launchAff_ do
  s <- newSolver z3Config
  x <- declareConst s "x" tInt
  assert s (app ">" [ x, intLit 5 ])
  r <- checkSat s          -- Sat
  m <- getModel s          -- x ↦ 6
  stop s
```

## Tests

```bash
spago test                         # golden + property + z3 integration
```

Needs `z3` on `PATH`. The companion repos carry their own suites:
`baskerville-mycroft` proves the agreement law (on fixtures small
enough to enumerate — the Sudoku chain puzzle — the SMT oracle and
Baskerville's counting oracle agree on `entailed` and `gap` claim for
claim, all 1458 of them), and `mycroft-z3-wasm` replays the same
integration beats with no `z3` binary at all.

## Design

The full design record — layer stack, the Cluedo demo plan, phases,
non-goals — lives in the Baskerville repo at
[`docs/smt-oracle-design.md`](../purescript-baskerville/docs/smt-oracle-design.md).
