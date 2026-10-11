# Copilot Instructions

Single-file Common Lisp library (`named-let.lisp`, system defined in `named-let.asd`) providing Scheme-style binding macros: `named-let`, `named-lambda`, `letrec`, `letrec*`, `flambda`, and a Lisp-1 style `define`. No dependencies.

## Build / test

- Load with `(ql:quickload "named-let")` (repo cloned under `~/quicklisp/local-projects/`) or `(asdf:load-system "named-let")`. No linter.
- Tests are FiveAM, in `named-let-test.lisp` (system `named-let-test`, suite `named-let-suite`). Run all: `(asdf:test-system "named-let-test")`, or `(ql:quickload "named-let-test")` then `(named-let-test:run-tests)`.
- Run one test: `(fiveam:run! 'named-let-test::letrec-mutual-recursion)`.
- Don't use CL-locked names (`loop`, `count`, ...) as named-let names in tests; SBCL package locks reject them.

## Architecture / conventions

- Package `NAMED-LET` shadows `LET` (the exported `LET` is commented out in `:export`); inside the file, use `cl:let` explicitly when the standard `LET` is needed.
- Macros are SERIES-aware: expansions look up symbols (e.g. `FUNCALL`) in the `SERIES` package if it is loaded and the symbol is fbound there, otherwise fall back to `COMMON-LISP` (see `get-symbol` in `define`). Preserve this when adding macros that emit calls to CL functions.
- Macro expansion templates use upper-case symbols (`DEFMACRO`, `LAMBDA`, `&REST`, `ARGS`) so they resolve to CL symbols regardless of the consumer's package shadowing.
- `define` stores the function in the symbol's value cell and defines a macro of the same name that expands to `(funcall (symbol-value 'name) ...)`; it only works at top level and is not a `defun` replacement.
- Any new public macro must be added to the `:export` list in the `defpackage`.
- Fasl and similar compiled artifacts are gitignored.
