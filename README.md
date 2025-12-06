# alicorn0.rs

Alicorn bootstrap interpreter but it's rust this time  
Taking this opportunity to simplify some decisions. No metalanguage, less different types of terms, pattern-wishcast to allow strict values to be a subtype of flex values.

## Status

- [x] **Format** - Lexer + listifier
  - [x] Indentation-based grouping
  - [x] Paren/bracket/brace lists with comma/semicolon semantics
  - [x] Function call syntax `f(x)` → `(f x)`
  - [ ] Mixfix operators (2 tests ignored)

- [x] **Terms** - Core term types
  - [x] Inferrable (pre-typecheck AST)
  - [x] Elaborated (core calculus)
  - [x] FlexValue/StrictValue (runtime values via pattern-wishcast)

- [x] **Basic pipeline** - Source → parse → elaborate → evaluate
  - [x] Literals, let-in, lambdas, application
  - [x] K combinator works end-to-end

- [ ] **Operatives** - Syntax transformers
  - [x] `let`, `forall`, `:`, `type`, `type_`
  - [x] `lambda` (smart: handles both `lambda x` and `lambda (x : T)`)
  - [x] `lambda_single`, `lambda_implicit`, `lambda_curry`
  - [x] `wrap`/`unwrap`/`wrapped` (host type wrapping)
  - [x] `intrinsic` (named intrinsics with Lua compat fallback)
  - [ ] `lambda_annotated`, `lambda-prog`
  - [ ] `mk`, `switch`, `enum` (data construction)
  - [ ] `record`, `record-of`

- [ ] **Type system**
  - [x] Basic type inference skeleton
  - [ ] Constraint solver (metavariables, unification)
  - [ ] Subtyping (ranges, variance, union/intersection)
  - [ ] Implicit argument inference

- [ ] **Error handling**
  - [x] Basic error messages with operative hints
  - [ ] Source span tracking in errors

- [ ] **Run prelude.alc** - The ultimate integration test
  - Gets past intrinsics, now blocked on type inference

## Commands

```bash
# Run a file
cargo run --bin alicorn -- testcases/bootstrap.alc

# Evaluate an expression
cargo run --bin alicorn -- -e "(fn (x : Number) x) 42"

# Debug listify output
cargo run --bin format-harness -- -f myfile.alc -c
```
