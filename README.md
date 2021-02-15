# alloy

## TODO
- [x] step 1
  - [x] Closure scope contains ThunkIDs instead of values
  - [x] arithmetic expressions
  - [x] attribute sets
  - [x] field accessor parsing
  - [x] bugs:
    - [x] `let id = x:x; x = 4 in id x` diverges
    - [x] `let x = 4; in {a = x;}` gives unbound variable, `let x=4; in x` works
  - [x] let bindings
  - [x] syntax rework
  - [x] remove lens dependency?
- [ ] step 2
  - [ ] figure out step 2
  - [x] recursion
  - [ ] builtins
    - [ ] error
      - [ ] strings
- [ ] eventually
  - [ ] recursion but really
  - [ ] booleans
    - [ ] are they just atoms? would be weird to have 0 not equal false though
  - [ ] trace
  - [ ] stack traces
  - [ ] list builtins
  - [ ] nix-style { foo.bar = 4; }
  - [ ] proper megaparsec errors for unexpected keywords
  - [ ] check whether all rejected words are actually keywords
  - [ ] think about runtime function call syntax
    - [ ] it's not lists
  - [ ] rename stuff
    - [ ] lazy evaluation terminology
    - [ ] code closure
  - [ ] imports
    - [ ] check for unbound variables to avoid capture issues
  - [ ] warnings
  - [x] fix precedence issue (`builtins.fix (self: 0)` fails)
  - [ ] benchmarks
    - [ ] use Text instead of String
  - [ ] inherit from set, inherit multiple
  - [ ] performance stuff
  - [ ] (!) syntax issue with braces for function calls, f(x) has different meaning in rt and ct. spaces?
  - [ ] `where`?
  - [ ] design questions
    - [ ] Everything
    - [ ] Atoms
  - [ ] remove microlens dependency?
