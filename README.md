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
- [ ] step 2
  - [ ] figure out step 2
- [ ] step n
  - [ ] builtins
    - [ ] error
  - [ ] imports
- [ ] eventually
  - [ ] recursion
  - [ ] trace
  - [x] remove lens dependency?
  - [x] remove microlens dependency?

## Design Questions
- [ ] Everything
- [ ] Atoms
