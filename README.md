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
- [x] step 2
  - [x] figure out step 2
  - [x] (!) syntax issue with braces for function calls, f(x) has different meaning in rt and ct. spaces?
    - related to the unsolved issue about RTExprs as values
  - [x] recursion
  - [x] builtins
  - [x] runtime functions POC
  - [x] fix precedence issue (`builtins.fix (self: 0)` fails)
- [ ] step 3
  - [ ] figure out step 3
  - [x] recursion but properly
    - implementation should be pretty easy, but it would require turning `let` into more than syntactic sugar
- [ ] eventually
  - [ ] RTExpr values/syntax?
    - in `plusEqual = l: r: {l = l+r;}` `r` should be able to be an expression without this having to be a bona fide function
    - maybe it's just a matter of making syntax slightly more explicit
      - `fn @comptimeArg (rta + rtb)`?
    - blocks _are_ RTExpr values
      - we can already do this with `plusEqual x {break 3 + 4;}`, so making the block expression syntax lighter (i.e. rust-style `{3+4}`) might be sufficient
  - [ ] annotate functions with their stack trace/closest binding
  - [ ] booleans, enums, atoms
    - [ ] unscoped/global atoms?
      - would mean the compiler collects them and assigns unique int ids to them
        - for this to work across modules, it could/would need to be a hash of its name
        - not sure if that's a good idea though
      - similar to how zig does error values
    - [ ] are enums just namespaced atoms?
      - _would_ be numbered in ascending order
      - still need global atoms?
    - [ ] are booleans just enums?
  - [ ] `builtins.trace`
    - could just use Haskell's `trace` for now
    - requires strings
  - [ ] function naming and deduplication
    - ideally `let someFn = [..]: {..}` would actually get the `someFn` symbol, but that might become tricky if it's behind a ct argument
    - Zig has memoization, but our functions can come from weirder places, and can be anonymous
    - maybe we perform a pass before evaluation where we find every function expression and name it after it's place in the expression
      - if it's behind arguments, include the applied arguments in the function name
    - the easy way out would be hashing function definitions
  - [ ] comptime stack traces
  - [ ] `builtins.error`
    - strings
  - [ ] black holes
  - [ ] negative lit
  - [ ] proper keyword parsing
    - [x] currently `truee` parses to `true e`
  - [ ] print `VClosure` using let-bindings?
  - [ ] (key)word parsing backtracks too much
  - [ ] custom pretty-printing
    - [ ] own type class
      - multi-line, single line
  - [x] turn the test cases into test cases so we can focus them
  - [ ] list builtins
    - [ ] concatenation
    - [ ] destructing
    - [ ] does map need to be a builtin?
  - [ ] nix-style `{ foo.bar: 4 }`?
  - [ ] proper megaparsec errors for unexpected keywords
    - highlight the entire word, say what was expected, etc.
  - [ ] Can type unification just be an equality check?
  - [ ] Does an attribute set of programs share RT stuff between itself?
    - may be just a question of naming them well
      - if functions are hashed this is free
  - [ ] check whether all rejected words are actually keywords
    - at a later point just make sure we didn't leave anything in there that we don't use
  - [ ] think about runtime function call syntax
    - it's not lists
  - [ ] rename stuff
    - [ ] lazy evaluation terminology
  - [ ] imports
    - [ ] check for unbound variables to avoid capture issues
  - [ ] warnings
  - [ ] have subcommands in the executable for the building and the repl
  - [ ] benchmarks
    - [ ] use Text instead of String
  - [ ] inherit from set, inherit multiple
  - [ ] `where` expressions?
  - [ ] remove microlens dependency?
