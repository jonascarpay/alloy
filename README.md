# alloy

Alloy[^1] is a programming language experiment that combines the fine-grained control of a systems programming language with the expressivity of a functional programming language.
It is best thought of as a combination of two small sublanguages: a high-level declarative macro language, and a low-level imperative runtime language.

| Language A | Language B |
| --- | --- |
| High-level | Low-level |
| Declarative | Imperative |
| Functional | Procedural |
| Compile time | Runtime |
| Lazy | Strict |
| Pure | Impure |
| Dynamic typing | Static typing |
| Static scoping | Dynamic scoping |
| Garbage-collected | Manual memory management |

This project is still in very early development.
The syntax is very preliminary, there are no syscalls, and there is no codegen beyond the internal intermediate representation.

### Types example

This following example shows off how to write type-based machinery in the compile-time language.
- `V3` is a function that takes a type as its argument and returns a three-dimensional vector struct
- `Vert` is a struct consisting of three vectors
- `sizeOf` is a manual implementation of C's `sizeof`, it takes a type and gives its size
- `zero` takes a type and gives a value for it, consisting entirely of zeroes.
- The final value is a function definition that takes no arguments and returns the size of `x` as an integer

```
with builtins;
with types;
with (import "lib/lib.ay");

let

  V3 t = struct { x: t, y: t, z: t };

  Vert = struct {
    pos: V3 double,
    nor: V3 double,
    tex: V3 int,
  };

  sizeOf t = matchType t {
    int: 4,
    double: 8,
    void: 0,
    struct: fields: sum (map sizeOf (attrValues fields)),
  };

  zero t = matchType t {
    int: 0,
    double: 0,
    struct: fields: mapAttrs zero fields,
  };

in
[] -> int {
  var x: Vert = zero Vert;
  var y: int = sizeOf (typeOf x);
  return y;
}
```

### Recursion example

### Control flow example

### Continuation example

[^1] working title, suggestions are welcome
