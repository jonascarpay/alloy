# alloy

Alloy (working title, suggestions welcome) is a programming language experiment that combines the fine-grained control of a systems programming language with the expressivity of a functional programming language.
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
The syntax is preliminary, there are no syscalls, and there is no codegen beyond the internal intermediate representation.
