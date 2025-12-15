# Coeffect-Sandbox

It is often the case that the programs we write expect some kind of *resource* to be available. This could be some hardware device such as a GPS, a sensor, or a clock, or it could be something more advanced like a variable with some sensitivity/security value indicating when it is allowed to be used.

While effect type systems have been in circulation for quite some time now (these track what a program *does* to its environment), [coeffects](https://tomasp.net/coeffects/) are a recent innovation which track what a program *needs* from an environment. They are in essence the *dual* of effects. 

This repository provide sample implementations (in OCaml) of programming languages utilizing coeffect type systems for some interesting use-cases. Currently the only language implementations in this repository are:
 - The Lambda Calculus
 - Implicit Parameters Coeffect

### Lambda Calculus

This is your standard untyped lambda calculus extended with numbers. Nothing interesting to discuss here. Expressions are either numbers, named bindings, lambda abstractions, or applications. Values are numbers and lambda abstractions.

### Implicit Parameters Coeffect

This is the first example of a coeffect system that allows for dynamic binding of special variables that are prefixed with `?` (e.g. `?x`, `?var`). This coeffect system tracks what implicit parameters are necessary in order to evaluate an expression. This may be due to the expression directly using the implicit parameters or a function requires them as latent coeffects. For the type system, the coeffect context is flat (meaning the context applies globally rather than per-variable) and is implemented using a mapping from implicit parameters to their type. In the semantics, this is implemented using a mapping from implicit parameters to their value that is propagated throughout the program and updated accordingly based on the lexical and dynamic binding rules.

Programmers can request variables to be dynamically bound with the following *optional* syntax after the parameter list for a function or lambda abstraction: `{ ?x1: <typ>, ?x2: <typ>, ... , ?xn: <typ> }`. This is a list of implicit parameters that will act as latent coeffects (these will be required to be bound dynamically when the function is called). Any implicit parameters used in the body of an expression that are not present in the list will be lexically bound. This provides the programmer with the flexibility of dynamic binding in a controlled manner (the dynamic binding is encoded in the type), otherwise the binding semantics work as expected.

Implicit parameters are bound to a value using the `letdyn ?param = ... in ...` syntax. In the type system, this removes any requirements of `?param` from the coeffect context in the nested expression. In the semantics, this will introduce a new implicit parameter `?param` with the specified value for the duration of the nested expression.

An interesting thing about the type system for implicit parameters is the circular relationship between the `(ABS)` rule and the `(PARAM)` rule:
```
  (ABS)      Γ,x:t1 @ r ^ s ⊢ e:t2
        ------------------------------
        Γ @ r ⊢ λx:t1.e : t1 -{s}-> t2

  (PARAM)   
        ------------------------------
        Γ @ { ?param: t } ⊢ ?param : t
```
Namely, the `(PARAM)` rule requires the flat context to contain `?param` typed as `t` and the `(ABS)` rule needs to know what `s` is (the latent implicit params). In essence, it needs to typecheck `e` to get `s` (since this is where the implicit params are used), but it needs `s` to typecheck `e`.

To circumvent this without requiring additional context from the syntax, we would use a constraint-based typechecker, which tracks constraints for the coeffects rather than concrete instantiations.

In this sandbox however, we require the programmer to explicitly provide `s` for lambda abstractions using the aforementioned `{ ... }` syntax after the parameter list. These will be the latent implicit parameters annotated in the type of the function. This also allows for some interesting scenarios where the programmer can explicitly state when they want to require the latent coeffect (for nested functions).

In the sample program, you will see the following interesting cases:

---
`foo` will be typed as: `int { ?x: int } -> int`. *NOTE: Type constraints after the `fun` decl are optional, see grammar for syntax.*
```py
fun foo (y: int) { ?x: int }
    : int { ?x: int } -> int 
    = ?x + y
```
It has an implicit parameter `?x` requirement, effective when applying the first and only argument. This is a *latent* coeffect.

---
Consider the case for currying functions: `bar` will be typed as: `int -> int { ?x: int } -> int` and `new1` will be typed as `int { ?x: int } -> int`.
```py
fun bar (y: int) (z: int) { ?x: int }
    : int -> int { ?x: int } -> int 
    = ?x + y + z

val new1 
   : int { ?x: int } -> int
   = bar 420
```
The latent coeffects will be encoded in the last arrow. Because of this implicit parameter bindings are always written after the parameter list of of curried function declarations.

---
Consider the case for nested functions: `baz` will be typed as: `int { ?x: int } -> int -> int` and `new2` will be typed as: `int -> int`.
```py
fun baz (y: int) {?x: int}
   : int { ?x: int } -> int -> int
   = \(z: int) -> ?x + y + z

val new2 
   : int -> int
   = letdyn ?x = 1 in baz 1
```
This might seem more or less the same as the previous case. But this time, the programmer can force the latent coeffect to occur earlier in the function type. The inner function will bind the param ?x lexically.

---
Consider the case for when we want full dynamic binding: `bam` will be typed as: `int { ?x: int } -> int { ?x: int } -> int` and `new3` will be typed as: `int { ?x: int } -> int`.
```py
fun bam (y: int) { ?x: int }
   : int { ?x: int } -> int { ?x: int } -> int
   = (\(x: int) -> \(z: int) { ?x: int } -> x + ?x + y + z) ?x

val new3
   : int { ?x: int } -> int
   = letdyn ?x = 1 in bam 1
```
Here the latent coeffect applies for every nested call.

Notes:
 - There is no subtyping yet for these coeffects, but this is currently being worked on. Informally, we should treat a type `int -> int` as being a subtype of `int { ?x: int } -> int`. In otherwords, we are allowed to not use all implicit parameter requirements.
 - Other than for `fun`, `val` declarations or `letdyn` bindings, there is no type inference.
 - This language is mostly pedagogical and is *not turing complete*. It is missing a substantial amount of features to be practical (e.g. recursive types, polymorphism, more operations and expressions).

## Building

The following steps rely on the dune build system:
1) Clone the repository
2) Create a local opam switch in the root of the project: `opam switch create .`
3) Set the opam environment `eval $(opam env)`
4) Run `dune build` to create executables or `dune runtest` to run tests
5) Run `dune exec -- Coeffect-Sandbox` to see usage and/or run the interpreter