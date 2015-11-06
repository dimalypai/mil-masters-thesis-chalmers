# Functional programming language (FunLang)

> *This chapter introduces the first source programming language which has been
> designed during this work -- FunLang. It starts from the language overview
> and example programs. Then the code generation to MIL is described. Finally,
> we make conclusions regarding the implementation of the FunLang compiler
> using MIL.*

## Overview

FunLang is a rather small functional programming language. Its design is mostly
inspired by Haskell. It is statically and explicitly typed. The type system of
FunLang is based on System F. One can say that FunLang is quite similar to MIL
in many cases. In addition to the pure polymorphic lambda calculus FunLang
incorporates `do`-notation similar to the one found in Haskell and has a couple
of built-in monads. It also adds a minimal exception handling.

## FunLang by example

Similarly to the previous chapter, we will provide several examples showcasing
the main features of the language.

### Data types

As most statically typed functional programming languages, FunLang support ADTs
which can be parameterised.  One of the simplest data types is `Bool`, which
represents true and false values:

~~~
type Bool = True | False
~~~

Another canonical example is a recursive data type representing a binary tree:

~~~
type Tree A
  = Empty
  | Node A (Tree A) (Tree A)
~~~

A tree is either empty or it is a node carrying a value of type `A`, which can
have two children. Leaf nodes having a value `x` are represented as `Node x
Empty Empty`.

### Functions and expressions

Functions in FunLang have syntax rather similar to Haskell. One needs to write
a function name and its type and then a so-called *function equation*
containing the function body:

~~~
constInt : Int -> Int -> Int
constInt = \(a : Int) (b : Int) -> a;;
~~~

The idea is that potentially FunLang can be extended to have the same rich
syntax for function equations as in Haskell, meaning having multiple of them
with parameters being expressed with patterns to the left of the equal sign.
This is the reason for having two semicolons at the end of a function
definition.  Each equation would end with one semicolon and then the whole
function would end with one more. Unfortunately, multiple function equations
and pattern matching for parameters are not implemented as part of this
project.

From the example above one can also see the syntax for lambda expressions in
FunLang, which allows to have multiple parameters instead of explicitly nesting
several lambdas. Note that variable shadowing is not allowed in FunLang.

Polymorphic functions are defined with the "big lambda", which also allows to
specify several type variables at once. Below are examples of the identity
function and function composition:

~~~
id : forall A . A -> A
id = /\A . \(x : A) -> x;;

compose : forall A . forall B . forall C . (B -> C) -> (A -> B) -> A -> C
compose = /\A B C . \(f : B -> C) (g : A -> B) (x : A) -> f (g x);;
~~~

Function application is specified with juxtaposition. Square brackets are used
for type application:

~~~
one : Int
one = id [Int] 1;;
~~~

FunLang has `Unit`, `Bool` (basically, defined as in the example above), `Int`,
`Float`, `Char` and `String` as built-in types. It supports the usual infix
arithmetic operations: `+`, `-`, `*` and `/`.

### Monads

FunLang provides built-in `IO` and `State` monads, which are similars to the
corresponding monads in Haskell. The notation looks very similar to Haskell,
except that more things are explicit, like variable bindings, for example, and
every *statement* in a `do` block is terminated with a semicolon, while the
whole block is terminated with the `end` keyword.

~~~
main : IO Unit
main = do
  printInt 1;
  i : Int <- readInt;
  printInt i;
  printInt (execState [Int] [Unit] stateFun 1);
  return (evalState [Int] [Unit] stateFun 0);
end;;

stateFun : State Int Unit
stateFun = do
  i : Int <- get [Int];
  put [Int] i;
  modify [Int] (\(s : Int) -> s);
end;;
~~~

As it can be seen from the previous example, FunLang provides a number of
built-in functions for working with `IO` and `State`:

~~~
printString : String -> IO Unit
readString : IO String
printInt : Int -> IO Unit
readInt : IO Int
printFloat : Float -> IO Unit
readFloat : IO Float
evalState : forall S . forall A . State S A -> S -> A
execState : forall S . forall A . State S A -> S -> S
get : forall S . State S S
put : forall S . S -> State S Unit
modify : forall S . (S -> S) -> State S Unit
~~~

The first six functions are for reading values of several built-in types from
the standard input and printing them to the standard output. They work in the
`IO` monad. `evalState` and `execState` are for running computations in the
`State` monad. They both take one such computation and an initial state value.
`evalState` returns the value of the computation as the result and `execState`
returns the value of the state itself. Note, there is no `runState` similar to
the one in Haskell, since it would require support for tuples in FunLang, which
was omitted for simplicity. The last three functions are for working with state
inside a stateful computation. They allow to read the state value, overwrite it
and modify it with a function.

It is worth highlighting, that unfortunately, FunLang does not support
combining monads in any way.

### Exceptions

FunLang provides rather naive support for exception handling. There are two
language constructs for it: `throw` and `catch`. `throw` is an expression that
allows to raise an exception. Exceptions in FunLang do not carry additional
information, so `throw` is used standalone, without any value specified, but in
order to assign a type to it, it needs to be annotated with a type. `catch` is
a binary infix operator, that tries to evaluate an expression on the left-hand
side and if that throws an exception, it returns an expression on the
right-hand side. The following example will result in `1`, since the expression
on the right is `throw` and the handler expression is `1`:

~~~
throw [Int] catch 1
~~~

## Code generation

* General outline
    + Monads
    + Type conversions
* Examples
    + Simple expressions
    + do-blocks (IO)
    + State
    + Exceptions
* Problems

## Conclusions

* Rather straight-forward code generation, because FunLang and MIL are rather
  close to each other.

