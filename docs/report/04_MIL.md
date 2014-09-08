# Monadic Intermediate Language (MIL)

> *In this chapter we present the main part of this thesis -- Monadic
> Intermediate Language. First, its overview and some examples are given. Then
> we specify its type system and representation of effects. Finally, some parts
> of the Haskell implementation are described.*

## Overview

General ideas and approach behind the design of MIL.

* System F
* Data types
* Pattern matching
* Tuples with subtyping (for OO)

## MIL by example

### Data types

~~~
type Bool = True | False;

type List A = Nil | Cons A (List A);
~~~

### Bind and Return

~~~
let (c : Char) <- read_char in
  return [IO] c
~~~

### Lifting

~~~
lift [IO -> State ::: IO] unit
~~~

### Pattern matching

~~~
case x of
  | True => 0
  | False => 1
end
~~~

### Recursive binding

### Built-in functions

~~~
read_char : IO Char

print_char : Char -> IO Unit
~~~

## Type system

Most of the type rules.
\cite{TAPL}

## Effects

* Special section devoted to combining monads with :::
* Monad transformers

## Haskell implementation

Since one of the aims of this thesis was to produce a programming framework for
working with the designed monadic intermediate representation, here we will
outline some of the implementation details. MIL is implemented in Haskell.

* AST
* Type checker?
* API

Implementation details specific to optimisations are presented in Chapter\ \ref{chap:opt}.

