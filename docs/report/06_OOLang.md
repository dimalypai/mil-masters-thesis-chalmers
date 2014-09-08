# Object-oriented programming language (OOLang)

> *In this chapter we will look at another source programming language --
> OOLang. Language overview, design principles and example programs are
> presented. Then we describe interesting parts of the type system and MIL code
> generation. At the end, conclusions regarding implementation of the OOLang
> compiler using MIL are drawn.*

## Overview

Object-oriented programming is the dominating programming paradigm in software
industry at the time of this writing.
Modern object-oriented languages incorporate many of the ideas introduced and
used for a long time in functional programming.

The design of OOLang is mainly inspired by the design of three programming
languages: C#, Haskell and Ruby. Most of the semantics is rather similar to the
one of C#. Syntax is inspired by both Haskell and Ruby syntax. Some crucial
language design ideas and decisions as well as some philosophy influence comes
from Haskell.

## OOLang by example

This section will introduce most of the OOLang features using example programs.

### Purity

### Mutability

### References

### Maybe

### Classes

## Type system highlights

* Purity
* Subtyping
* Mutability
* Maybe

## Code generation

* General outline
    + Monads
    + Type conversions
* Examples
    + Pure functions
    + Impure functions
    + Classes \cite{TAPL}
* Problems

## Conclusions

* Introduced tuples with subtyping to the MIL
* Rather complex type system increases the complexity of getting the
  type-correct MIL

