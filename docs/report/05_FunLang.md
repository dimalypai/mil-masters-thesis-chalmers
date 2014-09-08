# Functional programming language (FunLang)

> *This chapter introduces a first source programming language which has been
> designed during this work -- FunLang. It starts from a language overview and
> example programs. Then interesting parts of the type system and MIL code
> generation are described. Finally, we make conclusions regarding
> implementation of the FunLang compiler using MIL.*

## Overview

Functional programming experiences a significant increase in popularity.

Inspiration for the design (Haskell), main principles.

## FunLang by example

### Data types

~~~
type Bool = True | False

type Tree A
  = Empty
  | Node A (Tree A) (Tree A)
~~~

### Function definitions

~~~
~~~

### Polymorphism

~~~
~~~

### Pattern matching

~~~
~~~

### Monads

~~~
main : IO Unit
main = do
  printInt 1;
  (i : Int) <- readInt;
  printInt i;
end;;
~~~

### Exceptions

~~~
throw [Int] catch 1
~~~

## Type system highlights

* System F
* Monads
* Exceptions

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

