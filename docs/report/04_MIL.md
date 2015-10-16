# Monadic Intermediate Language (MIL)

> *In this chapter we present the main part of this thesis -- Monadic
> Intermediate Language. First, its overview and some examples are given. Then
> we specify its grammar, the type system and describe representation of
> effects in detail.  Finally, some parts of the Haskell implementation are
> described.*

## Overview

As it was described in the Introduction, the main goal of this thesis was to
design a compiler IR powerful enough to be used in compilers for modern
programming languages and support reasoning about programs with effects and
their transformation for the purpose of optimisations. Monadic Intermediate
Language (MIL) is the result of this effort.

MIL is a rather small functional language. Its type system is based on System F
(or polymorphic lambda calculus) \cite{SystemF}. Effects are modelled with
monads and monad transformers are used to combine them. MIL also has a number
of additional features allowing to more easily express many features found in
modern programming languages.

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

## Grammar

In this section the full grammar of MIL is outlined. The following tokens are
used: *upper* for an upper case letter, *lower* for a lower case letter,
*alphanum* for a possibly empty sequence of alpha-numeric characters and an
underscore. The details of integer, floating point and character literals are
omitted. They are more or less what is found in most modern programming
languages. $\varepsilon$ denotes an empty grammar production.

--------------  -------  --------------------------------------------------  -------------------------------
     *program*   $\to$   *typedefs fundefs*                                  top-level definitions

    *typedefs*   $\to$   $\varepsilon$                                       type definitions

                   |     *typedef* *typedefs*

     *fundefs*   $\to$   $\varepsilon$                                       function definitions

                   |     *fundef* *fundefs*

     *typedef*   $\to$   `type` *upperid* *typevars* `=` *condefs* `;`       type definition

      *fundef*   $\to$   *lowerid* `:` *srctype* `=` *expr* `;`              function definition

     *condefs*   $\to$   *condef*                                            data constructor definitions

                   |     *condef* `|` *condefs*

      *condef*   $\to$   *upperid* *atomsrctypes*                            data constructor definition

     *srctype*   $\to$   *appsrctype*                                        application type

                   |     *appsrctype* `->` *srctype*                         function type

                   |     `forall` *typevar* `.` *srctype*                    universally-quantified type

                   |     *appsrctype* `:::` *appsrctype*                     monad cons

  *appsrctype*   $\to$   *atomsrctype*                                       atomic type

                   |     *appsrctype* *atomsrctype*                          type application

*atomsrctypes*   $\to$   $\varepsilon$                                       atomic types

                   |     *atomsrctype* *atomsrctypes*

 *atomsrctype*   $\to$   *typecon*                                           type constructor

                   |     `{` *srctypes* `}`                                  tuple type

                   |     `(` *srctype* `)`                                   parenthesised type

    *srctypes*   $\to$   *srctype*                                           source types

                   |     *srctype* `,` *srctypes*

        *expr*   $\to$   *appexpr*                                           application expression

                   |     `\` *varbinder* `->` *expr*                         lambda abstraction

                   |     `/\` *typevar* `.` *expr*                           type lambda abstraction

                   |     `let` *letbinder* `in` *expr*                       monadic bind

                   |     `return` `[` *srctype* `]` *expr*                   monadic return

                   |     `lift` `[` *srctype* `=>` *srctype* `]` *expr*      lifting

                   |     `let` `rec` *letbinders* `in` *expr*                recursive binding

                   |     `case` *expr* `of` *casealts* `end`                 case expression

     *appexpr*   $\to$   *atomexpr*                                          atomic expression

                   |     *appexpr* *atomexpr*                                application

                   |     *appexpr* `[` *srctype* `]`                         type application

    *atomexpr*   $\to$   *literal*                                           literal

                   |     *var*                                               variable

                   |     *conname*                                           data constructor name

                   |     `{` *tupleelems* `}`                                tuple

                   |     `(` *expr* `)`                                      parenthesised expression

  *letbinders*   $\to$   *letbinder*                                         let binders

                   |     *letbinder* `;` *letbinders*

   *letbinder*   $\to$   *varbinder* `<-` *expr*                             let binder

    *casealts*   $\to$   *casealt*                                           case alternatives

                   |     *casealt* *casealts*

     *casealt*   $\to$   `|` *pattern* `=>` *expr*                           case alternative

     *literal*   $\to$   `unit`                                              unit literal

                   |     *intlit*                                            integer literal

                   |     *floatlit*                                          floating point literal

                   |     *charlit*                                           character literal

  *tupleelems*   $\to$   $\varepsilon$                                       tuple elements

                   |     *expr* `,` *tupleelems*

     *pattern*   $\to$   *literal*                                           literal pattern

                   |     *varbinder*                                         variable pattern

                   |     *conname* *varbinders*                              data constructor pattern

                   |     `{` *tupelempats* `}`                               tuple pattern

                   |     `_`                                                 default pattern

 *tupelempats*   $\to$   $\varepsilon$                                       tuple element patterns

                   |     *tupelempats1*

*tupelempats1*   $\to$   *tupelempat*

                   |     *tupelempat* `,` *tupelempats1*

  *tupelempat*   $\to$   *var* `:` *srctype*                                 tuple element pattern

  *varbinders*   $\to$   $\varepsilon$                                       variable binders

                   |     *varbinder* *varbinders*

   *varbinder*   $\to$   `(` *var* `:` *srctype* `)`                         variable binder

    *typevars*   $\to$   $\varepsilon$                                       type variables

                   |     *typevar* *typevars*

     *typecon*   $\to$   *upperid*                                           type constructor

     *typevar*   $\to$   *upperid*                                           type variable

     *conname*   $\to$   *upperid*                                           data constructor name

         *var*   $\to$   *lowerid*                                           variable

     *upperid*   $\to$   *upper* *alphanum*                                  upper case identifier

     *lowerid*   $\to$   *lower* *alphanum*                                  lower case identifier

                   |     `_` *alphanum*
--------------  -------  --------------------------------------------------  -------------------------------

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

