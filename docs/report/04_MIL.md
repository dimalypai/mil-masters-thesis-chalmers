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

MIL is a rather small strict functional language. Its type system is based on
System F (or polymorphic lambda calculus) \cite{SystemF}. Effects are modelled
with monads and monad transformers are used to combine them. MIL also has a
number of additional features allowing to more easily express many features
found in modern programming languages.

## MIL by example

In this section we will look at some examples to get the feel for MIL syntax.
Some common terminology will also be established.

### Data types

MIL supports simple algebraic data types (ADTs), found in many statically typed
functional programming languages. Here are the examples of the usual
definitions of `Bool` and `List` data types:

~~~
type Bool = True | False;

type List A = Nil | Cons A (List A);
~~~

Here, `Bool` and `List` are also called *type constructors*. `List` type is
*parameterised*, it takes one *type parameter* `A`, which can be instantiated
with any type. We say that type `Bool` has *kind* `*` (star) and type `List`
has kind `* => *` meaning that it takes a type and returns a type (hence the
name type constructor). Kinds can be thought of as *types of types*. Type
`Bool` has two *data constructors*: `True` and `False`.  `List` also has two
data constructors: `Nil` for empty list and `Cons` which carries a value of
type `A` and a list. This means that `List` data type is *recursive*.

Every data constructor can be used as a function. For example, `True` has just
type `Bool` (because it does not have any other data associated with it, so it
is just a value of type `Bool`), `Cons` has type `forall A . A -> List A ->
List A`, meaning that for any type `A` (`A` has kind `*`) it can be applied to
a value of type `A` and a value of type `List A` producing `List A`. Data
constructors can also be partially applied.

### Functions and expressions

Simple functions in MIL can be definied as follows:

~~~
intId : Int -> Int = \(i : Int) -> i;
~~~

MIL is *explicitly typed* (there is no type inference like in Haskell or ML),
so the types of functions and variable binders must be specified. In this
example, `intId` is the identity function for integers, it has the type `Int ->
Int` (it takes an integer and returns an integer). The body of the function is
a lambda expression.

Polymorphic functions look like this:

~~~
id : forall A . A -> A = /\A . \(x : A) -> x;
~~~

This a polymorphic version of the identity function, which works on any type.
It has a *universally-quantified* type (using `forall` keyword). The body of
the function is the so-called "big lambda" or "type lambda", which introduces a
*type variable* `A` (and `forall A` in the type) and then a lambda expression,
where the variable binder for `x` uses the type variable `A`.

Functions are applied using juxtaposition (placing arguments next to functions
using a whitespace). Square brackets are used for *type application*
(instantiating a type variable in a `forall` type). For example, applying `id`
to `True`:

~~~
true : Bool = id [Bool] True;
~~~

Regarding expressions, MIL supports the ones shown above (data constructors,
variables, applications, type applications, lambda and type lambda expressions)
and also integer, floating point and character literals as well as tuples (with
*width and depth subtyping*, see "Type system" section in this chapter and
\cite{Pierce}). These are not all kinds of expressions, some of them will be
covered in separate sections.

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

MIL supports simple *pattern matching* with `case` expression. An expression
which we match on (*scrutinee*) is evaluated and checked against patterns in
the order they are written. For the first successful match the right-hand side
(after `=>`) of the corresponding *case alternative* is the result of the
`case` expression.

~~~
case x of
  | True => 0
  | False => 1
end
~~~

Possible patterns are literals, variable binders, data constructors (with
variable binders for their data elements), tuples (with variable binders for
elements) and the *default pattern* (underscore), which matches anything.
Patterns cannot be nested.

### Recursive binding

Recursive bindings inside a function can be introduced using `let rec`
expression. It can specify several bindings (which can be mutually recursive)
at once. The result of `let rec` expression is a value of the *body expression*
(after `in`).

~~~
let rec
  (isEven : Int -> Bool) <- \(i : Int) -> ... isOdd ...;
  (isOdd : Int -> Bool) <- \(i : Int) -> ... isEven ...
in isEven 4
~~~

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

