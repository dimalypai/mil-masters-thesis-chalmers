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
\cite{TAPL}). These are not all kinds of expressions, some of them will be
covered in separate sections.

### Bind and Return

The bread and butter of MIL is monadic $bind$ and $return$. The following
example uses $bind$ (`let ... in` expression) to bind the result of `read_char`
built-in function to the variable `c` and then return it in the `IO` monad
($return$ needs to be annotated with a monad):

~~~
let (c : Char) <- read_char
in return [IO] c
~~~

### Lifting

Another crucial piece is the monad transformer $lift$ operation. The following
example demostrates how to lift a computation in the `IO` monad into a
combination of `State` and `IO` monads:

~~~
lift [IO => State ::: IO] return [IO] unit
~~~

The typing rules and details of monads combination will be covered in the "Type
system" section.

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

### Built-in data types and functions

MIL has several built-in types: `Unit` (which has only one value, which is the
`unit` literal), `Bool` (defined as in one of the data type examples earlier),
`Int` (for arbitrary integers), `Float` (for double precision floating point
numbers), `Char` (for characters) and `Ref` (for reference cells).

MIL also provides a number of built-in functions.  Here is the list with most
of them and their types:

~~~
read_char : IO Char
print_char : Char -> IO Unit
new_ref : forall A . A -> State (Ref A)
read_ref : forall A . Ref A -> State A
write_ref : forall A . Ref A -> A -> State Unit
throw_error : forall E . forall A . E -> Error E A
add_int : Int -> Int -> Int
add_float : Float -> Float -> Float
sub_int : Int -> Int -> Int
sub_float : Float -> Float -> Float
mul_int : Int -> Int -> Int
mul_float : Float -> Float -> Float
div_int : Int -> Int -> Error Unit Int
div_float : Float -> Float -> Error Unit Float
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

This section is devoted to the details of the MIL type system and the effect
representation. We will not provide a formal definition of the full type
system, but rather focus on the crucial parts. For example, data types and
functions will be omitted, since they are pretty straight-forward and similar
to the typing in many other functional languages.

Variables get their types from the type environment $\Gamma$:

\infrule[T-Var]{x : T \in \Gamma}{\Gamma \vdash x : T}

Lambda abstraction is what is found in most functional languages, but note that
it does not allow variable shadowing:

\infrule[T-Abs]{x \notin \Gamma \andalso \Gamma, x : T_1 \vdash e : T_2}{\Gamma \vdash \lambda (x : T_1) \to e : T_1 \to T_2}

Function application also has a classical shape, but the argument and parameter
types do not have to be the same, rather they need to satisfy the
$isCompatible$ relation, which we will define after all the rules together
with other relations that are used:

\infrule[T-App]{\Gamma \vdash e_1 : T_1 \to T_2 \andalso \Gamma \vdash e_2 : T_1' \andalso isCompatible(T_1', T_1)}{\Gamma \vdash e_1\ e_2 : T_2}

Type abstraction and type application have typical System F rules, but again,
type variable shadowing is not allowed:

\infrule[T-TAbs]{X \notin \Gamma \andalso \Gamma, X \vdash e : T}{\Gamma \vdash \Lambda X\ .\ e : forall\ X\ .\ T}

\infrule[T-TApp]{\Gamma \vdash e_1 : forall\ X\ .\ T_{1}}{\Gamma \vdash e_1\ [T_2] : [X \mapsto T_2]T_{1}}

The next four rules specify how data constructors get their types:

\infrule[T-ConstrNil]{\Gamma \vdash C \in T}{\Gamma \vdash C : T}

\infrule[T-Constr]{\Gamma \vdash C\ T_1 ... T_n \in T}{\Gamma \vdash C : T_1 \to ... \to T_n \to T}

\infrule[T-ConstrNilTypeVars]{\Gamma \vdash C \in T\ X_1 ... X_n}{\Gamma \vdash C : T\ X_1 ... X_n}

\infrule[T-ConstrTypeVars]{\Gamma \vdash C\ T_1 ... T_n \in T\ X_1 ... X_n}{\Gamma \vdash C : forall\ X_1 .\ ...\ . forall\ X_n . T_1 \to ... \to T_n \to T\ X_1 ... X_n}

Typing of tuples is specified with the following two rules:

\infax[T-EmptyTuple]{\Gamma \vdash \{ \} : \{ \}}

\infrule[T-Tuple]{for\ each\ i \andalso \Gamma \vdash e_i : T_i}{\Gamma \vdash \{ e_{i = 1..n} \} : \{ T_{i = 1..n} \}}

Probably the most important typing rule is the one for monadic $bind$:

\infrule[T-Let]{x \notin \Gamma \andalso \Gamma \vdash e_1 : M_1\ T_1' \andalso \Gamma, x : T_1 \vdash e_2 : M_2\ T_2 \andalso T_1 \equiv_\alpha T_1' \\ isMonad(M_1) \andalso isMonad(M_2) \andalso isCompatibleMonad(M_2, M_1)}{\Gamma \vdash let\ (x : T_1) \gets e_1\ in\ e_2 : highestEffectMonad(M_1, M_2)\ T_2}

The crucial parts are that both $e_1$ and $e_2$ should have monadic types.
These two monads have to satisfy the $isCompatibleMonad$ relation. The type
specified in the variable binder ($T_1$) and the result type of $e_1$ ($T_1'$)
must be alpha-equivalent (equivalent modulo renaming of type variables). $e_2$
gets the bound variable in scope. The monad for the type of the whole bind
expression is chosen using the $highestEffectMonad$ function. The rule also
specifies that $bind$ does not allow variable shadowing.

Monadic $return$ typing rule is quite minimal. Its type is the monadic type
$return$ is annotated with applied to the type of the expression that is being
returned:

\infrule[T-Return]{isMonad(M) \andalso \Gamma \vdash e : T}{\Gamma \vdash return\ [M]\ e : M\ T}

The next rule specifies the $lift$ operation. $lift$ is annotated with two
monads, we lift a computation in monad $M_1$ to monad $M_2$. $M_1$ has to be a
*suffix* of $M_2$. The monad of the expression $e$ that we are lifting ($M_1'$)
and monad $M_1$ should satisfy the non-commutative version of the
$isCompatibleMonad$ relation:

\infrule[T-Lift]{\Gamma \vdash e : M_1'\ T \andalso isMonad(M_1') \andalso isMonad(M_1) \andalso isMonad(M_2) \\ isCompatibleMonadNotCommut(M_1', M_1) \andalso M_1\ isMonadSuffixOf\ M_2}{\Gamma \vdash lift\ [M_1 \Rightarrow M_2]\ e : M_2\ T}

The last rule describes `let rec` expression:

\infrule[T-LetRec]{for\ each\ i \andalso x_i \notin \Gamma \andalso \Gamma, (x_j : T_j)_{j = 1..n} \vdash e_i : T_i' \andalso \Gamma, (x_j : T_j)_{j = 1..n} \vdash e : T \\ T_i \equiv_\alpha T_i'}{\Gamma \vdash let\ rec\ (x_i : T_i) \gets e_i;_{i = 1..n}\ in\ e : T}

Similarly to other expressions which introduce variables, `let rec` does not
allow variable shadowing. All binding expressions ($e_i$) are checked with all
the variable binders in scope, so that they can be mutually recursive. The type
of a binding expression should be alpha-equivalent to the type specified in the
corresponding variable binder.

Typing of `case` expressions is quite involved when written using judgement
rules, so we will omit it here. It can be informally described as the
following: pattern types should match the type of a scrutinee, every case
alternative is checked separately, variables bound in patterns are in scope for
the corresponding alternative. The types of expressions in alternatives should
satisfy the $isCompatible$ relation. The effect of the case expression is
chosen using the $highestEffectMonad$ function among all the alternatives.

Next, we will look at the relations and helper functions used in the typing
rules above and define what are the possible monads and how they are combined
in MIL.

The first one is the $isCompatible$ relation. TODO

\infrule{isCompatibleMonadNotCommut(M_1, M_2)}{isCompatible(M_1, M_2)}
TODO
\infrule{T_1 <: T_2}{isCompatible(T_1, T_2)}
~~~
isCompatibleWith (TyMonad mt1) (TyMonad mt2) =
  mt1 `isCompatibleMonadWithNotCommut` mt2
isCompatibleWith (TyApp mt1@(TyMonad _) t1@(TyMonad {})) (TyApp mt2@(TyMonad _) t2@(TyMonad {})) =
  (mt1 `isCompatibleWith` mt2) && (t1 `alphaEq` t2)
isCompatibleWith (TyApp mt1@(TyMonad _) t1) (TyApp mt2@(TyMonad _) t2) =
  (mt1 `isCompatibleWith` mt2) && (t1 `isCompatibleWith` t2)
isCompatibleWith (TyArrow t11 t12) (TyArrow t21 t22) =
  (t21 `isCompatibleWith` t11) && (t12 `isCompatibleWith` t22)
isCompatibleWith (TyForAll tv1 t1) (TyForAll tv2 t2) =
  t1 `isCompatibleWith` ((tv2, TyVar tv1) `substTypeIn` t2)
isCompatibleWith t1 t2 = t1 `isSubTypeOf` t2
~~~

There are four built-in monads in MIL: $Id$ (identity), $State$, $IO$ (for
input/output) and $Error$. They all satisfy the $isSingleMonad$ relation. Note
that the $Error$ type has an additional type parameter for the type of error
values.

\infax{isSingleMonad(Id)}
\infax{isSingleMonad(State)}
\infax{isSingleMonad(IO)}
\infax{isSingleMonad(Error\ T)}

$isMonad$ unary predicate defines what is considered a monad in MIL. Single
monad is one such case:

\infrule{isSingleMonad(M)}{isMonad(M)}

There is also an infix type constructor $:::$ that combines two monads.  We
call $:::$ a *monad cons* operator, similarly to list cons cells. This is the
way to combine monads in MIL. One can look at it as a type-level list of monads
(hence the naming). What it represents is a monad transformer stack. In MIL
there is no distinction between the `State` monad and the `StateT` monad
transformer.  It is the context that determines the meaning. When a monad is to
the left of monad cons, it is considered a transformer, when it is to the right
or is used as a type constructor elsewhere, it is a monad. When talking about
MIL these terms can be used interchangeably. The monad cons operator should be
thought of as right-associative. The following rule defines that $M_1 ::: M_2$
is a monad, if $M_1$ is a single monad and $M_2$ is a monad (so it can be a
monad cons as well):

\infrule{isSingleMonad(M_1) \andalso isMonad(M_2)}{isMonad(M_1 ::: M_2)}

The following example gives an intuition with relation to monad transformers in
Haskell (`State` in MIL does not have a type of storage as opposed to `StateT`
in Haskell, so it is substituted with `()`):

`Error Int ::: (State ::: IO)` $\Rightarrow$ `ErrorT Int (StateT () IO)`

For the sake of the definitions below, we also define $isMonadCons(M)$ to hold
if $M$ is a combination of two monads with $:::$. We also use $monadConsLeft$
and $monadConsRight$ functions to get the left-hand side and the right-hand
side of a monad cons respectively.

The core of determining whether two monads are compatible is the following
non-commutative operation:

\infrule{isSingleMonad(M_1) \andalso isSingleMonad(M_2) \andalso M_1 \equiv_\alpha M_2}{isCompatibleMonadNotCommut(M_1, M_2)}
\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \andalso monadConsLeft(M_1) \equiv_\alpha monadConsLeft(M_2) \\ isCompatibleMonadNotCommut(monadConsRight(M_1), monadConsRight(M_2))}{isCompatibleMonadNotCommut(M_1, M_2)}
\infrule{isSingleMonad(M_1) \andalso isMonadCons(M_2) \andalso M_1 \equiv_\alpha monadConsLeft(M_2)}{isCompatibleMonadNotCommut(M_1, M_2)}

It specifies that two single monads are compatible if they are
alpha-equivalent. Alpha-equivalence for MIL monads is pretty straight-forward:
every monad is alpha-equivalent to itself. In the case of two $Error\ T$, their
type arguments denoting the error types also must be alpha-equivalent. Another
case is for two monad conses: their left-hand sides must be alpha-equivalent
and then the recursive case on the right-hand sides must hold as well. Finally,
a single monad is compatible with a monad cons if it is alpha-equivalent to the
left-hand side of the monad cons. TODO: prefix intuition.

$isCompatibleMonad$ is just a disjunction of two
$isCompatibleMonadNotCommut$ with arguments swapped:

\infrule{isCompatibleMonadNotCommut(M_1, M_2) \andalso \vee \andalso isCompatibleMonadNotCommut(M_2, M_1)}{isCompatibleMonad(M_1, M_2)}

TODO

~~~
isMonadSuffixOf :: MonadType -> MonadType -> Bool
isMonadSuffixOf (MTyMonad m1) (MTyMonad m2) = m1 `alphaEq` m2
isMonadSuffixOf t1@(MTyMonadCons {}) t2@(MTyMonadCons _ mt2) =
  t1 `alphaEq` t2 || t1 `isMonadSuffixOf` mt2
isMonadSuffixOf t1@(MTyMonad {}) (MTyMonadCons _ mt2) = t1 `isMonadSuffixOf` mt2
isMonadSuffixOf (MTyMonadCons {}) (MTyMonad {}) = False
~~~

$highestEffectMonad$ is a function that takes two monads and return the one,
which encodes more effects. We define that monads combined with monad cons have
a higher effect than a monad:

\infrule{isMonadCons(M_1) \andalso isSingleMonad(M_2)}{highestEffectMonad(M_1, M_2) = M_1}
\infrule{isSingleMonad(M_1) \andalso isMonadCons(M_2)}{highestEffectMonad(M_1, M_2) = M_2}

For two monads, $highestEffectMonad$ returns the first one, so we do not have
any ordering between the MIL monads:

\infrule{isSingleMonad(M_1) \andalso isSingleMonad(M_2)}{highestEffectMonad(M_1, M_2) = M_1}

The most interesting case is when both arguments are monad conses. In this case
we recurse into the right hand-sides of monad conses, since that is where they
might differ:

\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \\ highestEffectMonad(monadConsRight(M_1), monadConsRight(M_2)) = monadConsRight(M_1)}{highestEffectMonad(M_1, M_2) = M_1}
\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \\ highestEffectMonad(monadConsRight(M_1), monadConsRight(M_2)) = monadConsRight(M_2)}{highestEffectMonad(M_1, M_2) = M_2}

One can say that the intuition behind $highestEffectMonad$ is that the longer
chain of monad conses has a higher effect.

## Haskell implementation

Since one of the aims of this thesis was to produce a programming framework for
working with the designed monadic intermediate representation, here we will
outline some of the implementation details. MIL is implemented in Haskell.

* AST
* Type checker?
* API

Implementation details specific to optimisations are presented in Chapter\ \ref{chap:opt}.

