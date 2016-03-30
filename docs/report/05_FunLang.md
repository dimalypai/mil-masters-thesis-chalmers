# Functional programming language (FunLang)

> *This chapter introduces the first source programming language which has been
> designed during this work -- FunLang. It starts from the language overview
> and example programs. Then the code generation to MIL is described. Finally,
> we make conclusions regarding the implementation of the FunLang compiler
> using MIL.*

## Overview

FunLang is a rather small functional programming language. Its design is mostly
inspired by Haskell. It is statically and explicitly typed. The type system of
FunLang is based on System F. In addition to the pure polymorphic lambda
calculus FunLang incorporates `do`-notation similar to the one found in Haskell
and has a couple of built-in monads. It also adds {-- a --} minimal exception handling.

One can say that FunLang is quite similar to MIL in many ways. The main
motivation behind designing FunLang as one of the languages for MIL evaluation
is to be able to explore {-- a --} compilation of a modern statically typed functional
language. Despite the many similarities to MIL, as we will see, the MIL code
produced for FunLang programs looks rather different from the programs' source
code. In addition to the semantics similar to the one of MIL, FunLang offers
cleaner syntax, since it is a user facing language, rather than an intermediate
representation. One of the main semantic differences is implicit effects which
are present in FunLang programs, as we will see in this chapter.

## FunLang by example

Similarly to the previous chapter, we will provide several examples showcasing
the main features of the language.

### Data types

As most statically typed functional programming languages, FunLang supports ADTs
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

FunLang provides built-in `IO` and `State` monads, which are similar to the
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
`IO` monad. The functions `evalState` and `execState` are for running
computations in the `State` monad. They both take one such computation and an
initial state value. The difference is that `evalState` returns the value of
the computation as the result and `execState` returns the value of the state
itself. Note, there is no `runState` similar to the one in Haskell, since it
would require support for tuples in FunLang, which was omitted for simplicity. {>> Arguably, you *do* have support for tuples in FunLang. type Pair A B = Pair A B. <<}
The last three functions are for working with state inside a stateful
computation. They allow to read the state value, overwrite it and modify it
with a function.

It is worth highlighting, that unfortunately, FunLang does not support
combining monads in any way. In spite of this property of FunLang, combining
monads in MIL is used extensively, as we will describe in the "Code generation"
section. Moreover, some features of FunLang, like the ability to run stateful
computations with `evalState` and `execState` inside of any computation
influences the representation of effects in MIL code quite significantly.

### Exceptions

FunLang provides rather naive support for exception handling. There are two
language constructs for it: `throw` and `catch`. The first one is an expression
that allows to raise an exception. Exceptions in FunLang do not carry
additional information, so `throw` is used standalone, without any value
specified, but in order to assign a type to it, it needs to be annotated with a
type. The `catch` construct is a binary infix operator, that tries to evaluate
an expression on the left-hand side and if that throws an exception, it returns
an expression on the right-hand side. The following example will result in `1`,
since the expression on the right is `throw` and the handler expression is `1`:

~~~
throw [Int] catch 1
~~~

## Code generation

In this section we will outline how FunLang code is translated into MIL code.

In order to generate FunLang code we needed to decide in which monads different
kinds of computations should be expressed. Looking at FunLang, there are three
different kinds of code: "pure" code (outside of monads), code inside `IO` and
code inside `State`. Pure code is not completely pure, though. First, functions
can be non-terminating. MIL does not have a monad for non-termination, so we do
not worry about this particular effect. Second, FunLang exceptions have
non-monadic types, but we need to express potential failure in MIL. This means
that the `Error` monad should be part of all computations. For stateful
computations we need to add `State` and for input/output -- IO. The problem
here is that FunLang allows to "escape" `State` by using `evalState` and
`execState` and therefore running stateful computations inside pure or `IO`
computations. Escaping `State` is not possible in MIL, so we need to add
`State` to all stacks. Given all this, we have two effect stacks: `State :::
Error Unit` for pure computations and computations in the `State` monad and
`State ::: (Error Unit ::: IO)` for computations in the `IO` monad. If we do an
exercise of expanding types as we did in one of the previous chapters for the
`IO` monad stack, we will get `s -> IO (Either Unit (a, s))`. This can be read
as "a function that given a state value can perform IO and either results in an
error or produces a value and a new state".  `Unit` is chosen as a type for
error values, because exceptions in FunLang do not carry any value. We choose
to have `State` on top of `Error`, but for FunLang different orderings of
`Error` and `State` cannot be observed, since `catch` can at most have a
stateful computation which has been run with `evalState` or `execState` and
therefore state cannot escape outside of that computation. One could extend
FunLang with the `Error` monad that can be combined with `State` somehow in
order to be able to interrupt stateful computations with errors.

Note, that there is an important relation between the pure and the `IO` monad
stacks above. They satisfy the $isCompatible$ relation in MIL (the former is a
prefix of the latter). This allows us to use computations in the pure monad
stack inside computations in the `IO` stack.

### General scheme and type conversions

In order to generate code for function definitions, a type of every function
needs to be converted to the corresponding MIL representation. There were two
main considertions here: all effects of FunLang that are possible to express in
MIL have to be captured in types and code generation scheme should be as
uniform as possible. These considerations together result in the fact that all
FunLang code, even expressions not inside monads, becomes monadic code in MIL.
In such a scheme every sub-expression has a monadic type and needs to be bound
to a variable with $bind$ before it can be used. For example, literal
occurences result in $return$s and in order to be passed into a function their
values need to be extracted from a monad (with $bind$). One can see some
resemblance to the ANF in a sense that all expressions are broken down into
intermediate simple expressions, like variables and function applications.

Next, we will provide some examples to demonstrate the code generation scheme
outlined above.

A FunLang function with the simplest type `Int` can potentially throw an
exception instead of returning an integer. This means that its MIL type should
be `(State ::: Error Unit) Int`. The same way, when there is a
universally-quantified type or a function type, instead of having lambda
binders in the function body, it can just have `throw` with an appropriate type
annotation. Therefore, there must be a monad in front of every `forall` type
and every function parameter type. Below is an example of the identity function
in FunLang and then the MIL code generated for it:

~~~
id : forall A . A -> A
id = /\A . \(x : A) -> x;;

id : (State ::: Error Unit) (forall A .
       (State ::: Error Unit) (A ->
         (State ::: Error Unit) A)) =
  return [State ::: Error Unit] /\A .
    return [State ::: Error Unit] \(x : A) ->
      return [State ::: Error Unit] x;
~~~

`IO` type gets translated to the corresponding monad stack described at the
beginning of this section. We will consider type conversions for `State` in a
separate section.

The next example demonstrates how function and type applications look in the
generated code:

~~~
one : Int
one = id [Int] 1;;

one : (State ::: Error Unit) Int =
  let (var_1 : Int -> (State ::: Error Unit) Int) <-
    let (var_0 : forall A . (State ::: Error Unit)
                   (A -> (State ::: Error Unit) A)) <- id
    in var_0 [Int]
  in let (var_2 : Int) <- return [State ::: Error Unit] 1
  in var_1 var_2;
~~~

Every sub-expression gets bound to a variable and the code generator produces
fresh variable names for all of the intermediate sub-expressions. One can
relate nested binds above to the fact that applications are left-associative.

### Data types and data constructors

Given the similarity between FunLang and MIL, there is basically almost no
translation of data type definitions. There exists one problem with data
constructors and their application as functions, though. Every MIL data
constructor is introduced as a function in the global scope and therefore, it
can be used as a function. Application of data constructors in MIL does not
have any effect, which makes them different from all the other function
definitions generated from FunLang, which have a monad attached to every
argument position. In order to avoid having this special case which can
complicate the code generation slightly, we generate a wrapper function for
every data constructor. This wrapper function is then used instead of the data
constructor occurrence itself.

The following example contains a definition of the `Pair` data type and the
corresponding wrapper function for its only data constructor:

~~~
type Pair A B = MkPair A B;

con_MkPair :
  (State ::: Error Unit) (forall A .
    (State ::: Error Unit) (forall B .
      (State ::: Error Unit) (A ->
        (State ::: Error Unit) (B ->
          (State ::: Error Unit) (Pair A B))))) =
  return [State ::: Error Unit] /\A .
    return [State ::: Error Unit] /\B .
      return [State ::: Error Unit] \(var_1 : A) ->
        return [State ::: Error Unit] \(var_2 : B) ->
          return [State ::: Error Unit]
            MkPair [A] [B] var_1 var_2;
~~~

What the code generator did in this case was generating an MIL expression from
the type of the data constructor. The problem of generating an expression which
has a particular type can be really hard for arbitrary types, but in the case
of data constructors the shape of possible types is quite restricted.

{>> Perhaps it would be good to say a few words about the fact that the optimizer will simplify the generated code quite a bit. <<}

### Built-in types and functions

Most of the FunLang built-in data types map one-to-one to the MIL data types,
except for the `String` type, since MIL provides only `Char`. A solution to
this is to predefine a `String` data type as the following:

~~~
type String
  = Empty_Str
  | Cons_Str Char String;
~~~

This is basically a list of characters. String is either empty or it is a cons
cell of a character and another string.

When it comes to built-in functions related to reading and printing built-in
data types, MIL provides only `read_char` and `print_char`, therefore FunLang
built-in functions like `printString`, `readString` and so on need to be
expressed in terms of `read_char` and `print_char`. All these functions have to
work inside the `IO` stack. It is very helpful that the `IO` stack contains
`Error`, since one needs to throw an exception in case when `readInt` is called
and the first character in the input stream is a letter, for example. When
using `read_char` and `print_char` lifting needs to be used in order for types
to match, since these MIL functions have just `IO` in their types. The
following code snippet is the implementation of the `printString` function:

~~~
printString : (State ::: Error Unit)
                (String -> (State ::: (Error Unit ::: IO)) Unit) =
return [State ::: Error Unit]
  \(s_ : String) ->
  case s_ of
  | Empty_Str =>
      return [State ::: (Error Unit ::: IO)] unit
  | Cons_Str (c_ : Char) (cs_ : String) =>
      let (unit_0 : Unit) <-
        lift [IO => State ::: (Error Unit ::: IO)] print_char c_
      in let (printString_ : String -> (State ::: (Error Unit ::: IO)) Unit) <-
           printString
         in printString_ cs_
  end;
~~~

In the actual implementation of FunLang most of the built-in functions, except
for `printString` and `readString` are actually just stubs in order to
demonstrate the concept without fully-implementing this non-crucial part.

Arithmetic operators map directly to built-in MIL functions:

~~~
plus : Int
plus = 1 + 2;;

plus : (State ::: Error Unit) Int =
  let (var_0 : Int) <- return [State ::: Error Unit] 1
  in let (var_1 : Int) <- return [State ::: Error Unit] 2
  in return [State ::: Error Unit] add_int var_0 var_1;
~~~

This is one of the cases when not every sub-expression is bound to a variable,
since the code generator knows the type of the `add_int` function when
generating code for addition.

A more interesting case is division. Since it is rather easy for division to
fail, namely, when using 0 as a divisor, `div_int` and `div_float` functions in
MIL have `Error` in their types. We need to use the $lift$ operation in this
case to get `State` on top:

~~~
division : Int
division = 1 / 2;;

division : (State ::: Error Unit) Int =
  let (var_0 : Int) <- return [State ::: Error Unit] 1
  in let (var_1 : Int) <- return [State ::: Error Unit] 2
  in lift [Error Unit => State ::: Error Unit] div_int var_0 var_1;
~~~

### State

Code generation for FunLang computations inside the `State` monad makes use of
MIL references. The main problem to solve was {== "Where does a function get the
state to work with?". ==}{>> I'm afraid I don't understand this question. Can you elaborate? <<} It was solved by adding an extra parameter of MIL `Ref`
type to every `State` function. The example below shows a type of a stateful
computation in FunLang and the corresponding MIL type:

~~~
stateFun : State Int Unit

stateFun : (State ::: Error Unit) (Ref Int -> (State ::: Error Unit) Unit)
~~~

This implies that the code generator needs to keep track of whether a
sub-expression type has a reference as a parameter and pass the reference in
scope as an argument. A special name for such reference is used: `state_`.
This variable must be introduced by a lambda for every `State` function.  The
following example shows a simple `State` function that just reads the state
value. One can see how the code generator introduced the `state_` reference and
then supplied it to the `get` function:

~~~
stateFun : State Int Unit
stateFun = do
  i : Int <- get [Int];
  return unit;
end;;

stateFun : (State ::: Error Unit) (Ref Int -> (State ::: Error Unit) Unit) =
  return [State ::: Error Unit] \(state_ : Ref Int) ->
    let (i : Int) <-
      let (var_0 : forall S_ . (State ::: Error Unit)
                      (Ref S_ -> (State ::: Error Unit) S_)) <-
        get
      in let (var_1 : Ref Int -> (State ::: Error Unit) Int) <-
           var_0 [Int]
      in var_1 state_
    in let (var_2 : Unit) <- return [State ::: Error Unit] unit
    in return [State ::: Error Unit] var_2;
~~~

Built-in `State` functions such as `evalState`, `execState`, `get`, `put` and
`modify` are implemented in MIL and this code is emitted at the beginning of
every program together with other built-in functions and data types.  The
function `evalState` creates a reference with the initial state value that it
is given and runs a given `State` computation with this reference as an
argument.  The same happens in `execState`, but in addition it reads the
reference to get the final state value out and returns it. The `get` and `put`
functions are wrappers around `read_ref` and `write_ref` respectively. The
`modify` operation is a more high-level combination of these two with a given
state transformation function applied in between.

### Exceptions

When it comes to code generation for FunLang exception handling, the `throw`
expression maps almost directly to the `throw_error` function in MIL. Its first
type parameter always gets `Unit`, since no other values can be thrown in
FunLang. Its second type parameter is the type annotation on `throw` converted
to the corresponding MIL type. And finally, the error value itself is `unit`.
The $lift$ operation to put `State` on top also needs to be generated, since
`throw_error` only has `Error` in its type.

Dealing with `catch` turned out to be a much bigger problem. When presenting
MIL built-in functions in the previous chapter, we omitted the `catch_error`
function, which one would definitely expect to see there. One possible type of
`catch_error` could be `forall E . forall A . Error E A -> (E -> Error E A) ->
Error E A`, where the first parameter (after the two type parameters)
corresponds to the left-hand side of FunLang `catch` and the second parameter
corresponds to the right-hand side wrapped in a lambda, which takes a `Unit`
placeholder. The problem with this type and the design of FunLang is that in
FunLang exceptions can be thrown and handled both inside pure computations and
inside `IO` computations, which corresponds to two different monad stacks. So
there are two possible types of computations that we need to pass to
`catch_error` and none of those match the type above. As was presented in the
previous chapter, a type of the argument to a function must satisfy the
$isCompatible$ relation with the corresponding parameter type. If we say that
instead of `Error E A` the first parameter of `catch_error` could be `State :::
(Error E ::: IO) A` (as well as all the other occurences of `Error E A`), then
we could pass a pure/stateful FunLang computation of type `(State ::: Error E)
A` as an argument (it has less effects than `catch_error` expects). But if we
were to apply `catch_error` with IO in its type inside a function of type
`(State ::: Error E) A`, the MIL type/lint checker would give an error, since
the result would have had more effects than specified by the function type. If
we instead say that `catch_error` should have `(State ::: Error E) A` instead
of `Error E A` in its type, then we cannot pass an IO computation as the first
argument, since its type is not compatible (it has more effects than
`catch_error` expects).

For this reason, MIL provides two functions: `catch_error_1` and
`catch_error_2` as a workaround. The types of these functions are not fully
specified in MIL itself.  There are placeholders for monads, which a source
language compiler needs to fill in, when using the MIL type/lint checker.
FunLang puts the pure monad stack and the `IO` stack respectively and then uses
the corresponding versions of `catch_error` depending on the context, in which
`catch` expression is used. This solution is, of course, not sustainable and
can support up to two possible monad stacks in a similar case, when exceptions
can be thrown in all the contexts in the source language.

What one would really want to capture in the type of `catch_error` here is the
fact that it expects a computation that *has* `Error` as one of its effects.
But what `Error E A` means in MIL is that a computation has *just* the `Error`
effect, not less and not more. Unfortunately, MIL does not offer an ability to
express the fact that a computation "has at least this effect, but maybe more".

## Conclusions

In general, we can say that code generation from FunLang to MIL is rather
straight-forward, because FunLang and MIL are quite close to each other. The
main difficulties were to get the type conversions right as well as working
with references for `State`.

The code generator also used one small practical trick in order to avoid
potential collisions with different names (function, variable and type names)
in FunLang code when generating fresh names. FunLang's lexer does not allow
underscores in those names, while MIL does allow them, so all the generated
names contained underscores in them, which eliminates the possibility of
collisions with names in source programs.

When looking at the MIL code generated for pure FunLang code, which is actually
pure and not only specifies that in the type (meaning when it does not throw
exceptions), one could see that the size of the generated code is significantly
bigger than it could have been if it was non-monadic. To eliminate all the
unnecessary returns, binds and remove monads from the types as well as
introduce opportunities for more optimisations, one would need to implement
some kind of *effect inference*. Such a process could analyse the code and
infer that it does not have the specified effects and then rewrite the code,
simplifying it. {>> But your transformations go a long way to simplify the code. With effect inference you would certainly get even better results but I don't think you should downplay the effectiveness of what you've already implemented. <<}

One could quite easily extend FunLang with more powerful features like `case`
expressions or combining monads with transformers, for example, because MIL is
a rather expressive language and has those features available out of the box.

We think that the problem described in the section on code generation for
exceptions might be the biggest current MIL limitation. We will expand this
more in the following chapter.

