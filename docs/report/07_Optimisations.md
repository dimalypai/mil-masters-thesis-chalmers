# Optimisations

> *The majority of modern compilers implement sophisticated optimisations to
> produce as good code as possible. This can mean quite different things, for
> example, low memory consumption, speed (low CPU consumption) or low energy
> consumption. In this chapter we will present several code transformations
> implemented in MIL.*

As was mentioned in the "Introduction" chapter one of the goals of designing a
monadic intermediate representation is to be able to express and use optimising
code transformations in the presence of effects. MIL provides a dozen of
implemented transformations available for using from a source language compiler
which targets MIL. Most of the transformations presented in this chapter are
either direct implementations or inspired by the transformations presented in
\cite{Tolmach} and \cite{Benton}.

Unfortunately, since there is no intepreter or code generator implemented for
MIL at the moment, we cannot execute MIL code and therefore cannot measure its
runtime characteristics like running time or memory consumption. We can only
estimate the impact of transformations by the code size. Depending on how MIL
is implemented, an intuition about the usefulness of certain optimisations
might be incorrect.

We will try to provide as many code snippets with real implementation of
transformations as possible, but to be able to do this in a reasonable amount
of space we will focus at the expression level of granularity, since this is
where the transformations happen. More high level cases (e.g. function
definition) are merely recursing down to get to expressions. The MIL AST has
support for Uniplate (<http://community.haskell.org/~ndm/uniplate/>) -- a
Haskell library for generic traversals of algebraic data types and so code
transformations are implemented using these traversals. We want to highlight
that many of the transformation implementations are relatively naive and serve
the purpose of demonstrating the concept.

## Monad laws

As was stated in Chapter 2 all monads must satisfy three monad laws: left
identity, right identity and associativity. These three laws can be used not
only to verify that a certain structure is ineed a monad, but also to optimise
monadic code.

### Left identity

The first law that we will consider is left identity. The following is its
definition from Chapter 2:

$$bind\ (return\ x)\ f = f\ x$$

From the compiler optimisation point of view, one can look at this law as at
some kind of *inlining*, which is one of the frequently used optimisations in
modern compilers. The left identity monad law allows us to avoid a redundant
$bind$ and use `x` directly.

The following code snippet is a Haskell implementation of the left identity
transformation for MIL:

~~~{.haskell}
TODO
~~~

The following is an MIL example containing the original code and then the
resulting code after applying the transformation:

~~~
let (x : Unit) <- return [Id] unit
in return [Id] x;

return [Id] unit;
~~~

### Right identity

The second law allows us to eliminate redundant $bind$ and a subsequent
$return$:

$$bind\ m\ return = m$$

An implementation of this transformation looks like follows:

~~~{.haskell}
TODO
~~~

An example for this transformations looks pretty much the same as for the left
identity, but what is really happening is that it is only the whole binder
expression which is left (instead of subtituting `x` with `1` in the body):

~~~
let (x : Int) <- return [Id] 1
in return [Id] x;

return [Id] 1;
~~~

### Associativity

The last of the three monad laws is associativity. This law was presented as
the following:

$$bind\ (bind\ m\ f)\ g = bind\ m\ (\lambda x \to bind\ (f\ x)\ g)$$

Th associativity transformation is implemented as in the snippet below:

~~~{.haskell}
TODO
~~~

The following example shows a simple MIL code snippet and its transformed
version:

~~~
let (y : Unit) <-
  let (x : Unit) <- return [Id] unit
  in return [Id] x
in return [Id] y;

let (x : Unit) <- return [Id] unit
in let (y : Unit) <- return [Id] x
in return [Id] y;
~~~

Associativity transformations opens up opportunities for other transformations,
by restructuring the code and making it more linear. For example, identity laws
usually can be applied more times after the associativity has been applied.

## Lift transformations

The next couple of transformations are related to the monad transformer $lift$
operation. Both of them are trying to eliminate redundant $lift$ operations.

### Identity

The first one, which we call identity, can remove a $lift$ where the source and
the target monads are the same.

This transformation is implemented as follows:

~~~{.haskell}
TODO
~~~

The next example is probably the simplest case of applying this transformation:

~~~
lift [Id => Id] return [Id] unit;

return [Id] unit;
~~~

### Composition

The second $lift$ transformation that is implemented for MIL is trying to
replace a sequence of two $lift$ operations with one. In such a sequence of
$lift$s the target of the inner (second) one is compatible with the source of
the outer (first) one, which is guaranteed by the type/lint checking.

An implementation of this transformation is below:

~~~{.haskell}
TODO
~~~

The next example demonstrates a composition of two $lift$ operations:

~~~
lift [IO ::: State => Error Unit ::: (IO ::: State)]
  lift [State => IO ::: State] return [State] unit;

lift [State => Error Unit ::: (IO ::: State)] return [State] unit;
~~~

## Effect-dependent transformations

While, for example, monad laws and lift transformations above are applicable to
all monads and monad transformer stacks, some code optimisations and
transformations are effect-dependent. One of the strengths of intermediate
representations based on monads and MIL in particular is the ability to express
such transformations and correctly apply them only when it is possible.

### Id

We will start with a transformation that is, in general, only applicable to
pure computations. Computations inside the `Id` monad are one example of such
computations.

This transformation allows to reorder two computations.  Such transformation is
not obviously beneficial, but it can be used to enable other transformations or
when it comes to low level considerations, may improve memory locality or
parallelisation.

Here is an implementation of this transformation:

~~~{.haskell}
exchangeExpr = transform f
  where
    f expr@(LetE varBinder e1 e2) =
        case e2 of
          LetE varBinder' e1' e2' | getBinderVar varBinder `isNotUsedIn` e1' ->
            case getTypeOf expr of
              TyApp (TyMonad (MTyMonad (SinMonad Id))) _ ->
                LetE varBinder' (exchangeExpr e1')
                  (LetE varBinder (exchangeExpr e1) (exchangeExpr e2'))
              _ -> LetE varBinder (exchangeExpr e1) (exchangeExpr e2)
          _ -> LetE varBinder (exchangeExpr e1) (exchangeExpr e2)
    f x = x
~~~

We will skip giving an example in this case, since it would not be particularly
interesting.

### State

Next, we will describe four transformations applicable to computations inside
the `State` monad.

The first one is a special case of the reordering transformation presented
above. We refer to it as "exchange new". It allows to reorder creation of two
references. It is implemented as the following:

~~~{.haskell}
TODO
~~~

Another special case of the reordering transformation is "exchange read", which
can reorder reading of two references. Below is an implementation of it:

~~~{.haskell}
TODO
~~~

The third transformation for `State` computations allows to eliminate a
reference reading in the case when this reference has already been read and
bound to a variable. It is implemented as follows:

~~~{.haskell}
TODO
~~~

Below is a small example that instead of reading the reference `x` again,
reuses the value of the variable `a`, which already contains the value of `x`:

~~~
let (x : Ref Int) <- new_ref [Int] 1
in let (a : Int) <- read_ref [Int] x
in let (b : Int) <- read_ref [Int] x
in return [State] unit;

let (x : Ref Int) <- new_ref [Int] 1
in let (a : Int) <- read_ref [Int] x
in let (b : Int) <- return [State] a
in return [State] unit;
~~~

The last `State` transformation also eliminates a reference reading, but in
this case the information from a reference writing operation is used. Its
implementation is shown in the next code snippet:

~~~{.haskell}
TODO
~~~

The following is an example of reusing an expression that was written to a
reference instead of reading the reference:

~~~
let (x : Ref Int) <- new_ref [Int] 1
in let (z : Unit) <- write_ref [Int] x 2
in let (a : Int) <- read_ref [Int] x
in return [State] unit;

let (x : Ref Int) <- new_ref [Int] 1
in let (z : Unit) <- write_ref [Int] x 2
in let (a : Int) <- return [State] 2
in return [State] unit;
~~~

### Error

The last of the effect-specific transformations implemented for MIL is a
transformation that tries to eliminate unnecessary `catch_error` and
`throw_error` calls. It (rather naively) looks for a specific case, when the
first argument of `catch_error` is `throw_error`, which basically means that
the handler part will definitely be executed.

It is implemented in Haskell as the following:

~~~{.haskell}
TODO
~~~

This optimisation is shown in the following code snippet:

~~~
catch_error_1 [Unit] [Int]
  (throw_error [Unit] [Int] unit)
  (\(e : Unit) -> return [Error Unit] 1);

return [Error Unit] 1;
~~~

## Case expression transformations

In this section we present two transformations around `case` expressions which
is an important part of implementing conditional and pattern matching
constructs in source languages.

### Constant case elimination

Constant case elimination transformation allows to remove a `case` expression,
which has a known outcome (because of literal patterns and a literal
scrutinee). It is implemented for MIL as below:

~~~{.haskell}
TODO
~~~

An example of applying this transformation is presented below:

~~~
fun : State Int =
  case 1 of
    | 0 => return [State] 0
    | 1 => return [State] 1
    | _ => return [State] 2
  end;

fun : State Int = return [State] 1;
~~~

This transformation can be applied to eliminate unnecessary `when` statements
in OOLang, when it is known that a condition evaluates to `true` or `false`.

### Common bind extraction

The common bind extraction is a transformation that can *hoist* a $bind$ to the
same variable out of `case` alternatives, given that the $bind$s have the same
body expression. Here is an implementation of this transformation:

~~~{.haskell}
TODO
~~~

The following is an example of applying the common bind extraction:

~~~
case 1 of
  | 0 => let (x : Int) <- return [IO] 0 in return [IO] unit
  | 1 => let (x : Int) <- return [IO] 1 in return [IO] unit
end;

let (x : Int) <-
  case 1 of
    | 0 => return [IO] 0
    | 1 => return [IO] 1
  end
in return [IO] unit;
~~~

## Constant folding

*Constant folding* is one of the very essential and popular optimisations in
compilers which deals with finding constant expressions and evaluating them at
compile time instead of doing unnecessary computations at runtime.

In MIL constant folding is implemented for all built-in arithmetic operations,
except for division. It is not implemented for division, since division is a
potentially failing operation (for example, when dividing by 0), so it would
require a more detailed analysis. An implementation of constant folding for MIL
looks like the following:

~~~{.haskell}
TODO
~~~

This time we will not have an example with only one transformation applied to a
small piece of MIL code, but rather an extended example with a piece of OOLang
code and a number of different transformations applied (the whole pipeline is
presented in the next section). The example contains simple OOLang function
performing arithmetic operations and printing their results. What follows is a
fragment of non-optimised MIL code (we needed to cut it to take a reasonable
amount of space) and finally the optimised version of it:

~~~
def main : Unit
  a : Int = 2 + 3;
  b : Int = 3 - 2;
  printInt b;  # Make sure it is not optimized away completely
  c : Int = 2 * 2;
  d : Int = a + c;
  printInt d;  # Make sure it is not optimized away completely
  e : Float = 2.0 + 3.0;
  f : Float = 3.0 - 2.0;
  printFloat f;  # Make sure it is not optimized away completely
  g : Float = 2.0 * 2.0;
  h : Float = e + g;
  printFloat h;  # Make sure it is not optimized away completely
  i : Int = 6 / 2;
  j : Float = 6.0 / 2.0;
end

main : (Error Unit ::: (State ::: IO)) Unit =
  let (a : Int) <-
    let (var_30 : Int) <- return [Error Unit ::: (State ::: IO)] 2
    in let (var_31 : Int) <- return [Error Unit ::: (State ::: IO)] 3
    in return [Error Unit ::: (State ::: IO)] add_int var_30 var_31
  in let (b : Int) <-
       let (var_28 : Int) <- return [Error Unit ::: (State ::: IO)] 3
       in let (var_29 : Int) <- return [Error Unit ::: (State ::: IO)] 2
       in return [Error Unit ::: (State ::: IO)] sub_int var_28 var_29
  in let (var_2 : Unit) <-
       let (var_0 : Int -> (Error Unit ::: (State ::: IO)) Unit) <-
          return [Error Unit ::: (State ::: IO)] printInt
       in let (var_1 : Int) <-
            return [Error Unit ::: (State ::: IO)] b
       in var_0 var_1
  ...

main : (Error Unit ::: (State ::: IO)) Unit =
  let (var_2 : Unit) <- printInt 1
  in let (var_5 : Unit) <- printInt 9
  in let (var_8 : Unit) <- printFloat 1.0
  in let (var_11 : Unit) <- printFloat 9.0
  in let (i : Int) <- div_int 6 2
  in let (j : Float) <- div_float 6.0 2.0
  in return [Error Unit ::: (State ::: IO)] unit;
~~~

## Transformations and source languages

As we saw above, every transformation is a pure function which takes a typed MIL
program and returns a typed MIL program. This allows to easily compose
different transformations in a pipeline.

To chain different transformations in the code, we use a *postfix application
operator* (also known as a very popular *pipeline operator* in the F#
programming language). We define it in Haskell as follows:

~~~
(|>) :: a -> (a -> b) -> b
a |> f = f a
~~~

Here is how the chain of transformations for FunLang looks like:

~~~
optimiseMil :: MIL.TyProgram -> MIL.TyProgram
optimiseMil milProgram =
  milProgram
  |> MILTrans.associativity
  |> MILTrans.leftIdentity
  |> MILTrans.rightIdentity
  |> MILTrans.associativity
  |> MILTrans.associativity
  |> MILTrans.foldConstants
  |> MILTrans.leftIdentity
  |> MILTrans.foldConstants
~~~

The one for OOLang looks like the following:

~~~
optimiseMil :: MIL.TyProgram -> MIL.TyProgram
optimiseMil milProgram =
  milProgram
  |> MILTrans.associativity
  |> MILTrans.leftIdentity
  |> MILTrans.rightIdentity
  |> MILTrans.foldConstants
  |> MILTrans.leftIdentity
  |> MILTrans.foldConstants
  |> MILTrans.eliminateConstantCase
~~~

These particular pipelines are targeting some specific use cases used for
testing. In order to achieve better results and apply as many optimisations as
possible one could make use of *fixed point optimisations* (which are also
available in Uniplate). Such a process would apply optimisation(s) until an
optimisation pass does not change the program. There are different approaches
of varying complexity to solving the problem of finding the best sequence of
transformations, which we did not focus on in this thesis.

Of course, a compiler writer can implement additional MIL code transformations
to optimise use cases specific to the source language being compiled.

Unfortunately, some of the transformations are not really applicable to the
source languages presented in the previous chapters because of the monad stacks
they have. For example, `State` transformations cannot be directly applied
since they work only on computations that have just `State` as their effect,
but monad stacks in FunLang and OOLang have, for example, `IO` in their impure
stacks in addition to `State`.

## Conclusions

This chapter presented implementations of many transformations of MIL code
ranging from monad laws and some other effect-independent transformations to
transformations for specific effects like `Id`, `State` and `Error`.

Implementing transformations shown in this chapter does not require a lot of
work. The structure of MIL AST and its support for Uniplate makes it possible
to write clear and concise Uniplate-based transformation code.

Generality and composability of most transformations allows to easily reuse
them and build modular code optimisation pipelines in source language
compilers.

Applicability of some transformations to the code generated for source
languages can be somewhat limited, but we believe that it can largely be solved
by implementing some kind of effect inference/elimination process mentiond in
Chapter 5.

