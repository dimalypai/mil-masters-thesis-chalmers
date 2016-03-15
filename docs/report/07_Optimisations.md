# Optimisations

> *The majority of modern compilers implement sophisticated optimisations to
> produce as good code as possible. In this chapter we will present several
> code transformations implemented in MIL. We conclude with a discussion around
> classical optimisation techniques and their relations to MIL.*

As was mentioned in the "Introduction" chapter one of the goals of designing a
monadic intermediate representation is to be able to express and use optimising
code transformations in the presence of effects. MIL provides a dozen of
implemented transformations available for using from a source language compiler
which targets MIL. Most of the transformations presented in this chapter are
either direct implementations or inspired by the transformations presented in
\cite{Tolmach} and \cite{Benton}.

Unfortunately, since there is no intepreter or code generator implemented for
MIL at the moment, we cannot execute MIL code and therefore cannot measure its
runtime characteristics like running time or memory consumption. We can just
estimate the impact of transformations by the code size. Depending on how MIL
is implemented, an intuition about the usefulness of certain optimisations
might be incorrect. Another way to get an idea about the runtime behaviour of
some of the MIL constructs is to make connections to the semantics of similar
operations as presented in \cite{Tolmach} and \cite{Benton}.

We will provide some code snippets with real implementation of transformations,
but to be able to do this in a reasonable amount of space we will focus at the
expression level of granularity, since this is where the transformations
happen. More high level cases (e.g. function definition) are merely recursing
down to get to expressions. The MIL AST has support for \cite{Uniplate} -- a
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

The left identity monad law allows us to avoid a redundant $bind$ and use `x`
directly.

The following code snippet is a Haskell implementation of the left identity
transformation for MIL:

~~~{.haskell}
leftIdentityExpr :: TyExpr -> TyExpr
leftIdentityExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e1 of
        ReturnE _tm e ->
          (VarE varBinder, leftIdentityExpr e) `replaceExprIn` (leftIdentityExpr e2)
        _ -> LetE varBinder (leftIdentityExpr e1) (leftIdentityExpr e2)
    f expr = descend f expr
~~~

This and all the other transformations are implemented using the same pattern
to perform a top-down traversal of the AST, namely using Uniplate's `descendBi`
on the top level with a function that has a case where the optimisation can be
applied and a general case, which uses `descend` with the function itself to
continue the recursion. Using `descendBi` allows to apply the optimisation the
top level instead of skipping the top level and recursing down directly. We
direct the reader to the Uniplate documentation for some related details.

In the implementation above we look for a $bind$ expression which has $return$
as a binder expression and substitute all of the occurences of the bound
variable in the $bind$ body with the expression under $return$ (applying the
transformation recursively to the subexpressions) using a helper function
`replaceExprIn`.

The following is an MIL example containing the original code and then the
resulting code after applying the transformation:

~~~
let (x : Unit) <- return [Id] unit
in return [Id] x;

==>

return [Id] unit;
~~~

### Right identity

The second law allows us to eliminate redundant $bind$ and a subsequent
$return$:

$$bind\ m\ return = m$$

An implementation of this transformation looks like follows:

~~~{.haskell}
rightIdentityExpr :: TyExpr -> TyExpr
rightIdentityExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e2 of
        ReturnE _tm (VarE vb) | vb == varBinder -> rightIdentityExpr e1
        _ -> LetE varBinder (rightIdentityExpr e1) (rightIdentityExpr e2)
    f expr = descend f expr
~~~

Here we look for a $bind$ containing $return$ of the bound variable as a body.
If such an expression is found, it is replaced with the binder expression (with
the transformation applied on top).

An example for this transformations looks pretty much the same as for the left
identity, but what is really happening is that it is only the whole binder
expression which is left (instead of subtituting `x` with `1` in the body):

~~~
let (x : Int) <- return [Id] 1
in return [Id] x;

==>

return [Id] 1;
~~~

### Associativity

The last of the three monad laws is associativity. This law was presented as
the following:

$$bind\ (bind\ m\ f)\ g = bind\ m\ (\lambda x \to bind\ (f\ x)\ g)$$

The associativity transformation is implemented as in the snippet below:

~~~{.haskell}
associativityExpr :: TyExpr -> TyExpr
associativityExpr = descendBi f
  where
    f (LetE varBinder e1 e2) =
      case e1 of
        LetE varBinder' e1' e2' ->
          LetE varBinder' (associativityExpr e1')
            (LetE varBinder (associativityExpr e2') (associativityExpr e2))
        _ -> LetE varBinder (associativityExpr e1) (associativityExpr e2)
    f expr = descend f expr
~~~

The following example shows a simple MIL code snippet and its transformed
version:

~~~
let (y : Unit) <-
  let (x : Unit) <- return [Id] unit
  in return [Id] x
in return [Id] y;

==>

let (x : Unit) <- return [Id] unit
in let (y : Unit) <- return [Id] x
in return [Id] y;
~~~

We can categorise the associativity transformation as one that *enables other
transformations* (using the taxonomy of machine-independent transformations
from \cite{EngineeringCompiler}). This transformation allows to restructure the
code and make it more linear, thus enabling other transformations to
potentially improve the code further. For example, identity laws usually can be
applied more times after the associativity has been applied.

## Lift transformations

The next couple of transformations are related to the monad transformer $lift$
operation. Both of them are trying to eliminate redundant $lift$ operations.
Whether these transformations can be considered optimisations really depends on
the code that is generated for $lift$. On one hand, $lift$ clearly has
implementations for different monad transformers in Haskell, but on the other,
one can think that $lift$ can be erased after the MIL stage (for example, when
generating LLVM or machine code). We could accept that a program in LLVM or
machine code simply allows all the effects all the time, and MIL’s type system
ensured that everything is well typed.

### Identity

The first one, which we call identity, can remove a $lift$ where the source and
the target monads are the same:

$$lift\ [M_1 \Rightarrow M_2]\ e = e, where\ M_1 \equiv_\alpha M_2$$

The next example is probably the simplest case of applying this transformation:

~~~
lift [Id => Id] return [Id] unit;

==>

return [Id] unit;
~~~

### Composition

The second $lift$ transformation that is implemented for MIL is trying to
replace a sequence of two $lift$ operations with one. In such a sequence of
$lift$s the target of the inner (second) one is compatible with the source of
the outer (first) one, which is guaranteed by the type/lint checking:

$$lift\ [M_2 \Rightarrow M_3]\ lift\ [M_1 \Rightarrow M_2]\ e = lift\ [M_1 \Rightarrow M_3]\ e$$

The next example demonstrates a composition of two $lift$ operations:

~~~
lift [IO ::: State => Error Unit ::: (IO ::: State)]
  lift [State => IO ::: State] return [State] unit;

==>

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

The first transformation allows to reorder two computations.  Such
transformation is not obviously beneficial, but it can be used to enable other
transformations or when it comes to low level considerations, may improve
memory locality or parallelisation:

$$let\ (x : T_1) \gets e_1\
  in\ let\ (y : T_2) \gets e_2\ in\ e$$
$$=$$
$$let\ (y : T_2) \gets e_2\
  in\ let\ (x : T_1) \gets e_1\ in\ e,$$
$$where\ x\ is\ not\ used\ in\ e_2\ and\ Monad\ is\ Id$$

Here is an implementation of this transformation:

~~~{.haskell}
exchangeExpr :: TyExpr -> TyExpr
exchangeExpr = descendBi f
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
    f expr = descend f expr
~~~

Another transformation, which can be used inside the `Id` monad, but not, for
example, inside the `IO` monad in the general case is the elimination of
redundant code:

$$let\ (x : T) \gets e\ in\ e' = e',\ where\ x\ is\ not\ used\ in\ e'\ and\ Monad\ is\ Id$$

We skiped giving examples in these cases, since they would not be particularly
interesting.

### State

Next, we will describe four transformations applicable to computations inside
the `State` monad. One thing to note when looking at these transformations is
that there are no explicit checks for a monad being `State`. This is redundant,
since the type system of MIL ensures that the state operations are used only
inside the monad with `State`.

The first one is a special case of the reordering transformation presented in
the previous section. We refer to it as "exchange new". It allows to reorder
creation of two references:

$$let\ (x : Ref\ T_1) \gets new\_ref\ [T_1]\ e_1\
  in\ let\ (y : Ref\ T_2) \gets new\_ref\ [T_2]\ e_2\ in\ e$$
$$=$$
$$let\ (y : Ref\ T_2) \gets new\_ref\ [T_2]\ e_2\
  in\ let\ (x : Ref\ T_1) \gets new\_ref\ [T_1]\ e_1\ in\ e,$$
$$where\ x\ is\ not\ used\ in\ e_2$$

In this transformation we are looking for a sequence of $bind$ operations with
`new_ref` as their binder expressions. It does a check for usage of the
variable that holds the reference created with the first `new_ref`. This is
done to avoid applying this transformation in the case, when the first
reference is used to create the second reference.

Another special case of the reordering transformation is "exchange read", which
can reorder reading of two references:

$$let\ (x : T_1) \gets read\_ref\ [T_1]\ e_1\
  in\ let\ (y : T_2) \gets read\_ref\ [T_2]\ e_2\ in\ e$$
$$=$$
$$let\ (y : T_2) \gets read\_ref\ [T_2]\ e_2\
  in\ let\ (x : T_1) \gets read\_ref\ [T_1]\ e_1\ in\ e,$$
$$where\ x\ is\ not\ used\ in\ e_2$$

It is very similar to the "exchange new" transformation. It also does a check
for usage of the variable that holds the value read from the first reference.
This is done to avoid applying this transformation in the case, when the first
reference contains another reference that is read in the second `read_ref`.

The third transformation for `State` computations allows to eliminate a
reference reading in the case when this reference has already been read and
bound to a variable:

$$let\ (x : T) \gets read\_ref\ [T]\ r\
  in\ let\ (y : T) \gets read\_ref\ [T]\ r\ in\ e$$
$$=$$
$$let\ (x : T) \gets read\_ref\ [T]\ r\
  in\ let\ (y : T) \gets return\ [State]\ x\ in\ e$$

Again, we look for a sequence of two `read_ref`s as in the previous
transformation, but here we also check that the same reference is read (it
works only when the reference is bound to a variable). In this case, `read_ref`
is replaced with $return$ of the variable that is bound to the result of the
first `read_ref`.

Below is a small example that instead of reading the reference `x` again,
reuses the value of the variable `a`, which already contains the value of `x`:

~~~
let (x : Ref Int) <- new_ref [Int] 1
in let (a : Int) <- read_ref [Int] x
in let (b : Int) <- read_ref [Int] x
in return [State] unit;

==>

let (x : Ref Int) <- new_ref [Int] 1
in let (a : Int) <- read_ref [Int] x
in let (b : Int) <- return [State] a
in return [State] unit;
~~~

The last `State` transformation also eliminates a reference reading, but in
this case the information from a reference writing operation is used:

$$let\ (u : Unit) \gets write\_ref\ [T]\ r\ c\
  in\ let\ (x : T) \gets read\_ref\ [T]\ r\ in\ e$$
$$=$$
$$let\ (u : Unit) \gets write\_ref\ [T]\ r\ c\
  in\ let\ (x : T) \gets return\ [State]\ c\ in\ e$$

This transformation is very similar to the previous one, except for that the
first operation must be `write_ref` in this case.

The following is an example of reusing an expression that was written to a
reference instead of reading the reference:

~~~
let (x : Ref Int) <- new_ref [Int] 1
in let (z : Unit) <- write_ref [Int] x 2
in let (a : Int) <- read_ref [Int] x
in return [State] unit;

==>

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
the handler part will definitely be executed:

$$catchName\ [Unit]\ [T]\ (throw\_error\ [Unit]\ [T]\ unit)\ (\lambda (err : Unit) \to e)$$
$$=$$
$$e,$$
$$where\ catchName == catch\_error\_1\ or\ catchName == catch\_error\_2$$

It tries to find an application of `catch_error_1` or `catch_error_2` with
`throw_error` as the first non-type argument. This transformation is simplified
by the fact that it looks only for the case when the type of error values is
`Unit`.

This optimisation is shown in the following code snippet:

~~~
catch_error_1 [Unit] [Int]
  (throw_error [Unit] [Int] unit)
  (\(e : Unit) -> return [Error Unit] 1);

==>

return [Error Unit] 1;
~~~

## Case expression transformations

In this section we present two transformations around `case` expressions which
is an important part of implementing conditional and pattern matching
constructs in source languages.

### Constant case elimination

Constant case elimination transformation allows to remove a `case` expression,
which has a known outcome (because of literal patterns and a literal
scrutinee):

$$case\ s_i\ of\ |\ s_j \Rightarrow e_j\ end$$
$$=$$
$$e_i,$$
$$where\ s\ is\ literal\ or\ data\ constructor$$

It tries to find literal or data constructor scrutinee expressions, which are,
basically, constant values and then to find a case alternative with a pattern
corresponding to such a value.

An example of applying this transformation is presented below:

~~~
fun : State Int =
  case 1 of
    | 0 => return [State] 0
    | 1 => return [State] 1
    | _ => return [State] 2
  end;

==>

fun : State Int = return [State] 1;
~~~

This transformation can be applied to eliminate unnecessary `when` statements
in OOLang, when it is known that a condition evaluates to `true` or `false`.

### Common bind extraction

The common bind extraction is a transformation that can *hoist* a $bind$ to the
"same variable" out of `case` alternatives, given that the $bind$s have the
"same body expression". By the word "same" here we mean alpha-equivalence. The
implementation of this transformation in MIL is more naive and uses simple
equality both for variables and for body expressions. The transformation is
presented below:

$$case\ s\ of\ |\ p_i \Rightarrow let\ (v_i : T) \gets e_i\ in\ e\ end$$
$$=$$
$$let\ (v : T) \gets case\ s\ of\ |\ p_i \Rightarrow e_i\ end\ in\ e,$$
$$where\ all\ occurrences\ of\ v_i\ in\ e\ are\ substituted\ with\ v$$

The following is an example of applying the common bind extraction:

~~~
case 1 of
  | 0 => let (x : Int) <- return [IO] 0 in return [IO] unit
  | 1 => let (y : Int) <- return [IO] 1 in return [IO] unit
end;

==>

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
require a more detailed analysis. This transformation is using the following
identities between the MIL built-in functions and mathematical operations
($l_i$ are literals):

$$add\_int\ l_1\ l_2 = l_1 + l_2$$
$$add\_float\ l_1\ l_2 = l_1 + l_2$$
$$sub\_int\ l_1\ l_2 = l_1 - l_2$$
$$sub\_float\ l_1\ l_2 = l_1 - l_2$$
$$mul\_int\ l_1\ l_2 = l_1 * l_2$$
$$mul\_float\ l_1\ l_2 = l_1 * l_2$$

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

==>

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

==>

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

## Discussion

Correctness is often considered "the single most important criterion that a
compiler must meet" \cite{EngineeringCompiler}. This, of course, includes
optimisations. Safety is extremely important when designing and applying code
transformations. We can see that MIL type and effect system is of great help
here. It plays a role in implementing transformations and being able to quite
easily find out effect information or make assumptions based on types about a
piece of code instead of performing additional analysis to derive these facts,
which are not present in the IR (which is usually the case for most IRs).

In this thesis we tried to work with quite simply looking algebraic identities
for expressing code transformations. Such identities are usually very minimal
and very general at the same time, e.g. monad laws. This makes the
implementation of optimisations based on them only a dozen lines of code in
size. We also want to highlight that most industrial compilers are *not*
implemented in a functional language, especially not in such an expressive and
concise one as Haskell, while MIL, being a Haskell library, certainly benefits
from this. Classical optimisations often use additional data structures (for
example, sets) to maintain certain information about the program. It is
certainly possible to implement more powerful and sophisticated transformations
for MIL using a more algorithmic approach to the problem.

One example of this is recognising equivalent `case` expressions and hoisting
them to perform the matching only once. An interesting observation to make here
is that this kind of optimisation is performed on a functional language
construct, but it can make consequent method calls on the same object in OOLang
more efficient, given the way how those are compiled. Optimising method calls
is one of the most important targets for OO-language compilers
\cite{ModernCompilerDesign}.

Some of the optimisations presented here try to meet the same goals as the
*value numbering* method \cite{EngineeringCompiler}, \cite{Muchnick}, namely
eliminating some redundancy in computations. MIL transformations like "use
read", "use write" are more specialised and simple, while value numbering is a
more general and more complex technique. MIL certainly could benefit from using
value numbering to eliminate more redundancies (see examples at the end of this
section).

One of the major well-known approaches to compiler optimisations nowadays is
*data-flow analysis*. Examples of data-flow optimisations are
*copy-propagation*, *constant-propagation* and *partial-redundancy*
\cite{Muchnick}, \cite{EngineeringCompiler}. Data-flow analyses mainly focus on
how values flow through the program and not on what effects can happen (which
is one of the strongest properties of MIL), although this still needs to be
handled somehow. Data-flow techniques should be possible to apply to MIL as
well. One possible way to go would be to incorporate Hoopl -- a Haskell library
for dataflow analysis and transformation \cite{Hoopl}.  However, an important
consideration and a potential obstacle with this is that very many common
optimisations are using the notions of *control-flow graph (CFG)* and *basic
blocks* \cite{Muchnick}.  MIL, on the other hand, being a functional language,
is *expression-based*. It could be considered to transform MIL further to
something more low-level and imperative to benefit from more classical
optimisations.

We will conclude with two examples that show some of the MIL strengths and
weaknesses and its relation to the classical methods mentioned above. The
examples are mainly based on the value numbering example from Chapter 8 in
\cite{EngineeringCompiler}.

First, we will demonstrate a redundancy which MIL cannot eliminate, but value
numbering can. Below is a fragment of code that reads several integer values
and then does some arithmetic manipulations and `Mutable` assignments in
OOLang. One can notice that there is a common subexpression, namely `a - d`,
which could be computed once.

~~~
b : Mutable Int <- readInt;
c : Mutable Int <- readInt;
d : Mutable Int <- readInt;
a : Mutable Int <- b + c;
b <- a - d;
printInt b;
c <- b + c;
d <- a - d;
printInt d;

==>

let (b : Int) <- readInt
in let (c : Int) <- readInt
in let (d : Int) <- readInt
in let (var_21 : Unit) <- printInt (sub_int (add_int b c) d)
in printInt (sub_int (add_int b c) d);
~~~

Looking at the generated (and optimised) MIL code, one can see that `b + c - d`
is present twice (note that the expression for `a` was propagated to both its
use sites).  This is something that value numbering would be capable of
eliminating, effectively rewriting `d <- a - d;` to `d <- b;`.

The second example is very similar to the first one, but there is an important
difference: the integers are not read, but just have constant values. We
introduced print statements to make sure that the code is not optimised away
completely. Another important consideration is that we disabled all
optimisations, except for monad laws for this example. It is especially
important that constant folding is disabled, because otherwise almost
everything would have been computed at compile time.

~~~
b : Mutable Int <- 1;
c : Mutable Int <- 2;
d : Mutable Int <- 3;
a : Mutable Int <- b + c;
printInt a;
b <- a - d;
printInt b;
c <- b + c;
printInt c;
d <- a - d;
printInt d;

==>

let (var_2 : Unit) <- printInt (add_int 1 2)
in let (var_5 : Unit) <- printInt (sub_int (add_int 1 2) 3)
in let (var_8 : Unit) <- printInt (add_int (sub_int (add_int 1 2) 3) 2)
in printInt (sub_int (add_int 1 2) 3);
~~~

Looking at the generated (and optimised) MIL code one can see that monad laws
in this case did pretty much what constant and copy propagation techniques
mentioned above are designed to do.

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
by implementing some kind of effect inference/elimination process mentioned in
Chapter 5.

