# Related work

> *Usage of monads in compiler's intermediate representations is quite
> unexplored. At the same time, nowadays the interest in reasoning about
> effects of a program and their control is significant. First, we will look at
> several intermediate languages which use monads. Then we will describe
> examples of monad transformers' usage to achieve modularity with effects. And
> finally we will cover some related work in programming with effects, which
> may serve as a source of inspiration for intermediate representations as well
> as reasoning about program effects in general.*

## Intermediate representations based on monads

First, we will describe some of the ideas behind an intermediate language
proposed in "Bridging the gulf" \cite{BridgingTheGulf}. The main intention of
the work was to design a common intermediate language for compiling ML and
Haskell programming languages. One of the main differences between these two
languages is that ML has strict evaluation and Haskell is a lazy language. In
addition to this, Haskell is a pure language, while ML is not. Therefore, the
main challenge was to have an IL that will work equally well for both. Note,
that this setting and the problem are slightly different from what this thesis
tries to address, but nevertheless it is still very interesting to see one of
the not so many monadic IRs and the considerations authors had when designing
it. Certain ground rules for an IL are given, namely: ability to translate both
core ML and Haskell, explicitly typed with a type system that supports
polymorphism (a variant of System F \cite{SystemF}), well-defined semantics,
efficiency of compiled programs comparable to those produced by good ML and
Haskell compilers. Authors propose two designs of such an IL. Both of them are
based on monads. The idea behind the first IL is to be explicit about things
that are usually implicit in the IL, but are explicit in the denotational
semantics of it. There are two monads in this IL, lifting (for denoting
possibly-diverging computations) and the combination of lifting with the state
transformer $ST$ (to distinguish between pure and stateful computations). The
main reason for using the lifting monad is to express whether a function takes
an argument, which is evaluated or not. Semantics of the monads is made
implicit in the language. Unfortunately, the first proposed design turned out
to be vague about the timing and degree of evaluation and solutions to these in
the presence of polymorphism are complicated and therefore, authors conclude
that it is unsuitable as a compiler IL. The second IL design introduces a
distinction between value types (for variables) and computation types (for
expressions), let expression becomes eager and new syntactic forms (evaluation
suspension and forcing) are introduced, thus making the control of evaluation
explicit. $Lift$ monad becomes implicit (and covered by types distinction and
new syntactic forms) and so there is only $ST$ monad left. Still having this
monad explicit allows to express that certain computations are free from
side-effects and perform useful transformations based on this information,
which is recorded in the program itself. The second design seems to address the
problems found in the first one, but as the authors highlight, the ability of
the first IL design to capture certainly-terminating computations is very
useful and they would like to incorporate it into the second design. The
problem of combining monads was not addressed in "Bridging the gulf".  Several
monad transformation rules are given, but they were not in the main focus of
this paper.

Next, let's look at an intermediate language from "Optimizing ML Using a
Hierarchy of Monadic Types" \cite{Tolmach}. The main property of this IL is
that it has a fixed hierarchy of monadic types (every next monad includes the
effects of the previous ones):

* $ID$ (for pure, terminating computations)
* $LIFT$ (for potentially non-terminating computations)
* $EXN$ (for computations that may raise an exception)
* $ST$ (for stateful computations).

In addition to the classical monad operations ($bind$ and $return$), an
embedding operation named $up$ is introduced, which corresponds to the $lift$
operation for monad transformers, introduced in the previous chapter. An
interesting decision problem about the combination of state and exception
arises (it is an example of a general problem of monad transformers composition
mentioned in the previous chapter). The author chooses exception handling that
does not alter the state (as opposed to reverting the state on exception
handling). There is a number of generalised monad laws and code motion laws for
monadic expressions given in the paper. Another valuable contribution of
Tolmach's work is a monad inference algorithm for computing the minimal monadic
effect of subexpressions, which allows to perform many more useful
transformations comparing to assigning the maximal effect. The author makes
several supporting claims for a monadic intermediate language: its usage in
organising and justifying a variety of optimizing transformations in the
presence of effects, reflecting the results of the effect inference in the
program itself as well as keeping them up to date as different optimisations
progress (as opposed to using some separate data structure for this). At the
time of the paper's appearance there was no evidence on whether the results
help to achieve any significant performance improvements for generated ML code,
but some measurements were ongoing. At the time of this thesis writing no
published results of these measurements have been found.

The next work we are going to describe is an intermediate language MIL-lite
from "Monads, Effects and Transformations" \cite{Benton}. MIL-lite is a
fragment of the monadic intermediate language used in the MLj compiler. A big
part of the paper is devoted to semantics of effects, but we are going to
concentrate on the IL design and the approach to expressing effects. Most terms
of the language are straight-forward terms found in functional languages.
MIL-lite is quite minimal and some syntactic sugar is expressed in terms of
this core.  There is a novel construct that is introduced: try-catch-in. One
can view it as a blend of monadic bind (let) and exception handling. The
authors claim that this new construct is suitable for expressing many
optimising transformations and is very general. let expression is actually
expressed in terms of try-catch-in.  A distinction between value types and
computation types is made similarly to \cite{BridgingTheGulf}.  A computation
type is effectively a value type with effects. The following effects are
covered: non-termination, reading from a reference, writing to a reference,
allocating a new reference and raising an exception. Note, how fine-grained the
effects for stateful computations are.  Effects are combined using sets and
inclusion of these sets introduces a subtyping relation. All possible
exceptions are also included in the set of effects. Authors provide a number of
effect-independent and effect-dependent equivalences and use the reasoning
about semantics of effects to prove some of them.

The GRIN project

## Monad transformers and modular effects

\cite{MonadTransformers}
\cite{ModularSemanticsCC}

\cite{ModularCompilersForEffects}
\cite{AlaCarte}

\cite{ModularCompilersTransformers}
\cite{ModularCompilersProofs}

## Programming with effects

There is a growing interest in bringing controlled and expressive effects into
programming languages.

Koka programming language \cite{Koka}

* Effect inference

Algebraic effects \cite{Brady}

* Dependent types

Extensible effects \cite{ExtensibleEffects}

* An alternative to monad transformers

One of the most recent ideas in expressing effects is a *polymonad*.

TODO: Missing some effect systems references?

