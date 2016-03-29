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

### Common IL for ML and Haskell

First, we will describe some of the ideas behind an intermediate language
proposed in "Bridging the gulf" \cite{BridgingTheGulf}. The main intention of
the work was to design a common intermediate language for compiling ML
\cite{ML} and Haskell \cite{HaskellReport} programming languages. One of the
main differences between these two languages is that ML has strict evaluation
and Haskell is a lazy language. In addition to this, Haskell is a pure
language, while ML is not. Therefore, the main challenge was to have an IL that
will work equally well for both. Note that this setting and the problem are
slightly different from what this thesis tries to address, but nevertheless it
is still very interesting to see one of the not so many monadic IRs and the
considerations authors had when designing it. Certain ground rules for an IL
are given, namely: ability to translate both core ML and Haskell, explicitly
typed with a type system that supports polymorphism (a variant of System F
\cite{SystemF}), well-defined semantics, efficiency of compiled programs
comparable to those produced by good ML and Haskell compilers. Authors propose
two designs of such an IL. Both of them are based on monads. The idea behind
the first IL is to be explicit about things that are usually implicit in the
IL, but are explicit in the denotational semantics of it. There are two monads
in this IL, lifting (for denoting possibly-diverging computations) and the
combination of lifting with the state transformer $ST$ (to distinguish between
pure and stateful computations). The main reason for using the lifting monad is
to express whether a function takes an argument, which is evaluated or not.
Semantics of the monads is made implicit in the language. Unfortunately, the
first proposed design turned out to be vague about the timing and degree of
evaluation and solutions to these in the presence of polymorphism are
complicated and therefore, authors conclude that it is unsuitable as a compiler
IL. The second IL design introduces a distinction between value types (for
variables) and computation types (for expressions), let expression becomes
eager and new syntactic forms (evaluation suspension and forcing) are
introduced, thus making the control of evaluation explicit. The $Lift$ monad
becomes implicit (and covered by types distinction and new syntactic forms) and
so there is only the $ST$ monad left. Still having this monad explicit allows to
express that certain computations are free from side-effects and perform useful
transformations based on this information, which is recorded in the program
itself. The second design seems to address the problems found in the first one,
but as the authors highlight, the ability of the first IL design to capture
certainly-terminating computations is very useful and they would like to
incorporate it into the second design. The problem of combining monads was not
addressed in "Bridging the gulf".  Several monad transformation rules are
given, but they were not in the main focus of this paper.

### Optimizing ML using a hierarchy of monadic types

Next, we will look at an intermediate language from "Optimizing ML Using a
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
arises (it is an example of a general problem of monad transformers' composition
mentioned in the previous chapter). The author chooses exception handling that
does not alter the state (as opposed to reverting the state on exception
handling). There is a number of generalised monad laws and code motion laws for
monadic expressions given in the paper. Another valuable contribution of
Tolmach's work is a monad inference algorithm for computing the minimal monadic
effect of subexpressions, which allows to perform many more useful
transformations comparing to assigning the maximal effect. The author makes
several supporting claims for a monadic intermediate language: its usage in
organising and justifying a variety of optimising transformations in the
presence of effects, reflecting the results of the effect inference in the
program itself as well as keeping them up to date as different optimisations
progress (as opposed to using some separate data structure for this). At the
time of the paper's appearance there was no evidence on whether the results
help to achieve any significant performance improvements for generated ML code,
but some measurements were ongoing. At the time of this thesis writing no
published results of these measurements have been found.

### MIL-lite

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
covered:

* non-termination
* allocating a new reference
* reading from a reference
* writing to a reference
* raising an exception

Note how fine-grained the effects for stateful computations are. Effects are
combined using sets and inclusion of these sets introduces a subtyping
relation. All possible exceptions are also included in the set of effects.
Authors provide a number of effect-independent and effect-dependent
equivalences and use the reasoning about semantics of effects to prove some of
them.

### The GRIN project

The last intermediate representation described in this section is the one from
"The GRIN project" \cite{GRIN}. GRIN (Graph Reduction Intermediate Notation) is
a monadic intermediate code that is used in a back end of a compiler for lazy
functional languages. GRIN resembles three-address code mentioned in the
Introduction. For the monadic part, it has $unit$ operation and $;$ (semicolon)
as a monadic bind. The monad is a kind of State monad with a heap as an
underlying storage.  There are $store$, $fetch$ and $update$ operations in the
monad.  The authors highlight that the monadic structure gives GRIN a very
"functional flavour" and therefore a nice setup for doing analysis and
transformations.

## Monad transformers and modular effects

In this section we will look at work that has been done in the area of monad
transformers and using them to combine different effects. Note that in these
cases monad transformers are used mainly in the implementation language, rather
than in an IR, as described below. It is a bit different area of monad
transformers' application compared to this thesis, but it still serves as a
great source of inspiration.

Probably, one of the most influential papers in this area is "Monad
Transformers and Modular Interpreters" \cite{MonadTransformers}. It describes
how to structure an interpreter for a programming language, where language
features are "pluggable" and the evaluation of each feature is implemented
separately (in different type class instances in Haskell, in this case).
Features have a strong relation to the effects performed (where effects
correspond to monad transformers). By adjusting the order of monad transformers
in the stack, one can choose different semantics of the language. Another key
idea that allows this kind of implementation is *extensible union types*, which
is used to specify different parts of the language's AST (abstract syntax tree)
in a modular way. A continuation of this work "Modular Denotational Semantics
for Compiler Construction" \cite{ModularSemanticsCC} applies the results to
define a modular monadic semantics and use it for a compiler. Authors describe
usage of monad laws to transform programs and reason about them.

What was not addressed in the work described above in particular is more
low-level code generation. So, later, Harrison and Kamin in
\cite{ModularCompilersTransformers} demonstrate a compiler structured in blocks
for every feature, where a block is a compilation semantics for a feature and
its associated monad transformers. They use partial evaluation of monadic
expressions to generate code. Harrison's thesis extends this work and includes
correctness proofs for such compilers \cite{ModularCompilersProofs}.

Another notable work on modular compilers is by Day and Hutton
\cite{ModularCompilersForEffects}. To structure the language's syntax in a
modular way, the authors used an approach know as *"data types á la carte"*
\cite{AlaCarte}, which is somewhat similar to extensible union types mentioned
earlier. On top of this they build a modular evaluator using monad
transformers, but the decision about the underlying monad, to which
transformers are applied is deferred until the application of top-level
evaluation function. Then a modular compiler for a stack machine is described.
The interesting point here is that the compilation scheme does not utilise a
monad, since the compilation process itself is not connected to program
effects. The authors conclude with a modular virtual machine that can execute
compiled code.

## Programming with effects

There is a growing interest in bringing controlled and expressive effects into
programming languages. This section is by no means a comprehensive survey of
the current state of effect systems, supporting libraries and programming
languages, but rather a few examples of different directions that are being
explored.

### Koka programming language

First, we will look at the Koka programming language \cite{Koka}, which is a
function-oriented language with JavaScript-like syntax. One of the main
features of Koka is that the effect of every function is automatically
inferred. The supported effects are the following:

* $total$ (pure mathematical functions)
* $exn$ (throwing exceptions)
* $div$ (non-termination)
* $ndet$ (non-deterministic functions)
* $alloc<h>$ (memory allocation in a heap $h$)
* $read<h>$ (reading of a heap $h$)
* $write<h>$ (writing to a heap $h$)
* $io$ (input/output operations)

The effect system is based on row polymorphism. This enables the fact that
effects can be combined into rows, for example $<exn, div>$ means that a
function can throw an exception and can diverge, which is basically the
Haskell's notion of purity, but more than that, effects can be polymorphic,
like $<exn, div | E>$, where $E$ is an effect variable.

### Polymonads

One of the most recent ideas in expressing effects is a *polymonad*, which is a
generalisation of monads \cite{Polymonads}. The main idea is that polymonads
give the monadic $bind$ a more general type:

$$polyBind :: L\ a \to (a \to M\ b) \to N\ b$$

Comparing to the monadic $bind$:

$$bind :: M\ a \to (a \to M\ b) \to M\ b$$

Polymonadic bind allows to compose computations with three different types
instead of one. Similar to monads, polymonads must satisfy a number of laws.
Polymonads allow to express different type and effects systems and information
flow tracking, as an example.

An interesting example of what polymonads can model is *contextual effects*.
Hicks et al. describe them as effects "which augment traditional effects with
the notion of *prior* and *future* effects of an expression within a broader
context". An example they give is a system where memory is partitioned into
regions and read and write operations have a region as part of their effect.
Then a sequence of read operations, for example, can capture which regions were
read before and after every operation.

### Eff programming language

The next example of programming with effects is another programming language,
called Eff \cite{Eff}. In Eff effects are viewed as algebraic operations and
they together with their handlers have first-class support. This is an approach
known as *algebraic effect handlers* introduced by Plotkin and Power
\cite{Plotkin}.  The language is statically typed and supports parametric
polymorphism and type inference. In addition to the usual types, Eff also has
effect types and handler types. An effect type denotes a collection of related
operations, a handler type describes that handlers work on computations of one
type and produce computations of some other type. There are several specific
language constructs in Eff, namely *instantiation* of an effect instance ($new\
ref$ or $new\ channel$, for example), *operation*, which can be applied when
bundled with an effect instance, and *handler*, which is somewhat reminiscent
to the pattern matching constructs found in functional languages.  It defines
which computation to perform depending on the evaluation of another
computation, which can result in a value or an effect operation. This is the
place where the semantics of different effects is defined. A handler can be
applied to a computation using the $with-handle$ construct, which reminds the
$try-catch$ construct in many languages. The last of these specific constructs
is a *resource*, which allows to create an effect instance that is associated
with the resource.  In this case resource describes how to handle different
operations for this effect instance by default as well as defining an initial
state. The language constructs, its type system and denotation semantics is
layed out in the paper. The authors also provide many interesting examples,
ranging from stateful computations to transactions, backtracking and
cooperative multithreading. Unfortunately, the Eff's type system does not
capture effects in the types and it is highlighted in the paper that the
language would benefit from a system that provides a static analysis of
effects.

### Algebraic effects and dependent types

Another example of programming with effects based on algebraic effects and
inspired by the Eff programming language is described in \cite{Brady}, which
introduces $Effects$ -- a library for the Idris programming language
\cite{Idris}.  In the general case, there is a type $EffM$ that describes a
program using some *computation context* (which can be a monad, for example,
but it does not have to be), lists of input and output effects as well as the
program's return type.  Programming using $Effects$ from a user point of view
is quite alike monadic programming in Haskell, since, for example, monadic
do-notation is used. Each effect is associated with a *resource*, which can
denote storage for stateful computations, for example. To run an effectful
computation one must specify an initial value of the resource.  To solve the
problem similar to the one solved by lifting in monad transformers, $Effects$
uses *labelled* effects to resolve the ambiguity.  An effect is usually
implemented as an algebraic data type (ADT) as well as an implementation of the
handler for this effect. Handlers can be implemented for specific contexts as
well as for the general case. The author highlights that monads and monad
transformers can express more concepts, but $Effects$ capture many useful use
cases. Implementation of common effects and examples of their usage as well as
the library implementation in Idris are described in details in the paper.

### Extensible effects

The last in this chapter is an example of a library-based approach. Extensible
effects is a library for the Haskell programming language
\cite{ExtensibleEffects}. It is positioned as an alternative to monad
transformers. The central concept of this library is a monad $Eff\ r$, where
$r$ is a type parameter that represents an *open union* of individual effects,
that are combined ($r$ stands for "requests"). One of the main ideas behind the
library is that effects come from communication between a client and an effect
handler (authority), which is quite similar to the algebraic effect handlers
approach. The implementation is described in detail in the paper together with
the examples of implementation of different effects as a user code (meaning, it
can be done outside of the library). The authors provide a detailed analysis of
monad transformers and problems with their expressiveness. One of the
highlights of differences from monad transformers is that effects of the same
type can be combined and as opposed to monad transformers, no explicit lifting
is needed, appropriate operations are found by types. Another difference is
that the order of effects is chosen only when running a computation, not when
defining the computation and in addition to that handled effects are subtracted
from the type. The authors claim that their framework subsumes the Monad
Transformers Library (MTL) in Haskell and allows to express computations, that
are not possible with MTL.

It is worth mentioning that algebraic effect handlers while having a good story
of modularity suffer from performance problems (compared to monad
transformers). As Wu and Schrijvers point out in \cite{FusionForFree},
modularity comes from the fact that syntax and semantics of effects are
separated and so different handlers implementing different semantics can be
provided for the same syntax tree. At the same time, "the construction and
traversal of intermediate trees is rather costly". The work described in
\cite{FusionForFree} attempts to address this problem by trying to fuse several
handlers into a single one and thus improving the performance.

There is also a follow up work on "Extensible effects" aimed at improving the
algorithmic efficiency of the library and its simplification
\cite{FreerMonads}.

