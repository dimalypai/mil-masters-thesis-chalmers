# Conclusions

> *Finally, we will look at the results of this work and outline some ideas for
> future work.*

## Results

In this thesis we tried to focus on the design and implementation of a compiler
IR that would allow to capture program effects such as input/output,
exceptions, state manipulation in programs themselves and provide a way to
express useful optimising code transformations (in the presence of effects)
that can be composed and reused. In addition to this, the ambition was to
explore an ability to provide a way of combining different effects to express
different semantics of source languages.

As the result of this work, we introduced a monadic IR for modern programming
languages -- MIL, which fulfills the goals that were set for this work (the
limitations will be mentioned in the next section). It uses monads to represent
computational effects and monad transformers to combine these effects and allow
to capture different semantics in the ordering of effects. It is possible to
use MIL as a target for languages that belong to two major programming
paradigms: object-oriented and functional. An example of this are two source
programming languages (FunLang and OOLang) that were designed and implemented
as a part of this project.

MIL provides an expressive framework for implementing effect-aware code
transformations. A number of code transformations has been implemented and are
provided with MIL to demonstrate this and to be used in building optimisation
pipelines in source language compilers, which target MIL. By providing support
for the Uniplate library in the MIL AST, the implementations of different
transformations are made clear and concise.

All of the source code for this project as well as this report are freely
available at <https://bitbucket.org/dmytrolypai/mil-masters-thesis-chalmers>
and <https://github.com/dmytrolypai/mil-masters-thesis-chalmers>. It includes
rather comprehensive test suites for MIL, FunLang and OOLang as well as
interactive environments for exploring and experimenting with the source
languages.

## Future work

Finally, we will outline some of the ideas for future work that came up during
the work on this thesis.

Probably, one of the most interesting and important directions in continuing
the work on MIL is the code generation from MIL. We think that the most
appealing choice is to generate code for LLVM, which is a mature and powerful
compiler framework. By generating code for LLVM we could reuse its low-level
generic optimisations and native code generation. Other possibilities for lower
level code generation include generating C code or native code.

Something that we did not address in this project and that is important to do
is to evaluate MIL from the efficiency point of view, namely perform benchmarks
on executable programs. This can be quite easily done given a code generator,
as suggested above. Another way could be to write an interpreter for MIL in a
way that it would be able to record some metrics during the execution, for
example, a number of function applications, $bind$ operations etc.

Another very important direction of MIL development is to make the MIL
representation of effects and their combination more expressive. As was
mentioned in several previous chapters, MIL does not allow to express the
desirable type of the `catch_error` function. Another problem is incorporating
non-termination as one of the built-in effects. The area of polymorphic effects
is worth exploring.

To be able to use more transformations on a practical program MIL needs some
kind of effect inference/elimination procedure to infer the real set of effects
of a program, which might be smaller than what is declared. Implementing such a
process for MIL would allow to optimise programs in source languages targeting
MIL more heavily. A somewhat related area is trying to have more fine-grained
effects, similar to \cite{Benton}, for example, splitting `IO` into `Input` and
`Output` and `State` into `Allocation`, `Reading` and `Writing`.

MIL has a number of built-in monads and it does not provide a way to define new
custom monads and therefore effects. Although the latter could be a subject for
further exploration, we believe that it might be an unnecessary complication
for MIL. The absence of user-defined effects is not a big limitation, given
that MIL is an intermediate language and its goal is to have a reasonable set
of built-in monads that allow to express most of the useful effects in
potential source languages. But what we think might be beneficial is to explore
which other monads can be added to MIL as built-in to be able to more easily
cover a wider range of use cases. As was mentioned in Chapter 2 there exists a
lot of interesting monads. What comes to mind as the most relevant to MIL are
Backtracking and Par monad \cite{ParMonad}. The first one could be used to
express, for example, logic programming languages and the second one could
provide a way to support parallelism in the IR.

Another related track can be to try bringing laziness into MIL. One of the ways
of doing it might be to incorporate ideas from \cite{BridgingTheGulf}, e.g.
introducing language constructs for evaluation suspension and forcing. This
paper was explained in more detail in the "Related work" chapter.

Finally, one could look into a broader area of compiler modularity, in addition
to what MIL already provides in terms of combining effects and pure composable
transformations and their reuse. What we did not focus in this work was trying
to have a modular representation of the AST (similar to the "a la carte"
approach) and using monad transformers in the compilers themselves to compile
different features/effects separately. Addressing this by following the work
described in \cite{MonadTransformers}, \cite{AlaCarte},
\cite{ModularCompilersTransformers}, \cite{ModularCompilersForEffects} could be
a way to cover this area of compiler modularity.

