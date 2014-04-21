% Planning Report\
  "Monadic Intermediate Language for Modular and Generic Compilers"
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)
% February 3, 2014

Background to the assignment
============================

The vast majority of compilers for a variety of programming languages make use
of different intermediate representations during the compilation process.
Intermediate representation (language) is a data structure built from a source
program. Classic examples are: three-address code, Register Transfer Language
(RTL), static single assignment (SSA) form and many others. Compilers can use
different representations of a source program during different compilation
stages, depending on properties that these representations have and how
suitable they are for a particular stage (for example, easy to produce from a
source code, easy to transform, easy to generate code from etc.). Most of the
transformations and optimisations are performed on intermediate
representation(s) of a program.

The problem formulation
=======================

One of the most important and difficult considerations during different
compilation stages are the effects that program fragments might cause. They
have a significant influence on the kinds of analyses and transformations that
may be performed. This is particularly important because semantic analysis and
optimisations are two major parts of modern compilers.

Aim for the work
================

Monads are proved to be a very powerful and generic way to encapsulate and
express different effects (such as state, exceptions, non-termination etc.) as
described in \cite{MonadsAndEffects, MonadsForFP}. The state of the art example
is the Haskell programming language. Researchers also explored using monads and
monad transformers (a way to combine different monads) for building modular
interpreters and compilers \cite{MTandMI, MDSforCC, MConMT, TMCforE}. Using
monads in intermediate representation enables a number of other possibilities.
There are certain properties of monads (such as monad laws) that allow to
perform very interesting program transformations that may influence the
performance and other important properties of the resulting code significantly
\cite{MonadsEffectsAndTrans, OptML}.

Incorporating monads in intermediate representation for the compiler allows to
describe different kinds of effects in a modular way. Different combinations of
monads are suitable for programming languages with different semantics and
features \cite{BridgTheGulf}. Building generic framework based on monads allows
to express several monadic intermediate languages suitable for particular
compilation stages ranging from high-level representations of source languages,
where we would like to operate in terms of exceptions, computations with
limited side-effects etc. to more low-level representations, which are more
suitable for code generation, where the effects are mainly stores and jumps.

Therefore, the main goal of the project is to design a monadic intermediate
language to be used as compiler intermediate representation and to build a
programming framework for working with this representation that can be used for
building compilers.

As a demonstration of using the designed monadic intermediate language, two
programming languages (object-oriented and functional) should be designed and
compilers for them should be built.

Part of the work is also to evaluate the suitability of the designed
intermediate representation for performing analyses and optimisations for
different languages that support different programming paradigms.

Limitations
===========

Unfortunately, there are time, resource and complexity limitations that force
to leave out many of the possible things that could be done and that would be
interesting to have:

* Source languages that need to be designed as part of the project should have
  a rather minimal set of features that are useful for demonstration. We need
  to leave out many useful features that any modern programming language should
  have.
* While modern software and especially such complex projects as compilers need
  to be modular, there are certain limits of modularity and generality that one
  can reach. In this project we try to achieve modularity in combining of
  effects, but working with a fixed set of effects that may be used and
  combined. The next step might be to abstract over effects completely and let
  user of the framework to supply and describe new effects. On the other hand,
  this might turn out to be overly complicated and not worth it. We also don't
  try to achieve the best modularity possible in type checking and code
  generation for different language features like in Compilation a la Carte and
  \cite{TMCforE}, for example.
* Any modern compiler ideally should target many different platforms, but this
  is very time-consuming and is not essentially related to the problem we are
  trying to solve. This will be partly solved by targeting LLVM which can
  generate code for many different targets.
* There are so many different optimisations that one can implement in a
  compiler. To meet the time requirements, we need to choose only the most
  relevant ones.
* It would be very interesting to explore compiling lazy programming language
  to the designed intermediate representation. But this is a separate problem
  in itself and was studied by Peyton Jones et al. \cite{BridgTheGulf}.
  Following these results it should not be very hard to incorporate laziness in
  the design MIL, although some problems may arise.
* There interesting programming paradigms and techniques that can be expressed
  with monads and so are directly related to this project. For example, an
  interesting problem to work on would be to compile a logic programming
  language such as Prolog using something like Backtracking or Logic monad.
  There are also different monads for parallelism and concurrency, for example,
  Par monad and STM monad. This is a very important topic nowadays and modern
  programming languages need to support parallel and concurrent programming in
  some way. Using such monads in intermediate language might be a good way of
  compiling such languages.
* This thesis is mostly related to statically typed source and intermediate
  languages, but dynamically typed languages are a big and important part of
  modern programming toolchain, especially for the Web. Efficient compilation
  of dynamic languages requires inferring static type information from the
  program. It might be beneficial to explore using the designed intermediate
  representation for compilation of dynamically typed languages.

Method of accomplishment
========================

The main method of accomplishment for this work is an experimentation based on
implementation. Studying, literature survey and preliminary design should be
followed by the implementation phase. Implementation of the compilers and
intermediate language framework should be done incrementally with planned
refinement of the designs of source languages and monadic intermediate
language. The main benefit of this approach is that the implementation process
requires to work out all the details and refine the design if needed.
Incremental development should allow to prioritise parts of the development and
to leave out features painlessly if needed in order to meet time constraints.

Time plan
=========

* Design monadic intermediate language - 3 weeks (2014-02-03 - 2014-02-24)
* Design object-oriented programming language - 1 week (2014-02-24 - 2014-03-03)
* Design functional programming language - 1 week (2014-03-03 - 2014-03-10)
* Refine MIL - possible intermediate step
* Build compiler - 8 weeks (2014-03-10 - 2014-05-05)
* Refine MIL - possible intermediate step
* Study and implement optimisations - 3 weeks (2014-05-05 - 2014-05-26)
* Refine MIL - possible intermediate step
* Final report writing - 4 weeks (2014-05-26 - 2014-07-01)

Master's Thesis opposition:

* 2014-03-21 - "Stack Traces in Haskell", Arash Rouhani

Planned attending of Master's presentations:

* 2014-04-28 - "Information Flow in Databases for Free", Daniel Schoepe
* 2012-05-22 - "Implementing incremental and parallel parsing", Tobias Olausson

\bibliographystyle{plain}
\bibliography{Proposal}

