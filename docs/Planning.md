% Planning Report\
  "Monadic Intermediate Language for Modular and Generic Compilers"
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)
% February 3, 2014

Background to the assignment. Why is it relevant?
=================================================

The vast majority of compilers make use of different intermediate
representations during the compilation process.  Intermediate representation
(language) is a data structure built from a source program.  Classic examples
are: three-address code, Register Transfer Language (RTL), static single
assignment (SSA) form and many others.  Compilers can use different
representations of a source program during different compilation stages,
depending on properties that these representations have and how suitable they
are for a particular stage (for example, easy to produce from a source code,
easy to transform, easy to generate code from etc.).  Most of the
transformations and optimisations are performed on intermediate
representation(s) of a program.  One of the most important and difficult
considerations during different compilation stages are the effects that program
fragments might cause. They have a significant influence on the kinds of
transformations that may be performed.

Monads are proved to be a very powerful and generic way to encapsulate and
express different effects (such as state, exceptions, non-termination etc.).
The state of the art example is the Haskell programming language.  Researchers
also explored using monads and monad transformers (a way to combine different
monads) for building modular interpreters and compilers.  Using monads in
intermediate representation enables a number of other possibilities.  There are
certain properties of monads (such as monad laws) that allow to perform very
interesting program transformations that may influence the performance and
other important properties of the resulting code significantly.

Incorporating monads in intermediate representation for the compiler allows to
describe different kinds of effects in a modular way. Different combinations of
monads are suitable for programming languages with different semantics and
features.  Building generic framework based on monads allows to express several
monadic intermediate languages suitable for particular compilation stages
ranging from high-level representations of source languages, where we would
like to operate in terms of exceptions, computations with limited side-effects
etc.  to more low-level representations, which are more suitable for code
generation, where the effects are mainly stores and jumps.

Aim for the work. What should be accomplished?
==============================================

Building a framework for intermediate representations for compiler construction.

The problem at hand, the assignment.
====================================

Design languages, implement compiler, evaluate.

Limitations. What should be left out and why?
=============================================

* Features of source languages
* Target languages and platforms
* Optimisations

Method of accomplishment. How should the work be carried out?
=============================================================

Implementation based.

Time plan
=========

* Design monadic intermediate language - 3 weeks
* Design object-oriented programming language - 2 weeks
* Design functional programming language - 2 weeks
* Refine MIL
* Build compiler - 6 weeks
* Refine MIL
* Study and implement optimisations - 3 weeks
* Refine MIL
* Final report writing - 4 weeks

