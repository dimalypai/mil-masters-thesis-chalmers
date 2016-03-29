# Abstract

\textbf{Monadic Intermediate Language for Modular and Generic Compilers}

DMYTRO LYPAI

Department of Computer Science and Engineering

Chalmers University of Technology and University of Gothenburg

\vspace{\baselineskip}

Most modern compilers perform a number of sophisticated program analyses and
transformations, with their correctness and safety being of extreme importance.
Intermediate representations used in compilers play a crucial role in defining
how these procedures will be performed: what is possible to express and a level
of effort required. A variety of computational effects present in modern
programming languages imposes additional challenges on compiler writers.

This thesis starts with a brief introduction to monads and monad transformers
and their relation to programming languages. A survey of the existing
intermediate languages based on monads as well as highlights of the
current developments in programming with effects are presented.

It continues with the main contribution of this work: Monadic Intermediate
Language (MIL), which is a statically and explicitly typed functional language,
which uses monads and monad transformers to express different computational
effects and their combinations. To evaluate the designed intermediate
language, two source languages representing two major programming
paradigms: object-oriented and functional, have been designed and compilers
targeting MIL have been implemented. Strengths and weaknesses of MIL uncovered
during the implementation of the source programming languages are described in
detail.

We conclude with describing code transformations and optimisations implemented
for MIL, including very general effect-independent algebraic equivalences, such
as, for example, monad laws, as well as a number of effect-dependent
transformations. Relations to some of the well-known state-of-the-art
optimisation techniques are outlined.

The ideas described in this thesis are implemented as three separate Haskell
packages (`mil`, `funlang` and `oolang`), which are open source and freely
available online.

\vspace{\baselineskip}

\textbf{Keywords:} *compilers, intermediate representations, intermediate
languages, computational effects, monads, monad transformers*

