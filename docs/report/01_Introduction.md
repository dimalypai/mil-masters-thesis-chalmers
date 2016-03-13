# Introduction

The vast majority of modern compilers follows more or less the same structure
known for many years, which is called *compiler pipeline*. Compiler pipeline is
a chain of phases, which together form the compilation process. They can be
divided into two big parts: *analysis* and *synthesis*.  Analysis (often
referred to as the *front end* of the compiler) works with a program in a
source language. Different analysis phases collect information about the source
program and build it's representation together with checking different
properties, such as lexical and grammatical structure, correct usage of data
types etc. Synthesis (often called the *back end* of the compiler) uses
information produced during the analysis to generate a program in a target
language, usually performing a lot of program transformations and optimisations
along the way \cite{DragonBook}. A typical compiler pipeline is pictured in
Figure \ref{compilerpipeline}. These phases do not necessarily happen one after
another in a sequence, some of them might be combined, for example, but it is a
convenient way of thinking about a compiler structure.

![Compiler pipeline\label{compilerpipeline}](CompilerPipeline.png)

One of the central parts of many compilers and, in particular, optimising
compilers is their *intermediate representation(s)* (IR) or *intermediate
language(s)* (IL). Intermediate representation is a data structure that
represents the program being compiled during the compilation process. Some
intermediate representations are also called languages, because they have a
symbolic representation. In this text these two terms will be used
interchangeably. One of the simplest examples of an IR is a syntax tree, built
by parsing a source language. Well-designed intermediate representation should
allow to abstract away unnecessary details while expressing the necessary ones
at certain compilation stages. It should be fairly easy to produce as well as
translate further. Many compilers use several different IRs, where they diverge
from the source language and resemble a compilation target's language or
architecture more and more as compilation progresses. Some IRs are quite
independent of both the source and the target, which allows them to be used in
retargetable compilers (compilers which can generate code for several different
target languages/architectures) by combining different front ends and back ends
which work with the same IR \cite{DragonBook}.

A major task performed by most modern industrial compilers is code
optimisation. Compiler writers try to make compiled programs perform as good as
possible regarding certain properties. The most common optimisations target
code execution speed (performance), code size and it's energy efficiency (which
becomes increasingly important given the advances in development of mobile
devices and their ubiquity). Code optimisation is also a very complex task,
since it needs to achieve good results, but even more importantly, not to
change program's observable behaviour. Combining these two aspects is extremely
difficult given a myriad of effects that modern programming languages allow to
perform. It includes, but is not limited to input/output, throwing exceptions,
modifying state, non-termination. Since IR is a compiler's main source of
information about the program being compiled (together with some other data
collected during the analysis) as well as the object the compiler transforms to
finally produce the target program, it can play a significant role in the
optimisation process.

This work tries to design an intermediate language based on monads and monad
transformers to allow powerful transformations and optimisations in the
presence of effects. The problem with many IRs is that a lot of valuable
information about programs is not captured in the IR itself, but instead a
compiler needs to track this information somehow.  One of the examples is the
effects a program may perform. The compiler has to infer information about
effects in order to decide whether it is safe to perform certain optimisations
or not and then record this in some separate data structures. Moreover, these
data structures need to be updated as transformations happen. Having an IL
based on monads we can encode effects right in the program and apply algebraic
properties and laws to perform code transformations.

Another area which this thesis tries to address is providing a flexible and
generic way of expressing parts of programming language semantics by combining
monads using monad transformers. In a usual setting to change the semantics of
a programming language, one needs to rewrite the code generation part of the
compiler for this language. As it will be described later, with monad
transformers it is possible to change certain parts of the semantics by just
changing the order in which monads are combined.

Finally, this work attempts to implement a programming library for working with
the introduced intermediate language and a set of effects-aware code
transformations and optimisations, which can be reused by compiler writers
targeting the designed IL. To evaluate and examplify the usage of the
intermediate language, two programming languages (object-oriented and
functional) are designed and compilers for them, which target the IL are
implemented.

If we consider the current state of compiler technology, we can see that there
are quite many different intermediate representations that are in use. Some of
the most notable are:

* Three-address code (TAC or 3AC). A simple representation, which is based on
  the idea, that there is at most one operator on the right-hand side of an
  instruction. This implies that all instructions are quite simple (for example,
  there are no built-up arithmetic expressions), which is beneficial for
  optimisation and target code generation \cite{DragonBook}.
* Representations based on static single assignment form (SSA) \cite{SSA}, the
  main idea of which is that every variable in a program is assigned exactly
  once. One of the main benefits of the SSA form is that many useful and powerful
  optimisations can be expressed in simple and efficient ways \cite{ComputingSSA}.
* A-normal form (ANF) introduced by Sabry and Felleisen \cite{ReasoningCPS} and
  further developed in \cite{EssenceCPS} is a representation that resembles CPS
  (continuation-passing style), which is a style of programming where functions
  take an extra argument called *continuation*. Continuation is a function, which
  has one parameter and is called with the return value of the function that is
  given the continuation as an argument. ANF requires all arguments to functions
  to be trivial (constants, lambda abstractions and variables) and that the
  result of any non-trivial expression is let-bound or is returned from a
  function. It has been shown that there is a correspondence between CPS-based
  intermediate representations (like ANF) and SSA form \cite{CPSSSA},
  \cite{SSAFP}.
* Intermediate representations based on monads. These will be described in more
  detail in the later chapters.

There are several specific examples of IRs used in industrial compilers that
are worth mentioning here, namely:

* Core (also referred to as System FC), STG and C-\- (Cmm) in GHC (The Glasgow
  Haskell Compiler). Core is a simple functional language (much smaller than
  Haskell) that is used for optimisations and further code generation
  \cite{GHCCore}. STG is an intermediate representation produced from Core and
  it's intention is to define how to efficiently implement Haskell on standard
  hardware \cite{STG}. C-- is a C-like portable assembly language \cite{Cmm} and
  is an example of the three-address code.
* Register transfer language (RTL) in GCC (GNU Compiler Collection) is a
  low-level intermediate representation with a syntax inspired by Lisp lists
  (<https://gcc.gnu.org/onlinedocs/gccint/RTL.html>). It is a language for an
  abstract machine with virtual (pseudo) registers and is another example of the
  three-address code. RTL is based on the idea described in \cite{PeepholeOpt}.
* LLVM Intermediate Representation. It is a language used in the LLVM compiler
  infrastructure (<http://llvm.org>), which aims to provide a language
  independent compiler toolchain with a number of optimisations. LLVM IR is a
  language for an abstract machine with an infinite set of registers, it makes
  use of SSA form, has a simple type system, abstracts away some low-level
  details such as calling convention \cite{LLVM}.

The IR designed as a part of this work is quite different from most of the IRs
described above. None of the mentioned IRs encodes different program effects
and they don't try to allow to express parts of the language semantics on the
high level either. Probably, the closest IR to the one we present is GHC Core,
which is also a high level functional language.

A lot of inspiration and intuition for this work comes from the Haskell
programming language \cite{HaskellReport}, thus some familiarity with Haskell
is assumed.

