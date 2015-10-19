# Introduction

{>> Rather than spending a lot of text on what a compiler looks like, it is important that you very early say what your project is about. The reader shouldn't have to read several pages to get the overarching idea. Once you've prested the idea, it's a good idea to explain things in more detail as you do. But I'd like you to add some text early on which explains your project concisely. <<}

Modern compilers are one of the most complex and sophisticated software
products in the world. {>> This is a very bold statement. And bold is good, but I wonder if you can substantiate your claim somehow. With a reference for instance <<} But the vast majority of them follows more or less the
same structure known for many years, which is called *compiler pipeline*.

Compiler pipeline is a chain of phases, which together form the compilation
process. They can be divided into two big parts: *analysis* and *synthesis*.
Analysis (often referred to as the *front end* of the compiler) works with a
program in a source language. Different analysis phases collect information
about the source program and build it's representation together with checking
different properties, such as lexical and grammatical structure, correct usage
of data types etc. Synthesis (often called the *back end* of the compiler) uses
information produced during the analysis to generate a program in a target
language, usually performing a lot of program transformations and optimisations
along the way \cite{DragonBook}.
A typical compiler pipeline is pictured in Figure ... These phases {++ do ++} not
necessarily happen one after another in a sequence, some of them might be
combined, for example, but it is a convenient way of thinking about a compiler
structure.

TODO: Figure 1. Compiler pipeline
   Source program
-> (character stream) -> Lexing
-> (token stream) -> Parsing
-> (syntax tree) -> Type checking
-> (syntax tree) -> Intermediate code generation
-> (intermediate representation) -> Optimisation
-> (intermediate representation) -> Target code generation
-> Target program

One of the central parts of {== almost any compiler ==}{>> in particular optimizing compilers. There are some compilers which doesn't have intermediate languages <<} is {~~ it's ~> its ~~} *intermediate
representation(s)* (IR) or *intermediate language(s)* (IL). Intermediate
representation is a data structure that represents {== a ==}{>> not just 'a' program, the program that is being compiled! <<} program during the
compilation process. Some intermediate representations are also called
languages, because they have a symbolic representation. In this text these two
terms will be used interchangeably. One of the simplest examples of an IR is a
syntax tree, built by parsing a source language. Well-designed intermediate
representation should allow to abstract away unnecessary details while
expressing the necessary ones at certain compilation stages{~~ , it ~> . It ~~} should be
fairly easy to produce as well as translate further. Many compilers use several
different IRs, where they diverge from the source language and resemble a
compilation target's language or architecture more and more as compilation
progresses. Some IRs are quite independent of both the source and the target,
which allows them to be used in retargetable compilers (compilers which can
generate code for several different target languages/architectures) by
combining different front ends and back ends which work with the same IR
\cite{DragonBook}.

There are many different intermediate representations that are used in modern
compilers. Some of the most notable are:

* Three-address code (TAC or 3AC). A simple representation, which is based on
  the idea, that there is at most one operator on the right-hand side of an
  instruction. This implies that all instructions are quite simple (for example,
  there are no built-up arithmetic expressions), which is beneficial for
  optimisation and target code generation \cite{DragonBook}.
* Representations based on static single assignment form (SSA) \cite{SSA}, the
  main idea of which is that every variable in a program is assigned exactly
  once. There are many useful and powerful optimisations that can be expressed
  using SSA form \cite{ComputingSSA}. {>> All optimizations that can be expressed on SSA form can also be expressed on, say, three address code. However, some analyses become simpler/more accurate when using SSA. <<}
* A-normal form (ANF) introduced by Sabry and Felleisen \cite{ReasoningCPS} and
  further developed in \cite{EssenceCPS} is a representation that resembles CPS
  (continuation-passing style) {>> you haven't explained what CPS is <<} and requires all arguments to functions to be
  trivial (constants, lambda abstractions and variables) and that the result of
  any non-trivial expression is let-bound or is returned from a function. It has
  been shown that there is a correspondence between CPS-based intermediate
  representations (like ANF) and SSA form \cite{CPSSSA}, \cite{SSAFP}.
* Intermediate representations based on monads. These will be described in more
  detail in the later chapters.
* Core (also referred to as System FC), STG and C-- (Cmm) in GHC (The Glasgow
  Haskell Compiler). Core is a simple functional language (much smaller than
  Haskell) that is used for optimisations and further code generation
  \cite{GHCCore} TODO: fix cite. STG is an intermediate representation produced
  from Core and it's intention is to define how to efficiently implement
  Haskell on standard hardware \cite{STG}. C-- is a C-like portable assembly
  language \cite{Cmm}.
* Register transfer language (RTL) in GCC (GNU Compiler Collection) is a
  low-level intermediate representation with a syntax inspired by Lisp lists
  (<https://gcc.gnu.org/onlinedocs/gccint/RTL.html>). It is a language for an
  abstract machine with virtual (pseudo) registers. RTL is based on the idea
  described in \cite{PeepholeOpt}.
* LLVM Intermediate Representation. It is a language used in the LLVM compiler
  infrastructure (<http://llvm.org>), which aims to provide a language
  independent compiler toolchain with a number of optimisations. LLVM IR is a
  language for an abstract machine with an infinite set of registers, it makes
  use of SSA form, has a simple type system, abstracts away some low-level
  details such as calling convention \cite{LLVM}.

{>> It's not clear whether the above list is meant to be an overview of categories for IRs, like TAC or SSA, or specific IRs, like LLVM or Core. My suggestion is that you list categories of IRs and under those categories you can list specific, famous examples, like LLVM and Core. <<}

A major part of most modern industrial compilers is code optimisation. Compiler
writers try to make compiled programs perform as good as possible regarding
certain properties. The most common optimisations target code execution speed
(performance), code size and it's energy efficiency (which becomes increasingly
important given the advances in development of mobile devices and their
ubiquity). Code optimisation is also a very complex task, since it needs to
achieve good results, but even more importantly, not to change program's
observable behaviour. Combining these two aspects is extremely difficult given
a myriad of effects that modern programming languages allow to perform. It
includes, but is not limited to input/output, throwing exceptions, modifying
state, non-termination. Intermediate representation can play a significant role
here. {>> How? <<} This work tries to use an intermediate language based on monads and monad
transformers to allow powerful transformations and optimisations in the
presence of effects. This is done by using these structures to encode effects
right in the program and applying algebraic properties and laws to perform code
transformations.

Another area which this thesis tries to address is providing a flexible and
generic way of expressing parts of programming language semantics by combining
monads using monad transformers.

Finally, this work attempts to implement a programming library for working with
the introduced intermediate language and a set of implemented code
transformations and optimisations. To examplify the usage of the intermediate
language, two programming languages (object-oriented and functional) that are
compiled to this IL are designed.

A lot of inspiration and intuition for this work comes from the Haskell
programming language \cite{HaskellReport}, thus some familiarity with Haskell
is assumed.

{>> I was hoping to see some explanation of the problem you're trying to solve. Why is it better to use a monadic intermediate language? What is the problem with other forms of IRs? It'd be good if you can say a few words about that. <<}