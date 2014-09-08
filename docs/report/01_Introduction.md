# Introduction

Modern compilers are one of the most complex and sophisticated software
products in the world. But the vast majority of them follow more or less the
same structure known for many years, which is called *compiler pipeline*.

* Compiler pipeline
Terms representation and language will be used interchangeably.
    + Why IR is important? (\cite{DragonBook})
    + Different IRs (RTL in GCC, SSA/ANF, Core, STG \cite{STG})
* Problem statement
    + Managing effects and optimisations
        - Encoded right in the program
        - Algebraic properties
    + Combining effects
        - Flexible language semantics
        - Programming framework

A lot of inspiration and intuition for this work comes from the Haskell
programming language \cite{HaskellReport}, thus some familiarity with Haskell
is assumed.

