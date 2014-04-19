% Notes on\
  "Monadic Intermediate Language for Modular and Generic Compilers"
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

Meeting 2014-02-03
==================

* LLVM vs C vs Native
    + C: the easiest to generate, hard to evaluate (far away from the machine code)
    + Native: the hardest to generate, the best for evaluation
    + LLVM: seems like a golden middle
* Typesetting (Pandoc)
* Polymorphism
    + monomorphic IL -> monomorphic source languages
    + System F -> complex code generator
    + Start with polymorphic and then we will see
* Data in IL
    + datatypes: products, sums, sequence, ADT?
* Writing textual description (document) of the language
* Nanopass style (small changes to different IRs)
    + <http://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf>
* Textual representation of IL
    + Parsing and pretty printing (look at nanopass?)
    + Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing (too much)
    + Checkpoints?
* Garbage collection
    + LLVM
        - <http://llvm.org/docs/GarbageCollection.html>
        - <http://www.reddit.com/r/programming/comments/1c5jz6/realistic_garbage_collection_on_top_of_llvm_epoch/>
        - <http://www.gamedev.net/blog/355/entry-2256288-real-world-garbage-collection-with-llvm/>
        - <http://lists.cs.uiuc.edu/pipermail/llvmdev/2011-February/038177.html>
        - <https://groups.google.com/forum/#!topic/llvm-dev/x35whlWokpE>

General thoughts. Week 1
========================

Source languages should have exceptions to show how this plays nicely with
Error monad (maybe even kinda transactions for one of them to show the
different ordering of State and Error)

Meeting 2014-02-10
==================

* let typing, no need in pure let -> monadic let/bind
* Effects representation and description
* My abstract meta questions about self description of MIL (one build-in type class of effects)
* More concrete view on things, maybe implement something from papers
* Type specialisation as a separate pass
* Polymorphic recursion

General thoughts. Week 2
========================

* Be careful with MIL type system
* Be minimal with source languages features
* Where are monad transformers in this picture
    + It starts to make sense (Bridging the gulf)

Meeting 2014-02-17
==================

Things to discuss:

* Papers reread
    + Harrison: when and how do they generate code. And what's up with continuations?
    + Tolmach exercise (quickly)
* MLj source code
* MIL notes (not all)
* Plans:
    + writing down some basic syntax and typing
    + switching to source languages design to refine MIL
    + it feels like I need to see how I will generate code:
        - source languages -> MIL
        - MIL -> LLVM

Notes:

* Data Types a la Carte for effects?
* Id - part of all stacks or not?
* Lattices - probably not, too complex.
* Type level lists, prefixes. Id ::: (State ::: (Exception ::: State))
* Transformers mainly for combining monads in MIL. Modular code generation is probably not our task.
  Nice retargeting neither.
* Code generation stages: SourceM -> IL1 -> IL2 -> IL3 -> LLVMM.
  MIL framework allows to express ILi nicely, and finally backend part produces LLVM.
* One AST type but less constructs further down (high level -> SSA/ANF).
* Effect systems. The same as monads.
* Computation types. Nice to have value types on syntactic level.
* References, multiple variables, efficiency. Probably, we need to have them.
* Type system: tuples, variants, iso-recursive types (maybe later, equi just NO).

Meeting 2014-02-24
==================

* System F vs F-omega, probably don't need kinds
* Variant types
* Source functional language with fixed set of monads
* OO and variants (super class field). One ADT for each class

Meeting 2014-03-03
==================

* Kinding (arrows for type operators, but only * for types and type variables)
* Typing rules
* OOLang: Maybe only for Ref. But what about method call with `?`
* References in MIL, no need in pure State

Meeting 2014-03-10
==================

* Typing rules fixes (T in scope, remove repeating, typevar-typename unification)
* when vs if
* Function syntax
* return statements
* purity

General thoughts. Week 6
========================

* The general question: is it source language compiler driver who orchestrates everything using MIL data types and functions?
  (and so there is no stage when we just give all the work to MIL backend)
* Does the source language compiler generates only one "MIL instance" and then
  MIL is on its own (ANF/SSA and so on)?
* MIL could have convinient representations already defined (ANF/SSA) so everybody could benefit.
* Who and when performs transformations on MIL?
* Is there only one CodeGen for the whole MIL?
* Code generation for unknown effects
* Phases on MIL:
    + Type checking: can leave it completely to MIL with additional supply of typing for "constants"
    + Transformations: a set of built-in transformations + new ones, derived by source language compiler
    + There could be built-in transformation from more high-level MIL to ANF representation and some optimisations expressed on ANF
    + Code generation: I am not sure we can do it on MIL side
* Recursion in MIL and FunLang
    + Restrict let rec in MIL to only functions?
    + Carefully consider effects and equalities

Meeting 2014-03-17
==================

* Maybe just fixed set of effects and a way to combine them (the generality is in combining)
* Different IRs (levels) differ in set of effects, not in constructs
* Code generation only from the fixed low-level representation (with fixed effects)
* Inconviniences of monad transformers are fine for us because we don't use them for programming
* Monadic annotations: lift, return. Can start with few and then add if needed

In general:
MIL library provides AST data types, set of effects and some built-in transformations.
Compiler writer generates as many of IRs as she wants and may run some built-in transformations and her own.
At some point she must generate the most low-level IR (like ANF/SSA) and from there, MIL library can perform some specific fixed
sequence of transformations and finally generate target code.

General thoughts. Week 7
========================

* Unit, Int and other built-in types representation and lexing. Special cases or type constructors?
* Strings in source languages and MIL
* Operators in MIL
* Annotating AST: approaches
* SrcSpan from Happy

Meeting 2014-03-24
==================

* It is possible to merge TyTypeCon and TyApp (and have list of types which are arguments). Then Type contains only types of kind * (fully applied).
  Otherwise, it is like currying (binary application vs list of arguments) on the type level.
* Maybe good to treat all types uniformly (like Unit, Int etc.).
* MIL must have at least characters.
* MIL should not have operators. It should have built-in functions instead. Application is always prefix (no infix).

General thoughts. Week 8
========================

* Source language AST:
    + Maybe don't need to have so many annotations in-place.
    + Some annotations don't make sense in some places. Have more stuff in the environment.
    + We want to have SrcSpan everywhere but where do we want to have types.
    + What and how goes to the environment.
    + Maybe, have almost two parallel representations - one for src program and another one - more internal (for type checking etc.).
      Feels ugly because of the duplication. Hard to figure out what do we need, where and when.
    + We can and should have global info in the environment:
        - Information about type constructors.
        - Information about data constructors.
        - Information about functions.
    + What we can't have in the environment:
        - Local names (variables). We know them at the type checking, maybe need to record their information in-place.
        - Types of subexpressions (probably don't need them in-place, we can figure them out locally if needed).
        - So we can abstract over names (like GHC) but have SrcSpan annotations almost everywhere.
        - Sometimes for internal stuff SrcSpans may get in the way. For example for Type in the environment, they don't really make sense.
          But for some occurences of types we would like to have SrcSpans. The same goes for constructors. Mostly for stuff in the environment, which
          is kind of internal.

Meeting 2014-03-31
==================

* Source tree annotations.
* Parsing errors.
* Nameless representation of terms: probably no.
* Tuples in MIL: probably no.

General thoughts. Week 10
=========================

* What about monad inference?
* SSA/ANF effects: IO and State? Lift?
* llvm-general
* Stack and heap
* Jump effect: continuation?

Meeting 2014-04-14
==================

* lift adds anything to the left of Monad (cons).
* type argument for monadic operations (get, put etc.) adds anything on the right of Monad (append).
* Monad inference: probably no. Just use monad laws with lifts.

General thoughts. Week 11
=========================

* OOLang design: purity, pure modifier versus Pure or Impure type.

