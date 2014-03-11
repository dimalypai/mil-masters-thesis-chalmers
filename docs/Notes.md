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

