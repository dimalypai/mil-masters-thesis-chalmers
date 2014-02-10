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

