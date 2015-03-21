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

* OOLang design:
  + pure modifier versus Pure or Impure type
  + references and purity
  + types nesting (parsing, type checking)

Meeting 2014-04-22
==================

* Daan Leijen, Microsoft Research: Koka language
* Effect: functions in references:
  f <- newRef (\() -> ())
  f := \() -> !f()
  (!f) ()
* Non-termination with type recursion:
  data A = A (A -> ())
  foo (A f) = f (A f)
* OO assignments: references for both? stack vs heap question?
    + Ref is compiled to references, ordinary types and Mutable are compiled to
      just value, they are source language type system feature.
* Mutable for non-primitive types: having no restrictions (like storing
  functions) leads to difficulties (like closures are heap allocated)
* Running monadic computations in FunLang: built-in functions, ideally - a way
  to compose monads
* FunLang: have a monad stack like in Tolmach rather than disjoint built-in
  monads as a way to "combine" monads
* SSA effects: IO, State, Lift
* Exceptions implementation in LLVM

Meeting 2014-04-28
==================

* Substitution: if you reduce under lambda and have only closed terms, it is
  possible to have name capture.
* Running and combining monads: maybe we need something for initial values of
  state for IO, which subsumes State.
* FunLang transaction-like (state rollback) vs OOLang (state preservation)
  ordering of State and Exception
* FunLang: try-catch

General thoughts. Weeks 13-14
=============================

* OO-polymorphism in MIL. Passing subclasses problem, self parameter. Maybe
  introduce record/tuples with subtyping instead of going through ADTs.
* FunLang monad hierarchy: IO subsumes State, but what is the type of it?
  Maybe, have built-in StateT?
* OOLang super-effect type (impure) and FunLang more fine-grained effects
  combinations.
* MIL effects: Read, Write, Alloc vs State? What about ordering of these more
  fine-grained effects and Error.
* FunLang: what about compiling State to references in MIL?

Meeting 2014-04-12
==================

* OO-polymorphism. More subtyping, depth subtyping, function subtyping?
* FunLang monad combining. It is a limitation, but we should postpone it for
  now.
* State vs ST in Haskell. runST doesn't let to return a reference by forall s.
* OOLang: Mutables are compiled with producing ANF/SSA.
* Fine-grained effects are nice and powerful, but maybe later. They require
  some thought.

General thoughts. Week 15
=========================

* MIL, OOLang and subtyping: can the different overriding variance in OOLang
  influence how the subtyping relation should look like in MIL? It seems that
  yes. We need the self argument to be covariant, which is incompatible with
  contravariant argument types. Is having covariance only for the first
  argument of the method, which is self (probably) too ugly and crazy? Yes.
  There is an idea to hide the self parameter with built-in ReaderM monad. Not
  sure how good is this, but we can avoid having covariant function types (and
  make this adjustable for example). ReaderM type will be covariant in the
  environment type and (probably) adjustable in the return type.
  For class methods: environment is the self type, the return type is the
  source method type.
  We need to keep super class methods in order to be able to call them via
  `super`, this on one hand eliminates the need for ReaderM trick, since it is
  only width subtyping and we don't subtype function types with different self
  parameters. But on the other hand, it is not yet clear how to do a method
  dispatch with this setup. Do we need some type cast operations? Maybe,
  introduce a third element of the tuple for super class methods (but with this
  we need ReaderM trick back again)?
* OOLang monad stack: it turned out to be useful for the purposes of lifting
  (when we have, for example, a pure function call inside an impure function)
  to include Id as a base monad for the stack. But is it OK to have IO not at
  the bottom? We don't run any transformers to get away from them, they are
  mostly for annotation purposes, so should it be fine?
* FunLang State compilation: compiling State to references might be good from
  the performance point of view, but there are some problems. Namely, it arises
  from the fact that we have explicit type application for polymorphic
  functions, like get, put etc and the storage itself is implicit here.
  Consider: g : (forall S . State S) <- return get;
            i : Int <- g [Int];
  This corresponds to read_ref (!) operation, which has type forall A . Ref A
  -> State A. So, the storage is explicit. There is no way, we could "prestore"
  the reference before specifying the type argument. Maybe, with type
  inference, it would be possible, by compiling get to read_ref ref.
* Non-termination (Lift effect): need to perform dependency analysis as in SPJ
  6.2.8 to find recursive and groups of mutually recursive definitions + loops
  in OOLang.

Meeting 2014-05-19
==================

* FunLang State: it should be possible to compile State to references, but
  maybe go simpler way and have a "pure State" monad in MIL as well, which then
  would be transformed to pure functions, for example.
* OOLang class representation: reread TAPL.

General thoughts. Week 16
=========================

* OOLang: Give up on Pure types as purity annotations and introduce separate
  purity annotation in the syntax tree (Bool field).
* Should Lift be renamed to NonTerm to avoid confusion with lift operation?
* OOLang: I completely forgot about non-termination for Pure functions (Haskell
  notion of purity). So, instead if Id we should have Pure_M (as an alias),
  which should be an alias for Lift. Then, Impure_M becomes: Error ::: State
  ::: Lift ::: IO. So, now, Pure_M is not at the bottom of Impure_M. To get
  Error ::: State to the left, we can use lift. But what about IO on the right?
  It seems that computation of type Lift is also a computation of type Lift :::
  IO, since it is at the top and it has more effects. It is like, we don't need
  to lift the computation in the monad on top of the stack (in Haskell).
  Having IO ::: Lift for Impure_M seems wrong, since they don't commute (do
  they?). When Lift (think MaybeT) is at the top we have an IO computation
  which does IO and delivers a value or does not deliver a value: IO (Maybe a).
  When we have IO ::: Lift, we have Maybe (IO a). So it is either does IO or it
  does not.
* The same problem comes for Error, but there it feels more involved, because
  there is State in between.
* Should Pure_M contain Error as well, so that everything can throw an
  exception, basically? Then, some primitive operations will have Error in the
  type, like division, for example.

Meeting 2014-05-26 (cancelled)
==============================

* Classes representation: current state and recursive types.
* Effects for Pure computations and their lifting.
* Code generation at the moment.
* Report structure.
* State: Pure and Ref.

General thoughts. Week 17
=========================

* OOLang: New monad stacks for Pure_M and Impure_M.
* Problems with MIL type checking: monad cons prefix. Seems that it requires
  polymorphism and/or type inference. Or maybe we could choose a highest
  effect.
* OOLang and FunLang notion of purity is probably Error and NonTerm and not Id.
  Would be good to do effect inference (for NonTerm for example), but for now
  it may be more conservative.
* Aliases turn out to be very nice for reading but tricky for implementation.
* MIL type annotations for expressions? More likely no than yes.

General thoughts. Week 18
=========================

* Types of built-in read* functions in MIL should contain Error: Error Unit :::
  IO. Than it is quite problematic to use them inside Impure_M monad, we need a
  way to insert effects between the Error and IO. Also, MIL built-in functions
  may use a different type of error from the source language Error effect.
  Maybe we could add one more Error with a built-in error type at the bottom
  right on top of the IO? This almost works, but there is a problem that this
  effect ordering is not very good for the language semantics. Ideally, there
  probably should be something like Error MilError ::: m ::: IO for any m, but
  this complicates things.
* Effects ordering and read function problems.
* Thoughts on why "monad prefixing works":
  We can think that ::: always has an implicit m at the end, and so it is sort
  of polymorphic in the suffix, namely, Error Unit ::: IO is actually Error
  Unit ::: IO ::: m, where m can be any monadic thing (single monad or cons).
  Also, we can imagine that there is always an implicit sequence of returns
  that makes the resulting types isomorphic (to complete the monads from the
  suffix).

Meeting 2014-06-05
==================

* Fix types of built-in functions for each source language (by providing their
  types).
* Read about monad morphism.

Meeting 2014-06-23
==================

* Performance evaluation. Write interpreter for MIL and count reductions?

General thoughts
================

* There is a problem with OOLang code generation, involving built-in functions.
  When we fully apply it at the end, we need to lift the result. First problem
  is that there seems to be not enough information in terms of which monad to
  lift from. And the second is that, say we have a function with State type
  inside Impure_M, which is Error ::: NonTerm ::: State ::: IO. Can we just
  lift [State -> Impure_M]? We should be able to, it is a matter of number of
  lifts implicitly hidden in our lift operation.
* The first problem seems to be solved by looking at the type of the left
  operand in the application.
* Now it seems that there should be more things happening in the type
  conversion for OOLang, more similar to the FunLang, namely Pure_M and each
  function arrow level and not only at the end if there is Pure in the source
  program.
* What about type checking MIL function definitions which are (mutually)
  recursive and a NonTerm effect? Need to check that they contain NonTerm
  somewhere in their type.

Meeting 2014-06-30
==================

* Built-in functions in MIL. Maybe go with just read_char or something like
  that and implement "primitive" functions in MIL.
* Future work: checked arithmetic operations.
* Integer type in MIL: bounded vs unbounded.
* let rec: NonTerm at the bottom.
* NonTerm as Maybe? Probably not really. It is not Nothing, is Just forever.
* Haskell intuition: be clear in the report.
* Monad transformers: we always need a monad at the bottom to get going. Lift
  puts more restrictions, since it requires not a monad but a monad
  transformer. But implicit prefix rule is about polymorphism in the monad.

General thoughts
================

* Strings. We now will have only characters in MIL and other built-in things
  will be constructed using character primitives. What to do with String type
  (literals)? We can have a list-like data type, but the problem is that, for
  example, printString will be recursive in MIL and thus potentially
  non-terminating. This will spoil its type.
* MIL type checking. We usually use alpha equivalence for checking types
  compatibility.  We probably need to do more, like checking monads
  compatibility as well (prefix). Do we need to do it everywhere?
* MIL parsing effort: it will be very nice for working on MIL in terms of
  testing and experimenting. Problems: as in FunLang parsing: type variables
  and type constructors plus monads. Have a separate source type
  representation? Probably, TypeChecker needs to do additional work, tracking
  type variables and converting special type constructors to built-in monads.

General thoughts. January
=========================

* The previous reasoning on the subject of "why monad prefix thing works"
  seems to be not quite correct. See MIL type checking test MoreEffects:
main : State Int =
  return [State ::: IO] 1;
In this case State Int should definitely not mean (State ::: M) Int,
because then we could hide IO, which is obviously wrong.
But:
implicitMonadQuantification : (State ::: IO) Int =
  return [State] 1;
should work. How should this behave in bind? Right now, first binder
restricts everything else with its monad:
let (x : Int) <- return [IO] 1 in
  return [IO ::: State] 2;
This will not pass the type checker and we cannot do anything to the first
return to make it IO ::: State. Should we choose the highest effect?

General thoughts. February
==========================

* During the work on OOLang CodeGen a couple of issues have been revealed. They
  have the same underlying source. One of the issues is type transformation for
  something like {f : Int -> Pure Int} -> Int -> Pure Int. And another one is
  purity checking for a function of such a type. Is it Pure or not? The whole
  thing after parameter binders is a return type, so from this point of view it
  is impure (it impurely delivers a pure function of type Int -> Pure Int). The
  current checking looks only at the last Pure and decides that it is pure.
* Purity checking approach in OOLang is wrong at the moment. The problem is
  related to the one above. Before the thought was that only partial
  application can produce side effects, but not really. When impure function f
  : Int -> Int -> Int is applied to only one Int it can already produce side
  effects. Because it could return a function (Int -> Int) impurely. Arity
  needs to be considered more. But then pure function with many arguments
  should have Pure on every level because of currying, like Int -> Pure (Int ->
  Pure Int). This is a bit unfortunate.

Monad stacks and types:
-----------------------

Error e m a => m (Either e a)
State s m a => s -> m (a, s)
NonTerm m a => m (Maybe a)
IOT m a => m (IO a)

   (NonTerm ::: Error e) a
=> Error e Id (Maybe a)
=> Either e (Maybe a)

   (Error e ::: NonTerm) a
=> NonTerm Id (Either e a)
=> Maybe (Either e a)
OOLang/FunLang Pure looks like this (as Tolmach's EXN)

   (Error e ::: NonTerm ::: State s ::: IO) a
=> (NonTerm ::: State s ::: IO) (Either e a)
=> (State s ::: IO) (Maybe (Either e a))
=> s -> IO (Maybe (Either e a), s)
Currently OOLang's Impure (Program should always return state even when it doesn't terminate, this sounds impossible)

   (Error e ::: State s ::: NonTerm ::: IO) a
=> (State s ::: NonTerm ::: IO) (Either e a)
=> s -> (NonTerm ::: IO) (Either e a, s)
=> s -> IO (Maybe (Either e a, s))
OOLang's Impure should look like this (as Tolmach's ST).
But it is hard to combine this stack with Pure stack, there is State in between Error and NonTerm.

   (State s ::: Error e ::: NonTerm ::: IO) a
=> s -> (Error e ::: NonTerm ::: IO) (a, s)
=> s -> (NonTerm ::: IO) (Either e (a, s))
=> s -> IO (Maybe (Either e (a, s)))
This is better in a sense that Error and NonTerm come together and we can lift
to State on top, but in this case state is rolled back in case of exception.
This is of course possible semantics, but would be nice to have Error ::: State
for OOLang. It is a pity that IL kind of pushes and  determines the semantics
of the source language.

One radical thing would be to exclude NonTerm altogether from MIL, it is kind of hard and causes trouble.
But is it as useful then, if we cannot express non-terminating things.

Then:
Error e a => Either e a
This is Pure for OOLang/FunLang.

   (Error e ::: State s ::: IO) a
=> (State s ::: IO) (Either e a)
=> s -> IO (Either e a, s)
This is Impure for OOLang.

   (Error e ::: IO) a
=> IO (Either e a)
This is IO for FunLang.

   (State s ::: Error e) a
=> s -> Error e Id (a, s)
=> s -> Either e (a, s)
Could be State for FunLang (for the fun of different Error/State ordering).
Pure things will need to be lifted to State.

There is still a limitation that FunLang doesn't allow combining monads (IO and State).

General thoughts. March
=======================

* MIL. What happens if we have something of monadic type in the argument position?
  When does the effects happen? It looks like they should happen only when
  doing bind, because otherwise there should be some type trickery going on as
  in OOLang (roughly), because if we evaluate them before passing to a function
  then arguments never can have monadic types, because this is a lie, effects
  have happened already.
