% Monadic Intermediate Language Notes
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

let:

* Two types of lets: monadic and pure in Bridging the Gulf
* One monadic let in Tolmach
* let as syntactic sugar on top of try-catch in Benton
* MIL: monadic let/bind and let rec

Type system:

* System F (omega?) as a base
* monadic stuff
* constants (constructors)?
    + think about this vs variant types
* recursive types
    + iso-recursive with fold/unfold hidden in data constructors of variant types
* data types:
    + products < tuples < records
        - records may be very beneficial for OO (as in Pierce)
        - records may be ordered in IL but unordered in source languages (because subtyping is too much)
        - problems with records typing and type safety (types introduced on the fly or separately)
        - probably have only tuples in MIL but records in source languages, golden middle
    + sums < variants
        - case with pattern matching
        - way more to consider
        - can define Bool (with if as case) and lists (having recursive types)
    + lists
* base types:
    + Unit (for side-effecting computations)
    + Bool
    + Int
    + Float?
    + Char?
    + not really essential: String. May be expressed as [Char].
* references
    + I doubt because we have State monad for this
    + but Gulf and Benton have them, although Tolmach and GRIN don't
    + probably, can be beneficial for OO and other impure languages
    + challenge with System F
    + how typing rules should look like (containing State effect)
* connection between, for example, generics in the source OO language and System F?
* think about value and computation types distinction (as in Bridging the gulf
  and Benton). They don't have Id, that can express value types as
  computations.

Explicit typing: It can and should be type checked but there is no need in type
reconstruction. Type annotations in places when we introduce variables
(lambdas, lets)

Question: why do we need System F?
Think about source language features (polymorphism).

Polymorphism options:

* We could do type erasure for source language, but we want to have multiple
source languages so for example we can do it only once on the IR level. Does this work
for LLVM which generates native code or only for things like JVM?
* We can generate different code for every type instantiation (type specialisation, like C++).
Again, more easy to do that only once for IR.
* Reified generics (C#)
* <http://llvm.1065342.n5.nabble.com/Parametric-polymorphism-td31733.html>

Combining effects:

* One needs to describe a monad and its transformer somehow to plug it into the language
    + Where are the monad transformers in this picture?
    + Day and Hutton don't use monad transformers for compilation at all (because they don't compile to MIL)
    + Bridging the gulf cleared things. They have a predefined StateLift transformer. We need general stuff there
    + Effects are specified by transformer stack in MIL? But on the other hand
      we could have a set of them and all combinations in different cases. What
      about lattice?
* What about typing rules? Provides type checking routine for new monad and its constants (TypeCheck type class in Haskell)?
* Also what about rewrite rules or transformations?
* MIL framework should have its own predefined set of effects/monads (with transformations) and be able to take new ones from the user.
* Describing monads and stuff in the MIL itself - no!

Code generation:

* Transformer and monadic operations describe code generation?
    + What about several backends -> through type class (like different MonadState)
* Or do we generate code in one place? Less modularity, but monads and their combinations are still general?
* Do we use monad transformers for code generation or only as a way to combine monads in MIL?

Runtime system:

* Should the calls to the runtime system (like memory allocations) be done at the level of MIL?
    + Every source language will bother about generating calls to the runtime (more difficult but more flexible)
* Better?: Or code generation form MIL will generate calls to the runtime system later (MIL with automatic memory management)?
    + Calls to the runtime system are generated only from MIL (less flexible)
* Heap abstraction through State monad? Allocations by put?

MIL as EDSL in Haskell. May be used for MIL code generation.

Use de Bruijn representation in MIL implementation

How to express conditionals? (Tolmach: explicit if, Benton: case with pairs, GRIN: if and case)

In general: if the MIL is reach enough it is easier to add source languages, we already have low-level IR (LLVM)

One of the things it needs to represent is A-normal form:

* Do we use State at first and then translate to just lets and function calls?

What should the type of main in MIL be? Specific stack?

Add type aliases to the language (as type in Haskell):
alias T = Int -> Int

