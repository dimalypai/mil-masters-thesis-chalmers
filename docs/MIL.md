% Monadic Intermediate Language
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)
%

let:

* Two types of lets: monadic and pure in Bridging the Gulf
* One monadic let in Tolmach
* let as syntactic sugar on top of try-catch in Benton
* let rec

Type system:

* System F as a base
* monadic stuff
* constants (constructors)?
    + think about this vs variant types
* recursive types
* data types:
    + products < tuples < records
        - records may be very beneficial for OO (as in Pierce)
        - records may be ordered in IL but unordered in source languages (because subtyping is too much)
        - problems with records typing and type safety (types introduced on the fly or separately)
    + sums < variants
        - case with pattern matching
        - way more to consider
        - can define Bool (with if as case) and lists (having recursive types)
    + lists?
* base types:
    + Unit (for side-effecting computations)
    + Bool
    + Int
    + Float?
    + Char?
    + String?
* references? I doubt, but Gulf and Benton have them, although Tolmach doesn't

Explicit typing: It can and should be type checked but there is no need in type reconstruction. Type annotations in places
when we introduce variables (lambdas, lets)

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

How to express conditionals? (Tolmach: explicit if, Benton: case with pairs, GRIN: if and case)

Combining effects:

* One needs to describe a monad and its transformer somehow to plug it into the language
* What about typing rules? Provides type checking routine for new monad and its constants (TypeCheck type class in Haskell)?

Use de Bruijn representation in MIL implementation

