% Functional Programming Language Notes
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

* Pure strict (call-by-value) functional language
* Mostly inspired by F#/ML and Haskell
* Hindley-Milner type system (polymorphism and inference)
* Variant types (iso-recursive), pattern matching
* Special features: maybe try something like 'terminates' and/or 'total' modifier
* Think about exceptions:
    + here it is harder to handle different types with ability to add new than in OO because we don't have subtyping
    + extensible variants (expression problem)
        - <http://www.cs.ucla.edu/~todd/research/toplas04.pdf>
        - <http://www.haskell.org/haskellwiki/Extensible_datatypes>
    + type classes (too much)
    + maybe have just one handler for exception in general
* Side-effects: fixed set/hierarchy of monads

Effects:

* Monadic stuff (with built-ins).
* Lift: explicit recursion, mutual recursion.

Monads:
* Should do block be an expression or a special case of function equation body?
  If it is an expression, how do we know in which monad it is operating?
* Do we really need annotation on return? Can we pass the monad from the
  signature to the type checking routine? The same problem is with expressions,
  we need to keep the monad in which we are currently operating.
* Do we need an explicit or implicit lift?

Exceptions:
* Introduce throw and catch keywords (or functions). catch is infix.
* Exceptions don't carry any value.
* Exceptions don't work in a monad, so they can be combined with IO or State?
* We can think as: throw is of type a, catch is of type a -> a -> a.

