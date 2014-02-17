% Functional Programming Language Notes
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

* Pure strict (call-by-value) functional language
* Mostly inspired by F#/ML and Haskell
* Hindley-Milner type system (polymorphism and inference)
* Variant types, records, pattern matching
* Special features: maybe try something like 'terminates' modifier
* Think about exceptions:
    + here it is harder to handle different types with ability to add new than in OO because we don't have subtyping
    + extensible variants (expression problem)
        - <http://www.cs.ucla.edu/~todd/research/toplas04.pdf>
        - <http://www.haskell.org/haskellwiki/Extensible_datatypes>
    + type classes (too much)
    + maybe have just one handler for exception in general

