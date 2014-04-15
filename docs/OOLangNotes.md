% Object-Oriented Programming Language Notes
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

* Python, C#, F# and Haskell inspired syntax: almost no curly braces
* Special modifiers: pure
* Functional features: first-class functions, currying, tuple data type etc.
* Expressions-based approach
* Explicit approach to mutability:
    + name binding with = for immutable variables
    + Mutable types with <- for assignment and ! for getting current value when using with immutable type
    + references with new, ! and :=
* Subtyping but with no downcasts?
* Think about exceptions (using subtyping)
* We can easily have anonymous datatypes created on the fly if we have records in MIL (or maybe just compile them to tuples)
    + record subtyping?
* Explicit nullability (Maybe/Option) for both value and reference types (in C# lingo)

Effects:

* Lift: loops, explicit recursion, mutual recursion.
* State: Assignments (<- and :=).
* IO: built-in functions.
* Error: division, throw.

