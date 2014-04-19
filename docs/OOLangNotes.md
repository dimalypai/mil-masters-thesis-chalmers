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

Purity: first we had optional pure modifier, but it turns out that there are
some problems with this. When you have a value (variable) of some type, for
example, Unit, it may be just unit literal or an enormous side-effecting
computation. When passing stuff through variables (parameters) we loose purity
information (and we have higher-order functions, so this is easy). The idea now
is to try to handle this with Impure type constructor, then anything can be
annotated with it and purity is then assumed by default. Or maybe have Pure
type construtor instead. This approach will be easier and more uniform in
general and should be easier even for implementation (less ad-hoc treatment).

Pure A is a subtype of A? + impure by default. Should be less noise and more
traditional?
If everything is pure by default, we can't pass impure stuff instead of pure
and there is no way to do pure from impure.

