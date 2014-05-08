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

References and purity: We take a conservative approach and treat anything which is
reference related as impure. It could be less conservative by treating
reference reads and writes impure, but declarations and passing as parameters
as pure, but let's just treat references uniformly, it should be simpler.

What does it mean to reference a pure value: Ref (Pure Int)? Is it different
from Ref Int? It makes perfect sense for lazy language (if we reference a
computation), but if we reference a value computed by an impure computation or
just a literal. And then, are Mutable (Pure Int) and Mutable Int or Maybe (Pure
Int) and Maybe Int different? Again, since we are in a strict language, when we
have something of atomic type, like main : Unit, which is impure and pass it as
an argument, we pass a value, not a computation, and this value, which main
returned (unit) is pure (so, the reasoning above about enormous side-effecting
computation is incorrect for strict language). Maybe we should allow Pure only
on the top level and of type arrows, for example, or, on the far right of type
arrow (the latter seems more correct, since we don't deliver a function
impurely, but rather have an impure function which delivers a value of some
type)? But with this approach we don't know if f : Int is pure or impure. So it
must be f : Pure Int if it is pure. But for the parameters it doesn't really
make sense being Pure Unit. So, only return type of the function and far right
of arrow?

What about function returning Mutable value or taking Mutable value as an
argument, this doesn't make too much sense.  Reference - yes, but mutable
variable which is treated by value - no.

Complicated type nesting. Parse generally and then let the type checker do the job.
Restrictions on well-formed types:
* variable declarations: nothing specific
* function parameters: Mutable and Pure are not allowed
* function return type: Mutable is not allowed
* Ref and Mutable on the top level (nested only via function arrows)

self reference inside classes. What type should it have? It seems that just
C, so it is not Mutable and not a reference (which could be assigned to).
Should we allow shadowing with class members? Should we allow referencing
class members inside only with self? We can allow shadowing (class members
can have the same names as global things), but allow only self referencing
of members inside.

While we don't have visibility modifiers, should fields be private
(protected) and methods public?

Don't allow mutually recursive field declarations (in init expressions).

Constructors question. For now, only one parameterless constructor or just one
constructor of any type?

References impurity together with field initialisation purity means that we can
have only Ref Maybe class fields.

OOLang has either Pure of impure, so probably impure should be the type of all
effects combined, and Pure - just a corresponding type. What the type of Error
and State should be? Are they polymorphic? Should Error be some built-in
Exception type? State is a mapping from variable names to any type?  Do we
really need these types at all? Are they useful for transformations and/or code
generation?

