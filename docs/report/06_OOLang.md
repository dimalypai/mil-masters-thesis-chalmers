# Object-oriented programming language (OOLang)

> *In this chapter we will look at another source programming language --
> OOLang. Language overview, design principles and example programs are
> presented. Then we describe interesting parts of the MIL code generation. At
> the end, conclusions regarding implementation of the OOLang compiler using
> MIL are drawn.*

## Overview

OOLang is an object-oriented programming language. The design of OOLang is
mainly inspired by three programming languages: C#, Haskell and Ruby. Most of
the semantics is rather similar to the one of C#. Syntax is inspired by both
Haskell and Ruby. Some crucial language design ideas and decisions as well as
some general influence comes from Haskell.

OOLang offers a separation between pure and impure functions on the type level,
encourages immutability and tries to eliminate one of the most common problem
of the modern OO-languages: null references.

## OOLang by example

This section will introduce most of the OOLang features using example programs.

### Functions, statements and expressions

OOLang allows to define functions consisting of several statements. Functions
can be pure or impure, which is captured in the return type. By default all
functions are impure. Impurity in OOLang comes from input/output and
assignments to references. Function definitions can contain parameters in curly
braces with a function arrow (`->`) separating several parameters (and a return
type), which means that functions are curried. Note, that OOLang does not have
built-in support for tuples, therefore it is not possible to define a function,
which would take several arguments at once (as a tuple).  Function application
is expressed using juxtaposition. Unlike many other programming languages with
statements, there is no `return` statement in OOLang.  Instead, every statement
has a value (and a type) and so the value of the last statement is what is
returned from a function.

OOLang has `Unit` (the type of the `unit` literal), `Bool` (which has literals
`true` and `false` as possible values), `Int`, `Float`, `Char` and `String` as
built-in types. It supports the usual infix arithmetic operations: `+`, `-`,
`*` and `/`.

The following example demonstrates some of the features described above:

~~~
def main : Unit
  fun 1;
  unit;
end

def fun : {a : Int} -> {b : Float} -> Pure Int
  a;
end
~~~

OOLang supports higher-order functions:

~~~
def main : Unit
  applyInt idInt 1;
  unit;
end

def applyInt : {f : Int -> Pure Int} -> {x : Int} -> Pure Int
  f x;
end

def idInt : {x : Int} -> Pure Int
  x;
end
~~~

At the moment, there are no lambda expressions in OOLang, but given that the
target for the language is MIL, it should be rather straight-forward to add
them.

OOLang provides a conditional `when-otherwise` statement, which corresponds to
`if-else` in most other languages. Both branches have to have the same type:

~~~
when b do
  1;
otherwise
  2;
end;
~~~

Exception handling is performed using `try-catch-finally`.  Exceptions can be
thrown with the `throw` statement, which is annotated with a type, since as it
was mentioned earlier, every statement in OOLang must have a type. The types of
`try` and `catch` block must be the same (or rather satisfy the subtyping
relation, which we will present later in this chapter). The type of `finally`
does not matter, since its value is not returned.

~~~
try
  throw [Int];
  1;
catch
  2;
finally
end;
~~~

OOLang provides a number of built-in functions for reading and printing values
of built-in data types:

~~~
printString : String -> Unit
readString : String
printBool : Bool -> Unit
readBool : Bool
printInt : Int -> Unit
readInt : Int
printFloat : Float -> Unit
readFloat : Float
~~~

### Mutability

By default all variables in OOLang are immutable. To declare a mutable variable
a special type constructor `Mutable` is used. The next example demonstrates a
declaration of an immutable and a `Mutable` variable and the `Mutable`
assignment operator:

~~~
def assignments : Pure Unit
  x : Int = 0;
  y : Mutable Int <- 0;
  y <- 1;
end
~~~

When variables are declared in OOLang, they must be initialised. There is an
exception to this rule involving the `Maybe` type, which will be described
later in this section.

`Mutable` variables never escape the scope of the function they are declared
in, therefore assignments to `Mutable` variables are considered pure, since
they cannot change any global state outside of the function. Given this,
`Mutable` variables are still quite useful, especially if OOLang is extended
with loop constructs. It is worth highlighting that OOLang `Mutable` variables
are somewhat similar to the `mutable` variables in F#.

### References

OOLang also supports references, which are quite similar to ordinary variables
in C# (for reference, not value, types) or Java. But there is a slight
difference, which makes OOLang references remind of C pointers, namely that one
can put a new value into the same reference cell (which is not possible in Java
or C#, where one can just mutate the value or make a reference variable point
to another reference). There is a `ref` operator for creating new references,
`:=` for assigning values to references and `!` for reading reference values.
The following examples demonstrates these operators and a declaration of a
`Ref` variable (note that `=` is used for initialisation):

~~~
def references : Unit
  x : Ref Int = ref 0;
  x := 1;
  y : Int = !x;
end
~~~

`Ref` variables (unlike `Mutable` variables) can be passed around, so working
with references is considered impure.

### Maybe

Variables in OOLang (unlike C# or Java) cannot be `null`. Instead, a built-in
`Maybe` type found in many functional programming languages is used. There are
two literal values for the `Maybe` type: `just` which is used together with
some value and `nothing` which has to be annotated with a type:

~~~
x : Maybe Int = just 1;
y : Maybe Int;
z : Maybe Int = nothing [Int];
~~~

There is a binary operator which we call "nothing coalesce" (similar to the
"null coalesce" operator in C#). It allows in one expression to check whether
the left operand is `nothing` and if it is, the value of the right operand is
returned, otherwise the value under `just` in the left operand itself is
returned:

~~~
nothing [Int] ?? 0  # Evaluates to 0
~~~

As was mentioned above, variables of type `Maybe` can be left uninitialised.
In this case they will get the value `nothing` (which will be possible to
change later on, but only for `Mutable` and `Ref` variables):

~~~
def assignmentsMaybe : Pure Unit
  x : Mutable (Maybe Int);
  x <- just 1;
end

def referencesMaybe : Pure Unit
  x : Ref (Maybe Int);
end
~~~

This is also one case in OOLang, when working with `Ref` is considered pure
(`Ref` declaration without initialisation).

### Classes

OOLang supports classes with inheritance similar to C# and Java. By default all
methods are virtual, meaning that they can be overriden. Overloading of methods
is not supported.  In addition to methods, classes can also contain data
fields.  Class methods are basically OOLang functions, but inside a class
definition and with access to class data fields.

The following example contains a simple class hierarchy consisting of two
classes with fields and a method, which is overriden in the subclass:

~~~
class Parent
  parentField : Int = 1;

  def method : {x : Int} -> Pure Int
    self.parentField;
  end
end

class Child < Parent
  childField : Bool = true;

  def method : {x : Int} -> Pure Int
    when self.childField do
      x;
    otherwise
      super.method x;
    end;
  end

  def getParent : Pure Parent
    super;
  end
end
~~~

All class members have to be accessed through a special variable `self` inside
the class. Class fields cannot be accessed from the outside of the class. Super
class members can be accessed through the `super` variable.

Unfortunately, OOLang does not support `Mutable` class fields in the code
generator. We will describe this issue in the "Code generation" section.

The next example demonstrates constructing objects and working with them:

~~~
parentObj = Parent.new;
parentObj.method 1;

mChild : Maybe Child = just Child.new;
mParent : Maybe Parent = mChild ? getParent;
~~~

Object are created using a special construct `ClassName.new`, which does not
have any parameters in the current state of OOLang, meaning that OOLang classes
do not support custom constructors. Default constructor which initialises the
fields is generated.  Methods are applied to arguments in the same way as
functions. There is a special syntax (`?`) for calling methods on objects
wrapped in `Maybe`. If the object variable has `just` value, then the method is
called on the underlying object and the result is wrapped in `just`, otherwise,
`nothing` is returned.  This operator is inspired by the `?.` operator in C#
6.0.

Since OOLang supports inheritance, one of the most interesting parts of its
type system is the subtyping relation. It is also used for checking the usage
of other types than classes. Note that it is not used for purity checking.

Subtyping in OOLang is reflexive (any type is a subtype of itself):

\infax{T <: T}

If class `A` inherits from class `B` then `A` is a subtype of `B`:

\infrule{class\ A < B ...}{A <: B}

Subtyping relation is also transitive:

\infrule{A <: B \andalso B <: C}{A <: C}

For `Pure` types the following holds:

\infrule{A <: B}{Pure\ A <: B}
\infrule{A <: B}{A <: Pure\ B}
\infrule{A <: B}{Pure\ A <: Pure\ B}

So, basically `Pure` is erased and subtyping is checked for the underlying
types, meaning that `Pure` types are interchangeable with other types. This is
why this relation is not used for purity checking (a separate process is used).

Mutables are treated by value, so they can be used in the same places as
ordinary types, except for special declaration and assignment operators
usage. And this gives us rules similar to the ones for `Pure`:

\infrule{A <: B}{Mutable\ A <: B}
\infrule{A <: B}{A <: Mutable\ B}
\infrule{A <: B}{Mutable\ A <: Mutable\ B}

`Ref` types are invariant. There is a known problem with generic arrays being
covariant in several popular programming languages, like Java and C#, which is
explained, for example, in \cite{Birka}.

`Maybe` type is covariant:

\infrule{A <: B}{Maybe\ A <: Maybe\ B}

## Code generation

In this section we will describe the code generation from OOLang to MIL.

The same way as with FunLang, we needed to decide on the monad stacks for
different kinds of OOLang computations. As was described above, OOLang has pure
and impure computations. Pure computations do not perform input/output and do
not manipulate state (with `Ref`s), but they can throw exceptions. Therefore,
the pure stack for OOLang is `Error Unit`. Similarly to FunLang, OOLang
exceptions do not carry values. Impure computations add `State` and `IO`. We
decided to order `Error` and `State` differently from FunLang to *not* get the
state rollback semantics, so we have `Error Unit ::: (State ::: IO)` for impure
OOLang computations. Again, the pure stack is a prefix of the impure stack
(they satisfy the $isCompatible$ relation) to be able to easily run pure
computations inside impure ones.

### General scheme and type conversions

The general approach to code generation and type conversions for OOLang is
rather similar to what was described in the FunLang chapter. Every function
type needs to be converted to the corresponding MIL type. There is one
significant difference in OOLang, though.  We know that no effects can happen
between partial applications of a function up until the return type, therefore
a monad is put only on top of the return type of the function. This is because
in FunLang every parameter needs a lambda binder, which could potentially be
substituted with `throw`, but in OOLang all parameter binders are already there
(using MIL lambda expressions without any effects) and it is only the return
type which is determined by the function body and therefore needs to capture
potential effects. For return types, `Pure A` results in `Error Unit A` and `A`
results in `(Error Unit ::: (State ::: IO)) A`.

Similarly to FunLang, sub-expressions are bound to variables with the monadic
$bind$. OOLang statement sequences result in sequences of monadic $bind$s.
Since every statement has a value, that value is what is bound to a variable.
For some statements it is just a `Unit` variable.

OOLang `when` statements are expressed using the `case` expression to pattern
match on the `Bool` condition. The return value of the statement is bound to a
variable. The following example tries to capture most of what has been
described about functions and statements so far:

~~~
def fun : {a : Int} -> {b : Bool} -> Unit
  when false do
    1;
    2;
  otherwise
    3;
  end;
  unit;
end

fun : Int -> Bool -> (Error Unit ::: (State ::: IO)) Unit =
  \(a : Int) -> \(b : Bool) ->
    let (var_2 : Int) <-
      let (var_0 : Bool) <-
        return [Error Unit ::: (State ::: IO)] False
      in case var_0 of
           | True =>
               let (var_1 : Int) <-
                 return [Error Unit ::: (State ::: IO)] 1
               in return [Error Unit ::: (State ::: IO)] 2
           | False =>
               return [Error Unit ::: (State ::: IO)] 3
         end
    in return [Error Unit ::: (State ::: IO)] unit;
~~~

Some of the more specific statements (declarations, assignments, exception
handling etc.) as well as class definitions will be covered in separate
sub-sections.

One of the challenges in the OOLang code generator was to correctly generate
code for variable and function occurences. In order to do this correctly, one
needs to realise that there are four different kinds of variable/function
occurences in OOLang:

* local variables of value types (non-function, primitive types)
* local variables of function types
* global functions with parameters
* global functions without parameters

For the first three cases, there is no monad at the front of the type. This
means that the code generator needs to emit `return` in order to make an
expression monadic. In the fourth case, either the pure stack or the impure
stack is in the type, so no `return` is needed.

### Built-in types and functions

Most of what was described for FunLang built-in types and functions holds for
OOLang as well, so we will omit repeating those points and will concentrate on
the things specific to OOLang.

Since OOLang provides the built-in `Maybe` type, the following definition for
`Maybe` is generated for every OOLang program:

~~~
type Maybe A
  = Nothing
  | Just A;
~~~

A difference from FunLang is that `Bool` is a built-in data type with literals,
rather than an ADT, so there are built-in function for printing and reading
boolean values. The `Bool` data type itself maps to the `Bool` ADT in MIL. The
following code snippet is an implementation of the `printBool` function:

~~~
printBool : Bool -> (Error Unit ::: (State ::: IO)) Unit =
  \(b_ : Bool) ->
    case b_ of
      | True => printString
          (Cons_Str 't'
          (Cons_Str 'r'
          (Cons_Str 'u'
          (Cons_Str 'e' Empty_Str))))
      | False => printString
          (Cons_Str 'f'
          (Cons_Str 'a'
          (Cons_Str 'l'
          (Cons_Str 's'
          (Cons_Str 'e' Empty_Str)))))
    end;
~~~

It basically just pattern matches on the argument and outputs a corresponding
`String`.

`readBool` is quite a long function, which basically reads characters with
`read_char` one by one and pattern matches on them in order to find `true` or
`false` as a sequence of characters. In case of failure it uses `throw_error`.

### Mutability and references

OOLang has three kinds of variables: default immutable variables, `Mutable`s
and `Ref`s. Declaration statements work similarly for all of them. The code
generator emits a monadic $bind$ that introduces the variable being declared in
scope and the rest of the statements become a body of the $bind$ expression and
therefore they can access the declared variable.

`Mutable` variables are basically erased during the code generation. The MIL
code produced for `Mutable`s resembles the SSA/ANF-style code. Every assignment
introduces a new variable in scope. This variable has an increasing counter as
part of its name. The code generator internally keeps track of what is the
current variable name to correctly emit the variable access.

OOLang `Ref` variables are compiled down to the MIL references. There is a
direct mapping between the operations on references: `ref` corresponds to
`new_ref`, `:=` corresponds to `write_ref` and `!` corresponds to `read_ref`.

The following example demonstrates both `Mutable` and `Ref` declaration,
assignment and access (declarations of immutable variables and their occurences
look exactly as for `Mutable`s, so we omit them):

~~~
def main : Unit
  a : Mutable Int <- 0;
  a <- 1;
  a;
  a <- 2;
  a;
  r : Ref Int = ref 0;
  r := 1;
  !r;
  unit;
end

main : (Error Unit ::: (State ::: IO)) Unit =
  let (a : Int) <-
    return [Error Unit ::: (State ::: IO)] 0
  in let (a_1 : Int) <-
    return [Error Unit ::: (State ::: IO)] 1
  in let (var_0 : Int) <-
    return [Error Unit ::: (State ::: IO)] a_1
  in let (a_2 : Int) <-
    return [Error Unit ::: (State ::: IO)] 2
  in let (var_1 : Int) <-
    return [Error Unit ::: (State ::: IO)] a_2
  in let (r : Ref Int) <-
       let (var_6 : Int) <-
         return [Error Unit ::: (State ::: IO)] 0
       in lift [State => Error Unit ::: State] new_ref [Int] var_6
  in let (var_5 : Unit) <-
       let (var_4 : Int) <-
         return [Error Unit ::: (State ::: IO)] 1
       in lift [State => Error Unit ::: State] write_ref [Int] r var_4
  in let (var_3 : Int) <-
       let (var_2 : Ref Int) <-
         return [Error Unit ::: (State ::: IO)] r
       in lift [State => Error Unit ::: State] read_ref [Int] var_2
  in return [Error Unit ::: (State ::: IO)] unit;
~~~

### Exceptions

When it comes to the code generation for exception handling, `try` block
corresponds to the first parameter of the `catch_error` function, while `catch`
block (with a lambda binder for the `Unit` error value on top) corresponds to
the second one. Since all OOLang statements and statement blocks result in MIL
expressions, the corresponding expressions are just passed as arguments to
`catch_error`.

One interesting consideration here is that we need to make sure that `finally`
block is executed after `try` and `catch`. For this reason, we bind the result
of `catch_error` to a variable, then bind the result of `finally` and then
return the `catch_error` result as the result of the whole statement.

The next example shows a translation for `try-catch-finally` statement:

~~~
def fun : Pure Unit
  try
    throw [Unit];
  catch
    unit;
  finally
    1;
  end;
end

fun : Error Unit Unit =
  let (var_0 : Unit) <-
    catch_error_1 [Unit] [Unit]
      (throw_error [Unit] [Unit] unit)
      (\(error_ : Unit) -> return [Error Unit] unit)
  in let (var_1 : Int) <- return [Error Unit] 1
  in return [Error Unit] var_0;
~~~

There is exactly the same problem with `catch_error` type as in the FunLang
case, therefore the type checker specifies the appropriate monad stacks for
`catch_error_1` and `catch_error_2`.

### Classes

In order to represent OOLang classes, MIL tuples are used. Every class maps to
a type, which is a tuple with two elements (which are tuples themselves)
representing data fields and methods. Since MIL tuples have both width and
depth subtyping, new data fields and/or methods can be added in subclasses and
it will still be possible to work with a tuple representing a subclass object
as if it were a tuple for a superclass object, for example, pass as function
arguments, pattern match with the `case` expressions etc. Note that tuples for
subclasses contain all of the superclass fields and methods before their own,
if any.  The inspiration for the representation of OOLang classes comes from
\cite{TAPL}.

For every class definition three MIL functions are generated:

* `new_ClassName_Data` function, which we call a *class data constructor
  function*. This is a function that creates the data part of an object tuple.
  It contains generated code for field initialisation. The following example
  contains two class definitions in OOLang and two class data constructor
  function definitions in MIL:

    ~~~
    class Parent
      parentField : Int = 1;
      parentField2 : Bool = true;
    end

    class Child < Parent
      childField : Float = 0.01;
    end

    new_Parent_Data : Error Unit {Int, Bool} =
      let (self_parentField : Int) <- return [Error Unit] 1
      in let (self_parentField2 : Bool) <- return [Error Unit] True
      in return [Error Unit] {self_parentField, self_parentField2};

    new_Child_Data : Error Unit {Int, Bool, Float} =
      let (self_parentField : Int) <- return [Error Unit] 1
      in let (self_parentField2 : Bool) <- return [Error Unit] True
      in let (self_childField : Float) <- return [Error Unit] 1.0e-2
      in return [Error Unit] { self_parentField
                             , self_parentField2
                             , self_childField};
    ~~~

    Note that the subclass just duplicates the superclass field
    initialisations. It is also worth highlighting that right now the code
    generator does not support referencing superclass fields in field initialisers.
    We think that adding this should not cause any problems.

* `class_ClassName` function, which we call a *class definition function*. This
  function is where the code for class methods is contained. It takes a class
  data tuple as a parameter and returns the full class type. Method types are
  translated as all the other function types with one addition. Method types get
  an additional parameter of type `Unit`. The reason is that we do not want to
  evaluate methods at the point of their definition and such a parameter helps to
  introduce laziness, that is needed in this case. This also means that the code
  generator needs to supply an additional argument when applying methods.  Method
  definitions need to be able to access `self` and if a class has a superclass,
  then `super` has to be available as well. The `super` variable is just bound to
  a constructed instance of the super class. The `self` case is a bit more
  interesting, since when we are defining methods, we are in the process of
  defining `self`. This is where the `let rec` expression is used to have a
  recursive binding for `self`.

    The following example contains a simple class definition function:

    ~~~
    class Parent
      def method : {x : Int} -> Pure Int
        0;
      end
    end

    class_Parent : {} -> Error Unit {{}, {Unit -> Int -> Error Unit Int}} =
      \(self_data : {}) ->
        let rec (self : {{}, {Unit -> Int -> Error Unit Int}}) <-
                  { self_data
                  , {\(lazy_ : Unit) ->
                       \(x : Int) -> return [Error Unit] 0}}
        in return [Error Unit] self;
    ~~~

    The next example demonstrates how a class definition for a subclass looks:

    ~~~
    class Child < Parent
      def method : {x : Int} -> Pure Int
        x;
      end

      def childMethod : Pure Bool
        true;
      end
    end

    class_Child : {} -> Error Unit
      {{}, { Unit -> Int -> Error Unit Int
           , Unit -> Error Unit Bool}} =
      \(self_data : {}) ->
        let (super : {{}, {Unit -> Int -> Error Unit Int}}) <- new_Parent
        in let rec (self : {{}, { Unit -> Int -> Error Unit Int
                                , Unit -> Error Unit Bool}}) <-
           case super of
           | { super_data : {}
             , super_methods : {Unit -> Int -> Error Unit Int}} =>
             case super_methods of
             | {super_method : Unit -> Int -> Error Unit Int} =>
                 { self_data
                 , {\(lazy_ : Unit) ->
                      \(x : Int) ->
                        return [Error Unit] x,
                    \(lazy_ : Unit) ->
                        return [Error Unit] True}}
             end
           end
        in return [Error Unit] self;
    ~~~

    Note that in the example above the `method` from superclass is overriden in
    the subclass, so a new definition is put in its place. In the case, when a
    subclass does not override a method, a reference to the corresponding
    superclass method will be used as a method tuple element. The superclass
    instance is decomposed with a sequence of `case` expressions and all its data
    fields and methods are available.

    One of the most interesting observations to make here is that the
    subclassing polymorphism is taken care of by storing methods' lambda
    expressions in a tuple. This tuple is essentially a classical *dispatch table*
    \cite{ModernCompilerDesign}. The ability to capture references to the `self`
    and `super` objects is powered by *closures* (data structures for storing a
    function together with its environment, which are heavily used in implementaion
    of functional programming languages).

    One of the main differences of the approach to class and object
    representation which was taken here compared to the more classical one
    described, for example, in \cite{ModernCompilerDesign}, is the access to the
    `self` object. Usually one would have a special parameter to every method,
    which represents `self` and would be passed as an argument to every method
    call. This would allow to avoid a recursive definition and a lazy parameter to
    every method. Given that some representation of methods' types is part of a
    class type (most likely via the dispatch table), this approach works well in
    languages and IRs with a type system not as strict as the one in MIL, namely,
    when having very general pointer or address types to represent method types and
    being able to cast them.  In MIL, having `self` as a parameter in every method
    would require its type to be in every method type, which would make the class
    type recursive. One could think solving this with having an ADT instead of a
    tuple for every class type, but that would require some kind of subtyping for
    ADTs.

* `new_ClassName` function, which corresponds to the `ClassName.new` construct
  in OOLang. It basically puts together `new_ClassName_Data` and
  `class_ClassName`:

    ~~~
    new_Parent : Error Unit
        { {Int, Bool}
        , {Unit -> Int -> Error Unit Int}} =
      let (self_data : {Int, Bool}) <- new_Parent_Data
      in class_Parent self_data;
    ~~~

An interesting caveat of the code generation for class definitions is that it
should process classes in order from superclasses down to subclasses (for
example, it generates field initialisers for a superclass which then will be
used when generating a class data constructor function for a subclass).
Therefore, the code generator performs sorting of classes by their depth in the
class hierarchy.

Class member access is performed using pattern matching in MIL. The first
`case` expression "separates" data fields from methods. The second one
introduces data fields into the scope (as variable binders in a tuple pattern)
and the third one does the same for methods:

~~~
def fun : {obj : Parent} -> Pure Int
  obj.method 1;
end

fun : {{}, {Unit -> Int -> Error Unit Int}} -> Error Unit Int =
  \(obj : {{}, {Unit -> Int -> Error Unit Int}}) ->
    let (var_3 : Int -> Error Unit Int) <-
      let (var_0 : {{}, {Unit -> Int -> Error Unit Int}}) <-
        return [Error Unit] obj
      in case var_0 of
           | {var_1 : {}, var_2 : {Unit -> Int -> Error Unit Int}} =>
               case var_1 of
                 | {} =>
                     case var_2 of
                       | {method : Unit -> Int -> Error Unit Int} =>
                           return [Error Unit] method unit
                     end
               end
         end
    in let (var_4 : Int) <- return [Error Unit] 1
    in var_3 var_4;
~~~

Member access for objects wrapped in `Maybe` has one additional level of
pattern matching on the value of the `Maybe` data type.

As we mentioned above, OOLang classes cannot have `Mutable` fields. They would
be just pure immutable values in an object tuple, which class methods could not
mutate. One could think about returning a new tuple for a mutated object, but
this does not really work, since it would not be possible for other methods to
see this change and get the new state of the object.

### Non-termination

Looking at most of modern programming languages and the effects programs
written in them can have, one would really want to express the non-terminating
nature of computations. The original version of MIL had a built-in `NonTerm`
monad, but later in the project it was scoped out for several reasons.

Non-termination is a very non-trivial effect to reason about. Usually, *lifted*
types are used to express the types of non-terminating computations
\cite{BridgingTheGulf}, \cite{Tolmach}. Very informally, one could use the
`Maybe` type as an underlying intuition for the *lifted* types, where `Nothing`
represents non-termination (or *bottom*).

OOLang (as well as FunLang) could have something like `Error Unit ::: NonTerm`
as its pure stack (in this discussion we will disregard `State` that FunLang
has in its pure stack). Unrolling this type gives `Maybe (Either e a)`, which
is rather close to what one would think about computations of type `Error Unit
::: NonTerm`. They can terminate or not and if they do, they either return an
error or a result. If we extend this stack to model impure OOLang computations,
while still maintaining the property that the pure stack is a prefix of the
impure one, we will get `Error Unit ::: (NonTerm ::: (State ::: IO))`.
Unrolling this type produces `s -> IO (Maybe (Either e a), s)`, which does not
really make sense, because what it captures is that a computation always
returns a state value even if it does not terminate. We can try to fix this by
switching the order of `NonTerm` and `State`: `Error Unit ::: (State :::
(NonTerm ::: IO))`. This stack gives us `s -> IO (Maybe (Either e a, s))` which
is better, since now the state value is under `Maybe`, but we cannot combine
this with pure computations of type `Error Unit ::: NonTerm` because of the
`State` monad transformer in between. One way of working around this is to have
`State ::: (Error Unit ::: (NonTerm ::: IO))` as the impure stack. Then the
pure stack with the help of the monad transformer $lift$ operation starts to
satisfy the $isCompatible$ relation. But in this case the intermediate language
forces the source language to have a particular semantics (the ordering of
`State` and `Error`).

The question of how to incorporate non-termination into MIL remains open. One
can partly relate this to the problem with the `catch_error` type raised in
this and the previous chapter in the sense that MIL stacks are rather fixed and
also order-dependent. This, of course, gives us benefits of expressing
different semantics, but at the same time it cuts off the flexibility that
might be available with a more set-based representation of effect combinations.
Maybe, some sort of polymorphism in MIL stacks and an ability to have *monad
variables* like, e.g. `Error Unit ::: (m ::: NonTerm)` would help in solving
this problem, but we did not explore the solution space further in this
project.

## Conclusions

Modern object-oriented programming languages incorporate many of the ideas
introduced and used for quite a long time in functional programming. OOLang
tries to follow the same path, which makes MIL a good fit to be a target
language for OOLang.

Despite the mismatch in the major programming paradigms supported by OOLang and
MIL, the current state of MIL allowed to encode most of the OOLang features.
OOLang classes and inheritance were a motivation to introduce tuples with
subtyping to MIL.

Unfortunately, there is no good way in MIL to express OOLang `Mutable` class
fields at the moment.

Many of the observations made in the corresponding section in the previous
chapter apply here as well. There are problems with capturing a desirable type
for `catch_error` as well as with the non-termination monad.

