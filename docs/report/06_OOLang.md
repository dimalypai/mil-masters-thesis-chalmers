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
functions are impure. Impurity in OOLang comes mainly from input/output and
assignments to references. Function definitions can contain parameters in curly
braces with a function arrow (`->`) separating several parameters, which means
that functions are curried. Function application is expressed using
juxtaposition. If a function definition has parameters, its return type is
separated from them with `=>`.  Unlike many other programming languages with
statements, there is no `return` statement in OOLang.  Instead, every statement
has a value (and a type) and so the value of the last statement is what is
returned from a function.

The following example demonstrates what was described in the previous
paragraph:

~~~
def main : Unit
  fun 1;
  unit;
end

def fun : {a : Int} -> {b : Float} => Pure Int
  a;
end
~~~

OOLang supports higher-order functions:

~~~
def main : Unit
  applyInt idInt 1;
  unit;
end

def applyInt : {f : Int -> Pure Int} -> {x : Int} => Pure Int
  f x;
end

def idInt : {x : Int} => Pure Int
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
`try` and `catch` block must have the same type. The type of `finally` does not
matter, since its value is not returned.

~~~
try
  throw [Int];
  1;
catch
  2;
finally
end;
~~~

OOLang has `Unit` (the type of the `unit` literal), `Bool` (which has literals
`true` and `false` as possible values), `Int`, `Float`, `Char` and `String` as
built-in types. It supports the usual infix arithmetic operations: `+`, `-`,
`*` and `/`.

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
they cannot change any global state outside of the function.

### References

OOLang also supports references, which are similar to ordinary variables in C#
(for reference, not value, types) or Java. There is a `ref` operator for
creating new references, `:=` for assigning values to references and `!` for
reading reference values. The following examples demonstrates these operators
and a declaration of a `Ref` variable (note that `=` is used for
initialisation):

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

There is a binary nothing coalesce operator (similar to the null coalesce
operator in C#), which allows in one expression to check whether the left
operand is `nothing` and if it is, the value of the right operand is returned,
otherwise the value under `just` in the left operand itself is returned:

~~~
nothing [Int] ?? 0
~~~

As it was mentioned above, variables of type `Maybe` can be left uninitialised.
In this case they will get the value `nothing`. This makes sense only with
`Mutable` and `Ref` variables, since for immutable variables there will be no
chance to change the value later on:

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
class Super
  superField : Int = 1;

  def method : {x : Int} => Pure Int
    self.superField;
  end
end
~~~

~~~
class Child < Super
  childField : Bool = true;

  def method : {x : Int} => Pure Int
    when self.childField do
      x;
    otherwise
      super.method;
    end;
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
superObj = Super.new;
superObj.method 1;

mChild : Maybe Child = just Child.new;
mSuper : Maybe Super = mChild ? getSuper;
~~~

Object are created using a special construct `ClassName.new`, which does not
have any parameters in the current state of OOLang, meaning that OOLang classes
do not support custom constructors. Default constructor which initialises the
fields is generated.  Methods are applied to arguments in the same way as
functions. There is a special syntax (`?`) for calling methods on objects
wrapped in `Maybe`. If the object variable has `just` value, then the method is
called on the underlying object and the result is wrapped in `just`, otherwise,
`nothing` is returned.  This operator is inspired by `?.` operator in C# 6.0.

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

References are covariant:

\infrule{A <: B}{Ref\ A <: Ref\ B}

`Maybe` type is covariant as well:

\infrule{A <: B}{Maybe\ A <: Maybe\ B}

## Code generation

* General outline
    + Monads
    + Type conversions
* Examples
    + Pure functions
    + Impure functions
    + Classes \cite{TAPL}
* Problems

## Conclusions

* Introduced tuples with subtyping to the MIL
* Rather complex type system increases the complexity of getting the
  type-correct MIL

