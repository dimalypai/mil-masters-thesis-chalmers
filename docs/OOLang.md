% Object-Oriented Programming Language
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

Example program
===============

```haskell
def main : Unit =>
  x : Mutable Int <- 1;
  y : Int = 1;
  z : Int = if x = 1 then 2 else 3;
  x1 : Int = !x; // ! : Mutable/Ref A -> A
  f (\y -> y) !x;
end

def f : (func : Int -> Int) -> (n : Int) -> Unit =>
  h {n, true};
end

def pure g : (p : {Int, Bool}) -> (m : Maybe Int) -> Maybe Int =>
  i : Mutable Int <- 0;
  x1 : Mutable (Maybe Int);
  x2 : Maybe Int; // nothing, immutable
  x1 <- just 0;
  n : Int = m ?? 0; // ?? : Maybe A -> A -> A
  while i < n do
    i++;
  end;
  just !i;
end

def h : {fst : Int, snd : Bool} -> Unit =>
  obj1 : Mutable Circle <- Circle.new;
  obj2 : Mutable (Maybe Circle) <- nothing;
  ft : Int = Circle::fortyTwo;
  a : Int = obj1.area;
  // ? : [Mutable/Ref] Maybe A -> A.method (: [C ->] B) -> [Mutable/Ref?] Maybe B
  ma : Maybe Int = obj2 ? area;
end

class Shape =>
  private field : Int = 1;
  private mutField : Mutable Int <- 2;
  private refField : Ref Int = new 1;

  public def new => end

  public def draw : Unit => unit; end
end

class Circle < Shape =>
  public static fortyTwo : Int = 42;
  public static pi : Float = 3.14;
  private radius : Mutable Int <- 0;

  public def setRadius : (r : Int) -> Unit =>
    self.radius <- r;
  end

  public def pure area : Int =>
    pi * radius * radius;
  end
end

```

Syntax
======

-------------  -------  --------------------------------------------------------------  -------------------------------
    *program*   $\to$   *topdefs*                                                       top-level definitions

    *topdefs*   $\to$   *topdef*

                  |     *topdef* *topdefs*

     *topdef*   $\to$   *classdef*

                  |     *fundef*

   *classdef*   $\to$   `class` *classname* *optsuper* `=>` *classbody* `end`

     *fundef*   $\to$   `def` *optpure* *funname* `:` *funtype* `=>` *funbody* `end`

  *classbody*   $\to$   *memberdecls*

    *funbody*   $\to$   *statements*

*memberdecls*   $\to$   $\varepsilon$

                  |     *memberdecl* *memberdecls*

 *memberdecl*   $\to$   *fielddecl*

                  |     *methoddecl*

  *fielddecl*   $\to$   *modifiers* *declaration* `;`

 *methoddecl*   $\to$   *modifiers* *fundef*

 *statements*   $\to$   $\varepsilon$

                  |     *statement* `;` *statements*

  *statement*   $\to$   *declaration*

                  |     *expr*

                  |     *assignment*

       *type*   $\to$   *maybetype*

                  |     `Mutable` *maybetype*

                  |     `Ref` *maybetype*

                  |     *type* `->` *type*

  *maybetype*   $\to$   *atomtype*

                  |     `Maybe` *atomtype*

   *atomtype*   $\to$   `Unit`

                  |     `Int`

    *funtype*   $\to$   *type*

                  |     *argtype* `->` *funtype*

    *argtype*   $\to$   `(` *name* `:` *type* `)`

   *optsuper*   $\to$   $\varepsilon$

                  |     `<` *supers*

     *supers*   $\to$   *classname*

                $\to$   *classname* `,` *supers*

    *optpure*   $\to$   $\varepsilon$

                  |     `pure`

  *modifiers*   $\to$   *modifier*

                  |     *modifier* *modifiers*

   *modifier*   $\to$   `public`

                  |     `private`

                  |     `static`

  *classname*   $\to$   *upper* *alphanum*

    *funname*   $\to$   *lower* *alphanum*

       *name*   $\to$   *lower* *alphanum*
-------------  -------  --------------------------------------------------------------  -------------------------------

Typing
======

