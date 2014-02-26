% Object-Oriented Programming Language
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

Example program
===============

```Cs
fun main : Unit =>
  mutable x : int <- 1;
  x : Mutable Int <- 1;
  y : Int = 1;
  z : Int = if x = 1 then 2 else 3;
end

fun f : (n : Int) -> Unit =>
  h {n, true};
end

pure fun g : (m : Int) -> (n : Int) -> Maybe Int =>
  mutable i : int;
  i : Mutable Int;
  i <- 0;
  while (i < n) do
    i++;
  end;
  just m;
end

fun h : {fst : Int, snd : Bool} -> Unit =>
  ...
end

class Shape =>
  private field : int;
  private mutable mutField : int;
  private refField : ref int;

  field : private Int = 1;
  mutField : private Mutable Int <- 2;
  refField : private Ref Int = new 1;

  private field : Int = 1;
  private mutField : Mutable Int <- 2;
  private refField : Ref Int = new 1;

  public new => ...

  public draw : Unit => ...
end

class Circle < Shape =>
  private static fortyTwo : Int = 42;
  fortyTwo : private static Int = 42;

  public setRadius : (r : Int) -> Unit =>
    ...
  end

  public pure area : Int =>
    ...
  end
end

```

Syntax
======

-------------  -------  --------------------------------------------------------------  -------------------------------
    *program*   $\to$   *topdefs*                                                       top-level definitions

    *topdefs*   $\to$   $\varepsilon$

                  |     *topdef* *topdefs*

     *topdef*   $\to$   *classdef*

                  |     *fundef*

   *classdef*   $\to$   `class` *classname* *optsuper* `=>` *classbody* `end`

     *fundef*   $\to$   *optpure* `fun` *funname* `:` *funtype* `=>` *funbody* `end`

   *optsuper*   $\to$   $\varepsilon$

                  |     `<` *supers*

    *optpure*   $\to$   $\varepsilon$

                  |     `pure`

     *supers*   $\to$   *classname*

                $\to$   *classname* `,` *supers*

  *classbody*   $\to$   *memberdecls*

    *funtype*   $\to$   *type*

                  |     *argtype* `->` *funtype*

    *argtype*   $\to$   `(` *name* `:` *type* `)`

   *atomtype*   $\to$   `Unit`

                  |     `Int`

       *type*   $\to$   *atomtype*

                  |     `Mutable` *atomtype*

                  |     `Ref` *atomtype*

                  |     *type* `->` *type*

    *funbody*   $\to$   *statements*

*memberdecls*   $\to$   $\varepsilon$

                  |     *memberdecl* *memberdecls*

 *statements*   $\to$   $\varepsilon$

                  |     *statement* `;` *statements*

 *memberdecl*   $\to$   *fielddecl*

                  |     *methoddecl*

  *statement*   $\to$   *namedecl*

                  |     *expr*

  *classname*   $\to$   *upper* *alphanum*

    *funname*   $\to$   *lower* *alphanum*

       *name*   $\to$   *lower* *alphanum*
-------------  -------  --------------------------------------------------------------  -------------------------------

Typing
======

