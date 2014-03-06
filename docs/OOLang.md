% Object-Oriented Programming Language
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

Example program
===============

```haskell
def main : Unit =>
  x : Mutable Int <- 1;
  y : Int = 1;
  z : Int = if x = 1 then 2 else 3;
  when x = 1 do
    x <- x + 1;
  otherwise
    x <- x - 1;
  end;
  x1 : Int = !x; // ! : Mutable/Ref A -> A
  f (\(y : Int) -> y) !x;
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
    i <- i + 1;
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

                  |     *whileloop*

                  |     *when*

*declaration*   $\to$   *name* `:` *type* *optinit*

    *optinit*   $\to$   $\varepsilon$

                  |     `=` *expr*

                  |     `<-` *expr*

       *expr*   $\to$   *literal*

                  |     *name*

                  |     *expr expr*

                  |     *expr*`.`*expr*

                  |     *expr* `?` *expr*

                  |     `\` *namebinders* `->` *expr*

                  |     *classname*`::`*expr*

                  |     *expr* `??` *expr*

                  |     `new` *expr*

                  |     `!`*expr*

                  |     *expr* `+` *expr*

                  |     *expr* `-` *expr*

                  |     *expr* `*` *expr*

                  |     *expr* `/` *expr*

                  |     *expr* `=` *expr*

                  |     *expr* `/=` *expr*

                  |     *expr* `<` *expr*

                  |     *expr* `>` *expr*

                  |     *expr* `<=` *expr*

                  |     *expr* `>=` *expr*

                  |     `if` *expr* `then` *expr* `else` *expr*

                  |     `just` *expr*

                  |     `nothing`

*namebinders*   $\to$   *namebinder*

                  |     *namebinder* *namebinders*

 *namebinder*   $\to$   `(` *name* `:` *type* `)`

 *assignment*   $\to$   *expr* `<-` *expr*

                  |     *expr* `:=` *expr*

                  |     *expr* `=` *expr*

  *whileloop*   $\to$   `while` *expr* `do` *statements* `end`

       *when*   $\to$   `when` *expr* `do` *statements* `otherwise` *statements* `end`

       *type*   $\to$   *maybetype*

                  |     `Mutable` *maybetype*

                  |     `Ref` *maybetype*

                  |     `(` *type* `)`

  *maybetype*   $\to$   *atomtype*

                  |     `Maybe` *atomtype*

                  |     `(` *maybetype* `)`

   *atomtype*   $\to$   `Unit`

                  |     `Int`

                  |     *classname*

                  |     *type* `->` *type*

    *funtype*   $\to$   *type*

                  |     *namebinder* `->` *funtype*

    *literal*   $\to$   `unit`

                  |     *intlit*

   *optsuper*   $\to$   $\varepsilon$

                  |     `<` *classname*

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

---------  -------  -----------------------------------  -----------------------
 $\Gamma$   $\to$   $\varnothing$                        empty context

              |     $\Gamma$`,` *var* `:` *type*         variable binding

              |     $\Gamma$`,` *funname* `:` *type*     function binding

              |     $\Gamma$`,` *classname*              class name
---------  -------  -----------------------------------  -----------------------

\infax[T-ClassDefsEmpty]{\Gamma \vdash \emph{empty classdefs}\ valid\ |\ \Gamma}

\infrule[T-ClassDefs]{\Gamma \vdash \emph{classdef}\ valid\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{classdefs}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{classdef:classdefs}\ valid\ |\ \Gamma_2}

\infrule[T-ClassDef]{C \notin \Gamma \andalso \Gamma, C \vdash \emph{classbody}\ valid\ |\ \Gamma_1}{\Gamma \vdash \texttt{class C => } \emph{classbody}\ end\ valid\ |\ \Gamma_1}

\infrule[T-ClassDefInherit]{C \notin \Gamma \andalso S \in \Gamma \andalso S \not<: C \andalso \Gamma, C \vdash \emph{classbody}\ valid\ |\ \Gamma_1}{\Gamma \vdash \texttt{class C < S => } \emph{classbody}\ end\ valid\ |\ \Gamma_1}

\infax[T-SubRefl]{\texttt{C <: C}}

\infrule[T-Inherit]{\texttt{class C < S => } \emph{classbody}\ end\ valid}{\texttt{C <: S}}

\infrule[T-SubTrans]{\texttt{C <: S} \andalso \texttt{S <: T}}{\texttt{C <: T}}

\infax[T-FunDefsEmpty]{\Gamma \vdash \emph{empty fundefs}\ valid\ |\ \Gamma}

\infrule[T-FunDefs]{\Gamma \vdash \emph{fundef}\ valid\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{fundefs}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{fundef:fundefs}\ valid\ |\ \Gamma_2}

\infrule[T-FunDef]{f \notin \Gamma \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash \emph{funbody}\ valid \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash e_n : T_n}{\Gamma \vdash \texttt{def f : (x1 : T1) -> ... -> Tn => } \emph{funbody}\ end\ valid\ |\ \Gamma, f : T_1 \to ... \to T_n}

\infrule[T-FunDefPure]{f \notin \Gamma \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash \emph{funbody}\ purely-valid \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash e_n : T_n}{\Gamma \vdash \texttt{def pure f : (x1 : T1) -> ... -> Tn => } \emph{funbody}\ end\ valid\ |\ \Gamma, f : T_1 \to ... \to T_n}

\infrule[T-Sub]{\Gamma \vdash e : S \andalso \texttt{S <: T}}{\Gamma \vdash e : T}

In these rules the order of class definitions matters, but in implementation we do an additional pass to collect class names before their checking.

