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
    return unit;
  otherwise
    x <- x - 1;
  end;
  x1 : Int = !x; // ! : Mutable/Ref A -> A
  f (\(y : Int) -> y) !x;
end

def f : (func : Int -> Int) -> (n : Int) -> Unit =>
  h n;
end

def pure g : (p : Int) -> (m : Maybe Int) -> Maybe Int =>
  i : Mutable Int <- 0;
  x1 : Mutable (Maybe Int);
  x2 : Maybe Int; // nothing, immutable
  x1 <- just 0;
  n : Int = m ?? 0; // ?? : Maybe A -> A -> A
  while i < n do
    i <- i + 1;
  end;
  y : Mutable Int <- 0;
  try
    y <- 1 / 0;
  catch DivisionByZero
    y <- 1;
  finally
    y <- 2;
  end;
  just !i;
end

def h : (fst : Int) -> Unit =>
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
  private refField : Ref Int = ref 1;

  public def new : Shape => end

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

                  |     *return*

                  |     *trycatch*

*declaration*   $\to$   *var* `:` *type* *optinit*

    *optinit*   $\to$   $\varepsilon$

                  |     `=` *expr*

                  |     `<-` *expr*

       *expr*   $\to$   *literal*

                  |     *var*

                  |     *funname*

                  |     *expr expr*

                  |     *expr*`.`*expr*

                  |     *expr* `?` *expr*

                  |     `\` *varbinders* `->` *expr*

                  |     *classname*`.`*expr*

                  |     *classname*`::`*expr*

                  |     *expr* `??` *expr*

                  |     `ref` *expr*

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

 *varbinders*   $\to$   *varbinder*

                  |     *varbinder* *varbinders*

  *varbinder*   $\to$   `(` *var* `:` *type* `)`

 *assignment*   $\to$   *expr* `<-` *expr*

                  |     *expr* `:=` *expr*

                  |     *expr* `=` *expr*

  *whileloop*   $\to$   `while` *expr* `do` *statements* `end`

       *when*   $\to$   `when` *expr* `do` *statements* `otherwise` *statements* `end`

     *return*   $\to$   `return` *expr*

   *trycatch*   $\to$   `try` *statements* `catch` *statements* `finally` *statements* `end`

       *type*   $\to$   *maybetype*

                  |     `Mutable` *maybetype*

                  |     `Ref` *maybetype*

                  |     `(` *type* `)`

  *maybetype*   $\to$   *atomtype*

                  |     `Maybe` *atomtype*

                  |     `(` *maybetype* `)`

   *atomtype*   $\to$   `Unit`

                  |     `Bool`

                  |     `Int`

                  |     *classname*

                  |     *type* `->` *type*

    *funtype*   $\to$   *type*

                  |     *varbinder* `->` *funtype*

    *literal*   $\to$   `unit`

                  |     *boollit*

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

        *var*   $\to$   *lower* *alphanum*
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

\infax[T-FunDefsEmpty]{\Gamma \vdash \emph{empty fundefs}\ valid\ |\ \Gamma}

\infrule[T-FunDefs]{\Gamma \vdash \emph{fundef}\ valid\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{fundefs}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{fundef:fundefs}\ valid\ |\ \Gamma_2}

\infrule[T-FunDef]{f \notin \Gamma \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash \emph{funbody}\ valid \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash e_n : T_n}{\Gamma \vdash \texttt{def f : (x1 : T1) -> ... -> Tn => } \emph{funbody}\ end\ valid\ |\ \Gamma, f : T_1 \to ... \to T_n}

\infrule[T-FunDefPure]{f \notin \Gamma \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash \emph{funbody}\ pure\ valid \andalso \Gamma, f : T_1 \to ... \to T_n, x1 : T_1, ... \vdash e_n : T_n}{\Gamma \vdash \texttt{def pure f : (x1 : T1) -> ... -> Tn => } \emph{funbody}\ end\ valid\ |\ \Gamma, f : T_1 \to ... \to T_n}

\infax[T-ClassBodyEmpty]{\Gamma \vdash \emph{empty memberdecls}\ valid\ |\ \Gamma}

\infrule[T-ClassBody]{\Gamma \vdash \emph{memberdecl}\ valid\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{memberdecls}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{memberdecl:memberdecls}\ valid\ |\ \Gamma_2}

\infrule[T-FieldDecl]{d \notin \Gamma}{\Gamma \vdash}

\infrule[T-MethodDecl]{f \notin \Gamma}{\Gamma \vdash}

\infax[T-FunBodyEmpty]{\Gamma \vdash \emph{empty statements}\ valid\ |\ \Gamma}

\infrule[T-FunBody]{\Gamma \vdash \emph{statement} : T\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{statements}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{statement:statements}\ valid\ |\ \Gamma_2}

\infrule[T-DeclStmt]{}{\Gamma \vdash : Unit}

\infax[T-ExprStmt]{\Gamma \vdash \emph{expr} : T}

\infrule[T-AssignStmt]{}{\Gamma \vdash : Unit}

\infrule[T-WhileStmt]{\Gamma \vdash \emph{expr} : Bool \andalso \Gamma \vdash \emph{statements}\ valid}{\Gamma \vdash while\ \emph{expr}\ do\ \emph{statements}\ end : Unit}

\infrule[T-WhenStmt]{\Gamma \vdash \emph{expr} : Bool \andalso \Gamma \vdash \emph{statements1}\ valid \andalso \Gamma \vdash \emph{statements2}\ valid}{\Gamma \vdash when\ \emph{expr}\ do\ \emph{statements1}\ otherwise\ \emph{statements2}\ end : Unit}

\infax[T-SubRefl]{\texttt{C <: C}}

\infrule[T-Inherit]{\texttt{class C < S => } \emph{classbody}\ end\ valid}{\texttt{C <: S}}

\infrule[T-SubTrans]{\texttt{C <: S} \andalso \texttt{S <: T}}{\texttt{C <: T}}

\infrule[T-Sub]{\Gamma \vdash e : S \andalso \texttt{S <: T}}{\Gamma \vdash e : T}

In these rules the order of definitions matters, but in implementation we do an additional pass to collect names before the checking.

Returning environment:

* When it's the same environment, it should not be modified
* When there are no environment, we just discard it

