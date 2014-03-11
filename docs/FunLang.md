% Functional Programming Language
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

Example program
===============

```haskell
type Bool = True | False

type Pair F S = Pair F S

type Tree A = Empty
            | Node A (Tree A) (Tree A)

not : Bool -> Bool
not True = False
not _    = True

id : forall A . A -> A
id = /\T . \x : T -> x

id2 : forall A . A -> A
id2 t x = x

idInt : Int -> Int
idInt = id [Int]

isZero : Int -> Bool
isZero n = case n of
  | 0 -> True
  | _ -> False

main : IO Unit
main = do
  i <- readInt;
  printInt i;

stateful : Int -> State Int Unit
stateful i = do
  put i;

doinput : Input Int
doinput = readInt

print : Int -> Output Unit
print = printInt

```

Syntax
======

-------------  -------  --------------------------------------------------  -------------------------------
    *program*   $\to$   *topdefs*                                           top-level definitions

    *topdefs*   $\to$   *topdef*

                  |     *topdef* *topdefs*

     *topdef*   $\to$   *typedef*

                  |     *fundef*

    *typedef*   $\to$   `type` *typename typevars* `=` *condefs*

   *typevars*   $\to$   $\varepsilon$

                  |     *typevar* *typevars*

    *condefs*   $\to$   *condef*

                  |     *condef* `|` *condefs*

     *condef*   $\to$   *conname* *confields*

  *confields*   $\to$   $\varepsilon$

                  |     *type* *confields*

     *fundef*   $\to$   *funtype* *funeqs*

    *funtype*   $\to$   *funname* `:` *type*

     *funeqs*   $\to$   *funeq*

                  |     *funeq* *funeqs*

      *funeq*   $\to$   *funname* *params* `=` *expr*

     *params*   $\to$   $\varepsilon$

                  |     *pattern* *params*

    *pattern*   $\to$   *name*

                  |     *literal*

                  |     *conname* *fieldpats*

                  |     `_`

  *fieldpats*   $\to$   $\varepsilon$

                  |     *fieldpat* *fieldpats*

   *fieldpat*   $\to$   *name*

                  |     *literal*

                  |     `_`

                  |     `(` *conname* *fieldpats* `)`

       *expr*   $\to$   *literal*

                  |     *name*

                  |     `\` *namebinders* `->` *expr*

                  |     *expr expr*

                  |     `/\` *typebinders* `.` *expr*

                  |     *expr* `[` *type* `]`

                  |     *conname*

                  |     `case` *expr* `of` *casealts*

   *casealts*   $\to$   `|` *casealt*

                  |     `|` *casealt* *casealts*

    *casealt*   $\to$   *pattern* `->` *expr*

*namebinders*   $\to$   *namebinder*

                  |     *namebinder* *namebinders*

 *namebinder*   $\to$   `(` *name* `:` *type* `)`

*typebinders*   $\to$   *typevar*

                  |     *typevar* *typebinders*

       *type*   $\to$   `Unit`

                  |     `Int`

                  |     *typename*

                  |     *typevar*

                  |     *type* `->` *type*

                  |     `forall` *typevar* `.` *type*

                  |     *type* *type*

                  |     `(` *type* `)`

   *typename*   $\to$   *upper* *alphanum*

    *typevar*   $\to$   *upper* *alphanum*

    *conname*   $\to$   *upper* *alphanum*

    *funname*   $\to$   *lower* *alphanum*

       *name*   $\to$   *lower* *alphanum*
-------------  -------  --------------------------------------------------  -------------------------------

Typing and Kinding
==================

---------  -------  -----------------------------------  -----------------------
 $\Gamma$   $\to$   $\varnothing$                        empty context

              |     $\Gamma$`,` *var* `:` *type*         variable binding

              |     $\Gamma$`,` *typevar*                type variable binding

              |     $\Gamma$`,` *typename* `::` *kind*   type operator binding

              |     $\Gamma$`,` *conname* `:` *type*     constructor binding
---------  -------  -----------------------------------  -----------------------

\infax[T-TypeDefsEmpty]{\Gamma \vdash \emph{empty typedefs}\ valid\ |\ \Gamma}

\infrule[T-TypeDefs]{\Gamma \vdash \emph{typedef}\ valid\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{typedefs}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{typedef:typedefs}\ valid\ |\ \Gamma_2}

\infrule[T-TypeDef]{T \notin \Gamma \andalso \Gamma, T :: \overbrace{* \Rightarrow ... \Rightarrow *}^{n+1}, X_1, ..., X_n \vdash \emph{condefs}\ valid\ in\ T\ X_1 ... X_n |\ \Gamma_1}{\Gamma \vdash type\ T\ X_1 ... X_n = \emph{condefs}\ valid\ |\ \Gamma_1 \setminus \{ X_1,...,X_n \}}

\infrule[T-ConDefs]{\Gamma \vdash \emph{condef}\ valid\ in\ T\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{condefs}\ valid\ in\ T\ |\ \Gamma_2}{\Gamma \vdash \emph{condef:condefs}\ valid\ in\ T\ |\ \Gamma_2}

\infrule[T-ConDef]{C \notin \Gamma \andalso C\ T_1 ... T_n \in T\ X_1 ... X_n \andalso \Gamma, C : forall\ X_1 .\ ...\ . forall\ X_n . T_1 \to ... \to T_n \to T\ X_1 ... X_n \vdash \emph{confields}\ valid\ |\ \Gamma_1}{\Gamma \vdash C\ T_1 ... T_n\ valid\ in\ T\ X_1 ... X_n |\ \Gamma_1}

\infax[T-ConFieldsEmpty]{\Gamma \vdash \emph{empty confields}\ valid\ |\ \Gamma}

\infrule[T-ConFields]{\Gamma \vdash \emph{confield}\ \texttt{well-kinded} \andalso \emph{confields}\ valid\ |\ \Gamma}{\Gamma \vdash \emph{confield:confields}\ valid\ |\ \Gamma}

\infax[T-FunDefsEmpty]{\Gamma \vdash \emph{empty fundefs}\ valid\ |\ \Gamma}

\infrule[T-FunDefs]{\Gamma \vdash \emph{fundef}\ valid\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{fundefs}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{fundef:fundefs}\ valid\ |\ \Gamma_2}

\infrule[T-FunDef]{\Gamma, f : T \vdash \emph{funeqs}\ valid\ |\ \Gamma_1}{\Gamma \vdash f : T\ \emph{funeqs}\ valid\ |\ \Gamma_1}

\infrule[T-FunEqNoParams]{f : T \in \Gamma \andalso \emph{expr} : T}{\Gamma \vdash f\ =\ \emph{expr}\ valid}

\infrule[T-FunEq]{f : T_1 \to ... \to T_n \to T \in \Gamma \andalso for\ each\ i\ \Gamma \vdash \emph{parami} : T_i \andalso \Gamma, \emph{param1} : T_1, ..., \emph{paramn} : T_n \vdash \emph{expr} : T}{\Gamma \vdash f\ \emph{param1 ... paramn}\ =\ \emph{expr}\ valid\ |\ \Gamma}

\infrule[T-FunEqs]{\Gamma \vdash \emph{funeq}\ valid\ |\ \Gamma \andalso \Gamma \vdash \emph{funeqs}\ valid\ |\ \Gamma \andalso consistent}{\Gamma \vdash \emph{funeq:funeqs}\ valid\ |\ \Gamma}

TODO: patterns

Returning environment:

* When it's the same environment, it should not be modified
* When there are no environment, we just discard it

