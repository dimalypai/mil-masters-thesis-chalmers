% Monadic Intermediate Language
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

Syntax
======

-------------  -------  --------------------------------------------------  -------------------------------
    *program*   $\to$   *typedefs fundefs*                                  top-level definitions

   *typedefs*   $\to$   $\varepsilon$                                       type definitions

                  |     *typedef* `;` *typedefs*

    *fundefs*   $\to$   $\varepsilon$                                       function definitions

                  |     *fundef* `;` *fundefs*

    *typedef*   $\to$   `type` *typename typevars* `=` *condefs*            type definition

     *fundef*   $\to$   *funname* `:` *type* `=` *expr*                     function definition

   *typevars*   $\to$   $\varepsilon$                                       type variables

                  |     *typevar* *typevars*

    *condefs*   $\to$   *condef* `;`                                        data constructor definitions

                  |     *condef* `;` *condefs*

     *condef*   $\to$   *conname* *confields*                               data constructor definition

  *confields*   $\to$   $\varepsilon$

                  |     *type* *confields*

       *expr*   $\to$   *literal*

                  |     *var*                                               variable

                  |     `\` *var* `:` *type* `->` *expr*                    abstraction

                  |     *expr expr*                                         application

                  |     `/\` *typevar* `.` *expr*                           type abstraction

                  |     *expr* `[` *type* `]`                               type application

                  |     *conname*                                           data constructor

                  |     `{` *tupleelems* `}`                                tuple

                  |     *expr* `.` *i*                                      projection

                  |     `new` *expr*                                        reference creation

                  |     `!`*expr*                                           dereferencing

                  |     *expr* `:=` *expr*                                  assignment

                  |     `let` *var* `:` *type* `<-` *expr* `in` *expr*      name binding

                  |     `return` *expr*                                     monadic return

                  |     `let rec` *var* `:` *type* `=` *expr* `in` *expr*   recursive binding

                  |     `case` *expr* `of` `{` *casealts* `}`               case expression

   *casealts*   $\to$   *casealt*

                  |     *casealt* `;` *casealts*

    *casealt*   $\to$   *casebind* `->` *expr*

   *casebind*   $\to$   *varbind*                                           variable binder

                  |     *conname* *varbinds*                                data constructor binder

                  |     *literal*                                           literal case

                  |     `_`                                                 default binder

   *varbinds*   $\to$   $\varepsilon$                                       variable binders

                  |     *varbind* *varbinds*

    *varbind*   $\to$   `(` *var* `:` *type* `)`                            variable binder

    *literal*   $\to$   `unit`

                  |     *intlit*

                  |     *floatlit*

                  |     *charlit*

 *tupleelems*   $\to$   *expr*

                  |     *expr* `,` *tupleelems*

       *type*   $\to$   *Monad* *valuetype*                                 computation type

                  |     *valuetype*

  *valuetype*   $\to$   `Unit`

                  |     `Int`

                  |     `Float`

                  |     `Char`

                  |     *typename*                                          type introduced with `type`

                  |     *typevar*                                           type variable

                  |     *type* `->` *type*                                  function type

                  |     `forall` *typevar* `.` *type*                       universal type

                  |     *type type*                                         type operator application

                  |     `{` *tupletypes* `}`                                tuple type

 *tupletypes*   $\to$   *type*

                  |     *type* `,` *tupletypes*

       *kind*   $\to$   `*`

                  |     `*` `=>` *kind*

        *var*   $\to$   *lower* *alphanum*

    *typevar*   $\to$   *upper* *alphanum*

   *typename*   $\to$   *upper* *alphanum*

    *conname*   $\to$   *upper* *alphanum*

    *funname*   $\to$   *lower* *alphanum*
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

\infax[T-FunDefsEmpty]{\Gamma \vdash \emph{empty fundefs}\ valid\ |\ \Gamma}

\infrule[T-FunDefs]{\Gamma \vdash \emph{fundef}\ valid\ |\ \Gamma_1 \andalso \Gamma_1 \vdash \emph{fundefs}\ valid\ |\ \Gamma_2}{\Gamma \vdash \emph{fundef:fundefs}\ valid\ |\ \Gamma_2}

\infax[T-Unit]{\Gamma \vdash unit : Unit}

\infax[T-Int]{\Gamma \vdash \emph{intlit} : Int}

\infax[T-Float]{\Gamma \vdash \emph{floatlit} : Float}

\infax[T-Char]{\Gamma \vdash \emph{charlit} : Char}

\infrule[T-Var]{x : T \in \Gamma}{\Gamma \vdash x : T}

\infrule[T-Abs]{\Gamma, x : T_1 \vdash e_2 : T_2}{\Gamma \vdash \lambda x : T_1 \to e_2 : T_1 \to T_2}

\infrule[T-App]{\Gamma \vdash e_1 : T_{11} \to T_{12} \andalso \Gamma \vdash e_2 : T_{11}}{\Gamma \vdash e_1\ e_2 : T_{12}}

\infrule[T-TAbs]{\Gamma, X \vdash e_2 : T_2}{\Gamma \vdash \Lambda X\ .\ e_2 : forall\ X\ .\ T_2}

\infrule[T-TApp]{\Gamma \vdash e_1 : forall\ X\ .\ T_{12}}{\Gamma \vdash e_1\ [T_2] : [X \mapsto T_2]T_{12}}

\infrule[T-ConstrNil]{\Gamma \vdash C \in T}{\Gamma \vdash C : T}

\infrule[T-Constr]{\Gamma \vdash C\ T_1 ... T_n \in T}{\Gamma \vdash C : T_1 \to ... \to T_n \to T}

\infrule[T-ConstrNilTypeVars]{\Gamma \vdash C \in T\ X_1 ... X_n}{\Gamma \vdash C : T\ X_1 ... X_n}

\infrule[T-ConstrTypeVars]{\Gamma \vdash C\ T_1 ... T_n \in T\ X_1 ... X_n}{\Gamma \vdash C : forall\ X_1 .\ ...\ . forall\ X_n . T_1 \to ... \to T_n \to T\ X_1 ... X_n}

\infrule[T-Tuple]{for\ each\ i \andalso \Gamma \vdash e_i : T_i}{\Gamma \vdash \{ e_{i = 1..n} \} : \{ T_{i = 1..n} \}}

\infrule[T-Proj]{\Gamma \vdash e : \{ T_{i = 1..n}\}}{\Gamma \vdash e.j : T_j}

\infrule[T-Ref]{\Gamma \vdash e : T}{\Gamma \vdash new\ e : State\ (Ref\ T)}

\infrule[T-Deref]{\Gamma \vdash e : Ref\ T}{\Gamma \vdash\ !e : State\ T}

\infrule[T-Assign]{\Gamma \vdash e_1 : Ref\ T \andalso \Gamma \vdash e_2 : T}{\Gamma \vdash e_1 := e_2 : State\ Unit}

\infrule[T-Let]{\Gamma \vdash e_1 : M_1\ T_1 \andalso \Gamma, x : T_1 \vdash e_2 : M_2\ T_2}{\Gamma \vdash let\ x : T_1 \gets e_1\ in\ e_2 : M_2\ T_2}

\infrule[T-Return]{\Gamma \vdash e : T}{\Gamma \vdash return\ e : M\ T}

\infrule[T-LetRec]{\Gamma \vdash e_1 : T_1 \andalso \Gamma, x : T_1 \vdash e_2 : T_2}{\Gamma \vdash let\ rec\ x : T_1 = e_1\ in\ e_2 : T_2}

\infrule[T-Case]{}{\Gamma \vdash case\ e\ of\ ...}

\infax[K-Unit]{\Gamma \vdash Unit :: *}

\infax[K-Int]{\Gamma \vdash Int :: *}

\infax[K-Float]{\Gamma \vdash Float :: *}

\infax[K-Char]{\Gamma \vdash Char :: *}

\infrule[K-TVar]{X \in \Gamma}{\Gamma \vdash X :: *}

\infrule[K-Typename]{T :: K \in \Gamma}{\Gamma \vdash T :: K}

\infax[K-TypeDefNil]{\Gamma \vdash T :: *}

\infrule[K-TypeDef]{\Gamma \vdash T\ X_1 ... X_n}{\Gamma \vdash T :: * \Rightarrow ... \Rightarrow *}

Returning environment:

* When it's the same environment, it should not be modified
* When there are no environment, we just discard it

