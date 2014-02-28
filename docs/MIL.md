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

                  |     `{` *tupleelems* `}`                                tuple

                  |     *expr* `.` *i*                                      projection

                  |     `new` *expr*                                        reference creation

                  |     `!`*expr*                                           dereferencing

                  |     *expr* `:=` *expr*                                  assignment

                  |     `let` *var* `:` *type* `<-` *expr* `in` *expr*      name binding

                  |     `return` *expr*                                     monadic return

                  |     `let rec` *var* `:` *type* `=` *expr* `in` *expr*   recursive binding

                  |     `case` *expr* `of` `{` *casealts* `}`

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

        *var*   $\to$   *lower* *alphanum*

    *typevar*   $\to$   *upper* *alphanum*

   *typename*   $\to$   *upper* *alphanum*

    *conname*   $\to$   *upper* *alphanum*

    *funname*   $\to$   *lower* *alphanum*
-------------  -------  --------------------------------------------------  -------------------------------

Typing
======

---------  -------  -----------------------------------  -----------------------
 $\Gamma$   $\to$   $\varnothing$                        empty context

                    $\Gamma$`,` *var* `:` *type*         variable binding

                    $\Gamma$`,` *typevar*                type variable binding
---------  -------  -----------------------------------  -----------------------

\infax[T-Unit]{\Gamma \vdash unit : Unit}

\infax[T-Int]{\Gamma \vdash intlit : Int}

\infax[T-Float]{\Gamma \vdash floatlit : Float}

\infax[T-Char]{\Gamma \vdash charlit : Char}

\infrule[T-Var]{x : T \in \Gamma}{\Gamma \vdash x : T}

\infrule[T-Abs]{\Gamma, x : T_1 \vdash e_2 : T_2}{\Gamma \vdash \lambda x : T_1 \to e_2 : T_1 \to T_2}

\infrule[T-App]{\Gamma \vdash e_1 : T_{11} \to T_{12} \andalso \Gamma \vdash e_2 : T_{11}}{\Gamma \vdash e_1\ e_2 : T_{12}}

\infrule[T-TAbs]{\Gamma, X \vdash e_2 : T_2}{\Gamma \vdash \Lambda X\ .\ e_2 : forall\ X\ .\ T_2}

\infrule[T-TApp]{\Gamma \vdash e_1 : forall\ X\ .\ T_{12}}{\Gamma \vdash e_1\ [T_2] : [X \mapsto T_2]T_{12}}

\infrule[T-TypeDef]{}{}

\infrule[T-Tuple]{for\ each\ i \andalso \Gamma \vdash e_i : T_i}{\Gamma \vdash \{ e_{i = 1..n} \} : \{ T_{i = 1..n} \}}

\infrule[T-Proj]{\Gamma \vdash e : \{ T_{i = 1..n}\}}{\Gamma \vdash e.j : T_j}

\infrule[T-Ref]{\Gamma \vdash e : T}{\Gamma \vdash new\ e : State\ (Ref\ T)}

\infrule[T-Deref]{\Gamma \vdash e : Ref\ T}{\Gamma \vdash\ !e : State\ T}

\infrule[T-Assign]{\Gamma \vdash e_1 : Ref\ T \andalso \Gamma \vdash e_2 : T}{\Gamma \vdash e_1 := e_2 : State\ Unit}

\infrule[T-Let]{\Gamma \vdash e_1 : M_1\ T_1 \andalso \Gamma, x : T_1 \vdash e_2 : M_2\ T_2}{\Gamma \vdash let\ x : T_1 \gets e_1\ in\ e_2 : M_2\ T_2}

\infrule[T-Return]{\Gamma \vdash e : T}{\Gamma \vdash return\ e : M\ T}

\infrule[T-LetRec]{\Gamma \vdash e_1 : M_1\ T_1 \andalso \Gamma, x : T_1 \vdash e_2 : M_2\ T_2}{\Gamma \vdash let\ rec\ x : T_1 \gets e_1\ in\ e_2 : M_2\ T_2}

\infrule[T-Case]{}{\Gamma \vdash case\ e\ of\ ...}

