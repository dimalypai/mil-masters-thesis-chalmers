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

isZero : Int -> Bool
isZero n = case n of
  | 0 -> True;
  | _ -> False;

main : IO Unit
main = do
  i <- readInt;
  printInt i;

stateful : Int -> State Unit
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

    *topdefs*   $\to$   $\varepsilon$

                  |     *topdef* *topdefs*

     *topdef*   $\to$   *typedef*

                  |     *fundef*

    *typedef*   $\to$   `type` *typename typebinds* `=` *condefs*

  *typebinds*   $\to$   $\varepsilon$

                  |     *typevar* *typebinds*

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

                  |     `/\` *typevar* `.` *expr*

                  |     *expr* `[` *type* `]`

*namebinders*   $\to$   *namebinder*

                  |     *namebinder* *namebinders*

 *namebinder*   $\to$   `(` *name* `:` *type* `)`

       *type*   $\to$   `Unit`

                  |     `Int`

                  |     *typename*

                  |     *type* *type*

   *typename*   $\to$   *upper* *alphanum*

    *typevar*   $\to$   *upper* *alphanum*

    *conname*   $\to$   *upper* *alphanum*

    *funname*   $\to$   *lower* *alphanum*

       *name*   $\to$   *lower* *alphanum*
-------------  -------  --------------------------------------------------  -------------------------------

Typing
======

---------  -------  -----------------------------------  -----------------------
 $\Gamma$   $\to$   $\varnothing$                        empty context

                    $\Gamma$`,` *var* `:` *type*         variable binding

                    $\Gamma$`,` *typevar*                type variable binding
---------  -------  -----------------------------------  -----------------------

