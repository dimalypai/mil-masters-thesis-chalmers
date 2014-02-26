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

    *typedef*   $\to$   `type` *typename typebinds* `=` ...

     *fundef*   $\to$   *funtype* *funeqs*

    *funtype*   $\to$   *funname* `:` *type*

     *funeqs*   $\to$   *funeq*

                  |     *funeq* *funeqs*

      *funeq*   $\to$   *funname* *params* `=` *expr*

     *params*   $\to$   $\varepsilon$

                  |     *param* *params*

      *param*   $\to$   *var*

                  |     *literal*

                  |     `_`

   *typename*   $\to$   *upper* *alphanum*

    *funname*   $\to$   *lower* *alphanum*
-------------  -------  --------------------------------------------------  -------------------------------

Typing
======

