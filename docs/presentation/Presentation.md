% Monadic Intermediate Language for Modular and Generic Compilers
% Dmytro Lypai
  Josef Svenningsson (supervisor)
% March 22, 2016

## Introduction

### Intermediate representations

TODO

- Important piece of compiler construction
- Examples:
    * Core, STG, C-\- (Cmm) in GHC
    * RTL in GCC
    * LLVM IR (SSA-based)

---

## Introduction

### Goals

- IR for modern programming languages
- Computational effects recorded in programs
- Expressing different semantics
- Transformations in the presence of effects

# Monads, semantics and functional programming

## Monads

*Monad* is a triple of a type constructor $M$ and two operations: $bind$ and
$return$.

$$return :: a \to M\ a$$
$$bind :: M\ a \to (a \to M\ b) \to M\ b$$

---

## Monad laws

* Left identity:
$$bind\ (return\ x)\ f = f\ x$$

* Right identity:
$$bind\ m\ return = m$$

* Associativity:
$$bind\ (bind\ m\ f)\ g = bind\ m\ (\lambda x \to bind\ (f\ x)\ g)$$

---

## Monads and programming languages

TODO

- Originate from Category Theory
- Applied to structure denotational semantics of programming languages by Moggi
- Wadler introduced monads into Haskell
- Important and popular programming pattern

---

## Monad transformers

*Monad transformer* is a type constructor $T$ which takes a monad $M$ and
returns a monad, or in other words, if $M$ is a monad, so is $T\ M$.

$$lift :: M\ a \to T\ M\ a$$

Laws:

$$lift\ .\ return = return$$
$$lift\ (bind\ m\ k) = bind\ (lift\ m)\ (lift\ .\ k)$$

## Related work

TODO

- Common IL for ML and Haskell (Peyton Jones et al.)
    * Focus on evaluation strategy
    * Two different designs: Lift/ST and just ST monad
- Optimizing ML using a hierarchy of monadic types (Tolmach)
    * Fixed hierarchy: $ST > EXN > LIFT > ID$
    * Monad inference
    * Transformations
- MIL-lite (Benton and Kennedy)
    * MLj compiler
    * Semantics of effects
    * Fine-grained effects: non-termination, allocation, reading, writing,
      raising an exception
    * Transformations

## MIL

TODO

- Monadic Intermediate Language
- System F
- Algebraic data types
- Built-in monads/monad transformers
- Tuples with subtyping
- Pattern matching

# MIL Examples

---

## MIL Type system highlights

\infrule{\Gamma \vdash e_1 : T_1 \to T_2 \andalso \Gamma \vdash e_2 : T_1' \andalso isCompatible(T_1', T_1)}{\Gamma \vdash e_1\ e_2 : T_2}

\infrule{x \notin \Gamma \andalso \Gamma \vdash e_1 : M_1\ T_1' \andalso \Gamma, x : T_1 \vdash e_2 : M_2\ T_2 \andalso T_1 \equiv_\alpha T_1' \\ isMonad(M_1) \andalso isMonad(M_2) \andalso isCompatibleMonad(M_2, M_1)}{\Gamma \vdash let\ (x : T_1) \gets e_1\ in\ e_2 : highestEffectMonad(M_1, M_2)\ T_2}

\infrule{isMonad(M) \andalso \Gamma \vdash e : T}{\Gamma \vdash return\ [M]\ e : M\ T}

\infrule{\Gamma \vdash e : M_1'\ T \andalso isMonad(M_1') \andalso isMonad(M_1) \andalso isMonad(M_2) \\ isCompatibleMonadNotCommut(M_1', M_1) \andalso isMonadSuffix(M_1, M_2)}{\Gamma \vdash lift\ [M_1 \Rightarrow M_2]\ e : M_2\ T}

## MIL Type system highlights (cont.)

\infax{isSingleMonad(Id)}
\infax{isSingleMonad(State)}
\infax{isSingleMonad(IO)}
\infax{isSingleMonad(Error\ T)}
\infrule{isSingleMonad(M)}{isMonad(M)}
\infrule{isSingleMonad(M_1) \andalso isMonad(M_2)}{isMonad(M_1 ::: M_2)}

## MIL Type system highlights (cont.)

\infrule{isMonad(M_1) \andalso isMonad(M_2) \andalso isCompatibleMonadNotCommut(M_1, M_2)}{isCompatible(M_1, M_2)}
\infrule{isMonad(M_1) \andalso isMonad(M_2) \andalso isCompatible(M_1, M_2) \andalso isCompatible(T_1, T_2)}{isCompatible(M_1\ T_1, M_2\ T_2)}
\infrule{isCompatible(T_{21}, T_{11}) \andalso isCompatible(T_{12}, T_{22})}{isCompatible(T_{11} \to T_{12}, T_{21} \to T_{22})}
\infrule{isCompatible(T_1, [Y \mapsto X]T_2)}{isCompatible(forall\ X\ .\ T_1, forall\ Y\ .\ T_2)}
\infrule{T_1 <: T_2}{isCompatible(T_1, T_2)}

## MIL Type system highlights (cont.)

\infrule{isSingleMonad(M_1) \andalso isSingleMonad(M_2) \andalso M_1 \equiv_\alpha M_2}{isCompatibleMonadNotCommut(M_1, M_2)}
\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \andalso monadConsLeft(M_1) \equiv_\alpha monadConsLeft(M_2) \\ isCompatibleMonadNotCommut(monadConsRight(M_1), monadConsRight(M_2))}{isCompatibleMonadNotCommut(M_1, M_2)}
\infrule{isSingleMonad(M_1) \andalso isMonadCons(M_2) \andalso M_1 \equiv_\alpha monadConsLeft(M_2)}{isCompatibleMonadNotCommut(M_1, M_2)}
\infrule{isCompatibleMonadNotCommut(M_1, M_2)}{isCompatibleMonad(M_1, M_2)}
\infrule{isCompatibleMonadNotCommut(M_2, M_1)}{isCompatibleMonad(M_1, M_2)}

## MIL Type system highlights (cont.)

\infrule{isSingleMonad(M_1) \andalso isSingleMonad(M_2) \andalso M_1 \equiv_\alpha M_2}{isMonadSuffix(M_1, M_2)}
\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \andalso M_1 \equiv_\alpha M_2}{isMonadSuffix(M_1, M_2)}
\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \andalso isMonadSuffix(M_1, monadConsRight(M_2))}{isMonadSuffix(M_1, M_2)}
\infrule{isSingleMonad(M_1) \andalso isMonadCons(M_2) \andalso isMonadSuffix(M_1, monadConsRight(M_2))}{isMonadSuffix(M_1, M_2)}

## MIL Type system highlights (cont.)

\infrule{isMonadCons(M_1) \andalso isSingleMonad(M_2)}{highestEffectMonad(M_1, M_2) = M_1}
\infrule{isSingleMonad(M_1) \andalso isMonadCons(M_2)}{highestEffectMonad(M_1, M_2) = M_2}
\infrule{isSingleMonad(M_1) \andalso isSingleMonad(M_2)}{highestEffectMonad(M_1, M_2) = M_1}
\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \\ highestEffectMonad(monadConsRight(M_1), monadConsRight(M_2)) = monadConsRight(M_1)}{highestEffectMonad(M_1, M_2) = M_1}
\infrule{isMonadCons(M_1) \andalso isMonadCons(M_2) \\ highestEffectMonad(monadConsRight(M_1), monadConsRight(M_2)) = monadConsRight(M_2)}{highestEffectMonad(M_1, M_2) = M_2}

## FunLang

- Functional programming language
- Inspired by Haskell
- System F
- Pure and strict
- Algebraic data types
- Built-in monads (do notation)
- Simple exception handling

# FunLang Examples

# FunLang code generation TODO

---

## OOLang

- Object-oriented programming language
- Inspired by C#, Haskell and a bit of Ruby
- Immutability
- Controlled impurity

# OOLang Examples

# OOLang code generation TODO

---

## Optimisations

- Monad laws
- Lift identity
- Lift composition
- Id exchange
- Id elimination
- State exchange new
- State exchange read
- State use read
- State use write
- Eliminate throw-catch
- Constant case elimination
- Common bind extraction
- Constant folding

# Optimisations TODO

# Conclusions

## Results

- IR with explicit effects
- Modular semantics via monad transformers
- Two source languages as an evaluation
- Clear, concise and reusable effect-aware transformations (with Uniplate)
- `dmytrolypai/mil-masters-thesis-chalmers` at BitBucket and GitHub (soon)

## Future work

- Code generation (LLVM)
- Interpreter (with metrics)
- More expressive effect system
- Effect inference/elimination
- More monads (Backtracking, Par)
- Laziness
- Modularity (a la carte, transformers)

# Thank you

