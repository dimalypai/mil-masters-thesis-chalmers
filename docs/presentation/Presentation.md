% Monadic Intermediate Language for Modular and Generic Compilers
% Dmytro Lypai
  Josef Svenningsson (supervisor)
% March 22, 2016

## Introduction

### Intermediate representations

- Important piece of compiler construction
- Examples:
    * Core, STG, C-\- (Cmm) in GHC
    * RTL in GCC
    * LLVM IR (SSA-based)
- Different design considerations

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

## Examples of monads

- Identity
- State
- Either
- IO

---

## Monads and programming languages

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

---

## Examples of monad transformers

- StateT
- ErrorT
- ReaderT

###

### Non-commutative effects

* `StateT s (ErrorT e Identity) a` $\Rightarrow$ `s -> ErrorT e Identity (a, s)` $\Rightarrow$\
  $\Rightarrow$ `s -> Identity (Either e (a, s))` $\Rightarrow$ `s -> Either e (a, s)`

* `ErrorT e (StateT s Identity) a` $\Rightarrow$ `StateT s Identity (Either e a)` $\Rightarrow$\
  $\Rightarrow$ `s -> Identity (Either e a, s)` $\Rightarrow$ `s -> (Either e a, s)`

---

## Related work

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
- The GRIN project (Boquist and Johnsson)

## Related work (cont.)

- Monad transformers and modular effects
- Koka programming language
- Polymonads
- Algebraic effects and effect handlers:
    * Eff
    * Idris
    * Extensible effects

## MIL

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

\infrule{isMonad(M_1) \andalso isMonad(M_2) \andalso isCompatibleMonadNotCommut(M_1, M_2)}{isCompatible(M_1, M_2)}
\infrule{isMonad(M_1) \andalso isMonad(M_2) \andalso isCompatible(M_1, M_2) \andalso isCompatible(T_1, T_2)}{isCompatible(M_1\ T_1, M_2\ T_2)}
\infrule{isCompatible(T_{21}, T_{11}) \andalso isCompatible(T_{12}, T_{22})}{isCompatible(T_{11} \to T_{12}, T_{21} \to T_{22})}
\infrule{isCompatible(T_1, [Y \mapsto X]T_2)}{isCompatible(forall\ X\ .\ T_1, forall\ Y\ .\ T_2)}
\infrule{T_1 <: T_2}{isCompatible(T_1, T_2)}

## MIL Type system highlights (cont.)

### Relation to Haskell:

- `Error Int ::: (State ::: IO)` $\Rightarrow$ `ErrorT Int (StateT () IO)`

### Examples for $isCompatibleMonadNotCommut$

- $State ::: Error\ Unit$ is compatible with $State ::: (Error\ Unit ::: IO)$
- $State ::: Error\ Int$ is not compatible with $Error\ Int ::: State$
- $State ::: IO$ is not compatible with just $State$

### Example for $isCompatible$

- a function of type $(State ::: IO)\ Int \to State\ Int$ can be passed as an
  argument to a function which has a parameter of type $State\ Int \to (State
  ::: IO)\ Int$

### Example for $isMonadSuffix$

- $State ::: IO$ and $Error\ Unit ::: (State ::: IO)$

## FunLang

- Functional programming language
- Inspired by Haskell
- System F
- Pure and strict
- Algebraic data types
- Built-in monads (do notation)
- Simple exception handling

# FunLang Examples

---

## FunLang Code generation

### Monad transformer stacks

- Pure and State: `State ::: Error Unit`
- IO: `State ::: (Error Unit ::: IO)`

###

### Demo

## FunLang Conclusions

- Straight-forward code generation (in general)
- Would benefit from effect inference/elimination
- Can be quite easily extended with more features
- Exception handling and MIL limitations:
    * `catch_error` type: `forall E . forall A . Error E A -> (E -> Error E A) -> Error E A`
    * `catch_error_1` and `catch_error_2` instead

---

## OOLang

- Object-oriented programming language
- Inspired by C#, Haskell and a bit of Ruby
- Immutability
- Controlled impurity
- Classes with inheritance

# OOLang Examples

---

## OOLang Code generation

### Monad transformer stacks

- Pure: `Error Unit`
- Impure: `Error Unit ::: (State ::: IO)`

###

### Demo

## OOLang Conclusions

- Non-termination problem:
    * `Error Unit ::: NonTerm => Maybe (Either e a)`
    * `Error Unit ::: (NonTerm ::: (State ::: IO)) => s -> IO (Maybe (Either e a), s)`
    * `Error Unit ::: (State ::: (NonTerm ::: IO)) => s -> IO (Maybe (Either e a, s))`
    * `State ::: (Error Unit ::: (NonTerm ::: IO))`
    * `Error Unit ::: (m ::: NonTerm)`
- MIL is quite a good fit for modern OO-languages
- Influence to introduce tuples with subtyping in MIL

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

# Optimisations

[comment1]: # - optimisation pipelines

[comment2]: # - when elimination

[comment3]: # - constant folding

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

