# Monads, semantics and functional programming

> *This chapter introduces monads -- the central concept of this thesis.
> Applications of monads in semantics and functional programming are described.
> At the end, we talk about a way to combine different monads -- monad
> transformers. More details in relation to this work is left for one of the
> following chapters.*

## Monads introduction

Monad is quite an overloaded word. It is used in philosophy, linear algebra,
music, biology and probably some other areas. Monads related to this work is a
concept, which comes from category theory. But here it is more useful to use a
definition adopted in functional programming.

*Monad* is a triple of a type constructor $M$ and two operations: $bind$ and
$return$ (also known as $unit$). Type constructor $M$ has one type parameter.
In Haskell, monad concept is represented by a type class, which also has some
additional operations. Monad operations have the following types:

$$return :: a \to M\ a$$
$$bind :: M\ a \to (a \to M\ b) \to M\ b$$

$return$ takes a value of type $a$ and returns a monadic computation that just
returns that value and does not do anything else. $bind$ takes a monadic
computation that can produce a value of type $a$ and a function that can
consume that value and give a computation of type $M\ b$. So, $bind$ gives a
way to apply such a function to a computation of type $M\ a$ to get a
computation of type $M\ b$. Since, $bind$ is very often used as an infix
operator, it's type usually has argument before the function.

Monad operations must satisfy the following three laws:

* *Left identity (unit)*:
$$bind\ (return\ x)\ f = f\ x$$

    It means that feeding the result of the unit computation which just returns
    a value to function $f$ is the same as just applying function $f$ to that
    value.

* *Right identity (unit)*:
$$bind\ m\ return = m$$

    This law means that computing the result of $m$ and then returning that
    result is the same as just computing $m$.

* *Associativity*:
$$bind\ (bind\ m\ f)\ g = bind\ m\ (\lambda x \to bind\ (f\ x)\ g)$$

    Associativity law has a similar idea to associativity laws in arithmetic,
    namely that the order of parentheses does not matter \cite{WadlerMonadsForFP}.

## Monads in semantics

One of the first applications of monads to programming languages was done by
Eugenio Moggi. Moggi applied monads to structure the denotational semantics of
programming languages \cite{MoggiNotions}. He was studying the notions of
computations such as partiality, nondeterminism, side-effects, exceptions,
continuations, interactive input and interactive output and proving equivalence
of programs with those effects using a calculi based on a categorical
semantics. There is also a "modular approach" to denotational semantics
proposed in \cite{MoggiAbstractView}.

## Monads in Haskell

Later, following and building on top of Moggi's work, a group of researchers
related to the Haskell programming language incorporated monads as a way of
structuring pure functional programs. First, monads were used to combine
input/output and lazy evaluation, and then they became an integral part of
Haskell and are used for expressing state manipulation, error handling,
environment reading, collecting output, nondeterminism, continuations and other
\cite{WadlerComprMonads}, \cite{ImperativeFP}, \cite{WadlerMonadsForFP}. Monads
were also successfully used for building parsers \cite{MonadicParsing} and
working with concurrency using Software Transactional Memory \cite{STM}.

Nowadays, monads is a very important and popular programming pattern in
Haskell. They are used extensively for a wide variety of applications, for
example backtracking \cite{BacktrackingM} and parallelism \cite{ParMonad} among
others.

## Monad transformers

*Monad transformer* is a type constructor $T$ which takes a monad $M$ and
returns a monad, or in other words, if $M$ is a monad, so is $T\ M$.

Monad transformers are used to add new operations to a monad without changing
the computation in that monad, for example state manipulation can be added to
collecting ouput. They also compose easily, meaning that it is possible to
apply a monad transformer to another monad transformer. The resulting structure
is usually called *monad transformer stack*. There is one caveat, though,
namely, that some of the monad transformer/monad combination effects depend on
the order in which they are combined.

Monad transformer comes with $lift$ operation, which embeds a computation in
monad $M$ into monad $T\ M$. It has the following type:

$$lift :: M\ a \to T\ M\ a$$

The main intention for $lift$ is to be used to specify on which level a certain
monadic operation is performed.

Monad transformers must satisfy the following two laws:

* Lifting a unit monadic computation results in a unit monadic computation.
$$lift\ .\ return = return$$

* Lifting a sequence of monadic computations is the same as lifting them
  individually and then combining lifted computation in the new monad.
$$lift\ (bind\ m\ k) = bind\ (lift\ m)\ (lift\ .\ k)$$

All of this is described in greater detail in \cite{MonadTransformers}.

