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
$return$. Type constructor $M$ has one type parameter.  In Haskell, the monad
concept is represented by a type class, which also has some additional
operations. Monad operations have the following types:

$$return :: a \to M\ a$$
$$bind :: M\ a \to (a \to M\ b) \to M\ b$$

{>> Don't start a sentence with a lowercase function name. One way to fix it is to add "The function" before the function name. <<}

$return$ takes a value of type $a$ and returns a monadic computation that just
returns that value and does not do anything else. $bind$ takes a monadic
computation that can produce a value of type $a$ and a function that can
consume that value and give a computation of type $M\ b$. So, $bind$ gives a
way to apply such a function to a computation of type $M\ a$ to get a
computation of type $M\ b$. Since, $bind$ is very often used as an infix
operator, it's type usually has the argument before the function.

Monad operations must satisfy the following three laws:

* *Left identity*:
$$bind\ (return\ x)\ f = f\ x$$

    It means that feeding the result of the return computation which just returns
    a value to function $f$ is the same as just applying function $f$ to that
    value.

* *Right identity*:
$$bind\ m\ return = m$$

    This law means that computing the result of $m$ and then returning that
    result is the same as just computing $m$.

* *Associativity*:
$$bind\ (bind\ m\ f)\ g = bind\ m\ (\lambda x \to bind\ (f\ x)\ g)$$

    Associativity law has a similar idea to associativity laws in arithmetic,
    namely that the order of parentheses does not matter \cite{WadlerMonadsForFP}.

{>> It's not only the associativity law that is similar to laws in arithmetic. The identity laws are similar to laws for multiplication with 1 or addition with zero. <<}

## Examples of monads

In this section we are going to look at examples of several monads.

* The simplest of all monads is the $Identity$ monad. It does not decorate
  computations with any effect or information. {== It is basically just a function
  application. ==}{>> I'm not sure I understand this sentece. It's true that `bind` is simply function application but your "It" seems to refer to the monad and the monad itself is not function application. The monad is just a pure value <<} We provide a definition of $Identity$ in Haskell below:

    ~~~{.haskell}
    newtype Identity a = Identity a

    instance Monad Identity where
      return = Identity
      (Identity x) >>= k = k x
    ~~~

    It might not seem very useful at first, but the $Identity$ monad helps to combine
    several monads by being a base on top of which others build up. It is also
    useful when a computation is abstracted over a monad and a user of the
    computation does not want to use any specific effects with this computation, so
    she can choose the $Identity$ monad as the monad instance in this case.

{>> I think you should talk about the notion of "running" a monad when you introduce the monads. You use the term further down in the text so it's important that you have defined it properly here. <<}

{>> Also, you need to introduce the operations of each monad here. Like "get" and "set" for the state monad. You refer to them in the section on monad transformers. <<}

* The next example is the $State$ monad for stateful computations. Since in most
  functional languages immutability is encouraged by default, the usual
  technique to deal with {== mutable state ==}{>> But the state monad doesn't really give us *mutable* state, does it? <<} is to pass the state around as function
  arguments. One of the possible $State$ monad implementations pretty much makes
  this technique implicit. Here is such an implementation in Haskell:

    ~~~{.haskell}
    newtype State s a = State (s -> (a, s))

    instance Monad (State s) where
      return x = State (\s -> (x, s))
      (State h) >>= f = State (\s -> let (a, s') = h s
                                         (State g) = f a
                                     in g s')
    ~~~

    One can think about a stateful computation as taking a state and producing
    a value and the new state. This is what the type definition above captures.
    $return$ produces a stateful computation that just gives the given value back
    without modifying the state. The $bind$ operation chains stateful computations
    together and passes the result and states between them.

* Another ubiquitous effect is error (exception) handling. There are several
  different monads that can be used. {== One of them is the $Either$ monad. ==}{>> When you write like this it makes me wonder: why did you pick this particular monad? How is preferable to the other choices? <<}

    ~~~{.haskell}
    data Either e a = Left e | Right a

    instance Monad (Either e) where
      return = Right
      Right x >>= f = f x
      Left e >>= f = Left e
    ~~~

    Data constructor `Left` represents an error (represented by a value of type
    `e`) that occured during the computation, while `Right` denotes a successful
    computation with a value of type `a` as the result. In the $Either$ monad
    $return$ is simply `Right`. The $bind$ operation passes the result of a
    successful computation to the next one, but in the case of an error, it stops
    and just propagates the error further.

* Very famous in the Haskell world $IO$ is also an example of a monad. We will
  skip a representation of the $IO$ monad in Haskell, but one can think about
  it as a $State$ with "real world" as a storage. Since $IO$ wraps arbitrary
  side-effects and interaction with the outside world, considering such a
  computation as the one that changes some very global state (of the world) is a
  rather useful metaphor.

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

Later, following and building on top of Moggi's work, Philip Wadler
incorporated monads as a way of structuring pure functional programs in
Haskell. First, monads were used to combine input/output and lazy evaluation,
and then they became an integral part of Haskell and are used for expressing
state manipulation, error handling, environment reading, collecting output,
nondeterminism, continuations and other \cite{WadlerComprMonads},
\cite{ImperativeFP}, \cite{WadlerMonadsForFP}. Monads were also successfully
used for building parsers \cite{MonadicParsing} and working with concurrency
using Software Transactional Memory \cite{STM}.

Nowadays, monads is a very important and popular programming pattern in
Haskell. They are used extensively for a wide variety of applications, for
example backtracking \cite{BacktrackingM} and parallelism \cite{ParMonad} among
others.

## Monad transformers

*Monad transformer* is a type constructor $T$ which takes a monad $M$ and
returns a monad, or in other words, if $M$ is a monad, so is $T\ M$. {>> Very good and crisp definition! <<}

Monad transformers are used to add new operations to a monad without changing
the computation in that monad, for example state manipulation can be added to
collecting ouput. They also compose easily, meaning that it is possible to
apply a monad transformer to another monad transformer. The resulting structure
is usually called *monad transformer stack*. There is one caveat, though,
namely, that effects produced by some of the monad transformer/monad
combinations depend on the order in which they are combined.

Monad transformer comes with {== $lift$ ==}{>> I think you should do something about the typography of "lift". It doesn't look very good in the text, the letters are too far apart. Whatevery you've done to make "return" look good, do the same to "lift". <<} operation, which embeds a computation in
monad $M$ into monad $T\ M$. It has the following type:

$$lift :: M\ a \to T\ M\ a$$

{>> "M" needs to be a monad in the type above. The "MonadTrans" instances you give below certainly require "M" to be a monad. <<}

The main intention for $lift$ is to be used to specify on which level a certain
monadic operation is performed.

Monad transformers must satisfy the following two laws:

* Lifting a monadic computation which only does $return$ results in the same
  monadic computation.
$$lift\ .\ return = return$$

* Lifting a sequence of monadic computations is the same as lifting them
  individually and then combining lifted computations in the new monad.
$$lift\ (bind\ m\ k) = bind\ (lift\ m)\ (lift\ .\ k)$$

Monad transformers and their application to building modular interpreters are
introduced and described in detail in \cite{MonadTransformers}.

To give a better understanding of monad transformers, we provide several
examples of them:

* $StateT\ s\ m$ is a transformer that adds an effect of a stateful computation
  to monad $m$. An implementation in Haskell could be the following:

    ~~~{.haskell}
    newtype StateT s m a = StateT (s -> m (a, s))
    ~~~

    Using the $StateT$ monad transformer, $State\ s$ monad can be expressed as
    $StateT\ s\ Identity$.

    Every monad transformer also has to be an instance of `Monad`, but we will
    skip these definitions here. They would be quite similar to the example
    definitions in the "Examples of monads" section above, but a bit more involved.

    To make $StateT$ a monad transformer in Haskell, we need to make it an
    instance of `MonadTrans`, which contains the $lift$ operation:

    ~~~{.haskell}
    instance MonadTrans (StateT s) where
      lift m = StateT (\s -> do
                 a <- m
                 return (a, s))
    ~~~

    The `do` block above uses the underlying monad's $bind$ and $return$ and just
    returns the given state together with the underlying computation's result.

* $ErrorT\ e\ m$ adds a notion of failure to monad $m$. One way to represent it
  is using the $Either$ data type:

    ~~~{.haskell}
    newtype ErrorT e m a = ErrorT (m (Either e a))

    instance MonadTrans (ErrorT e) where
      lift m = ErrorT (do
        a <- m
        return (Right a))
    ~~~

    To make the computation `m` to be an `ErrorT` computation we just run it in
    the underlying monad and then return the result as success using `Right`.

* $ReaderT\ r\ m$ adds a layer of interaction with a read-only environment of
  type $r$:

    ~~~{.haskell}
    newtype ReaderT r m a = ReaderT (r -> m a)
    ~~~

    By analogy with $State$ above, $Reader\ r$ monad is $ReaderT\ r\ Identity$.

    $lift$ implementation below returns a computation in $ReaderT$, which just
    disregards the given environment and gives back the computation in the
    underlying monad:

    ~~~{.haskell}
    instance MonadTrans (ReaderT r) where
      lift m = ReaderT (\r -> m)
    ~~~

One of the disadvantages of monad transformers for practical programming is the
problem of lifting. When one has a stack of several monad transformers and
wants to use an operation that is defined with the type containing only one of
them, like `get` or `put` for `StateT`, for example, it is not possible to just
use them directly (unless `StateT` is on top) because the types do not match.
In this case lifting needs to be used, to add necessary layers on top of a
computation in one of the monads deeper down in the stack. There are attempts
to solve the lifting problem, for example by using type classes to
abstract different types of computations and use these abstractions instead of
specific monad transformers. This idea is based on \cite{Overloading} and
incorporated in Haskell packages like `mtl`. It is rather convenient for a
user, but has quite an overhead for a library writer, because the number of
instances that needs to be provided is quadratic to the number of monads
\cite{MonadTransformers}. Another, more recent idea, is presented in
\cite{ModularMT}, where "a uniform lifting through any monad transformer" is
defined. {>> Algebraic effects need to be mentioned here as well <<}

As it was mentioned earlier, the order in which different monads are combined
can be significant. If it is the case, then it is said that these two effects
do not *commute*. Probably, the most famous example is the ordering of $State$
and $Error$. One can see this by "unrolling" the types:

* `StateT s (ErrorT e Identity) a` $\Rightarrow$ `s -> ErrorT e Identity (a, s)` $\Rightarrow$\
  $\Rightarrow$ `s -> Identity (Either e (a, s))` $\Rightarrow$ `s -> Either e (a, s)`

* `ErrorT e (StateT s Identity) a` $\Rightarrow$ `StateT s Identity (Either e a)` $\Rightarrow$\
  $\Rightarrow$ `s -> Identity (Either e a, s)` $\Rightarrow$ `s -> (Either e a, s)`

In the first case, when $StateT$ is on top of $ErrorT$ the semantics of a
computation is that it either fails and returns an error or it succeeds and
returns a value and a new state. One can think of this as a kind of rollback.
On the other hand, in the second case, when the order is reversed, the
computation always returns a new state, regardless of whether it gives an error
or a result.

