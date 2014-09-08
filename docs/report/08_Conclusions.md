# Conclusions

> *Finally, we will look at the results of this work and outline ideas for the
> future work.*

## Results

* Common IR for modern programming languages
It is possible to represent language that belong to two major programming
paradigms.
* Expressive framework for optimisations

## Future work

* Low-level code generation
    + LLVM (reuse its low-level generic optimisations)
    + C
    + Native code
* Efficiency evaluation
    + Will be provided given code generator
    + Interpreter with reductions counter
* More parametricity in effects
    + Be able to be polymorphic in effects in the middle of the stack
    + Built-in functions example
* Explore different monads
    + Backtracking (Logic)
    + Par monad \cite{ParMonad}
* Laziness
    + Incorporate ideas from \cite{BridgingTheGulf}
* Modularity of the compiler for different effects (transformers, a la carte)

