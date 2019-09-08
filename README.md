# monad-suspend

Uses an underlying Coroutines implementation with a `spend` function which you can use to annotate how much "work" is being done. The caller can then specify to pause the computation when a certain cost threshold has been reached. You can use this to write a well-behaved scheduler which fairly runs several computations.

Also includes an **unlawful** monad which automatically ticks the cost up with every bind; it's not very principled but should allow computations to pause themselves after X number of "ops" whatever that means.
