haskell-club
============

The first Rule of Haskell Club is.....

Read http://learnyouahaskell.com/

We're now up to half way through chapter 11.  

Next session we'll continue to practice with Applicative Functors.  We should understand:
 * the `<*>` implementation for `Maybe` `[]`, `((->) a)` and `IO` 
 * the Applicative Laws
 * why `<$>` exists

Ahead of the session, let's try to do the excercises in 

https://en.wikibooks.org/wiki/Haskell/Applicative_functors

section "Applicative functor laws", which I paste below for convenience

Exercises
---------

1. Check that the Applicative laws hold for this instance for Maybe
1. Write Applicative instances for
    * `Either e`, for a fixed `e`
    * `((->) r)`, for a fixed `r`

