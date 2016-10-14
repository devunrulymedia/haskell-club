haskell-club
============

The first Rule of Haskell Club is.....

Read http://learnyouahaskell.com/

We're now up to half way through chapter 11, and have just encountered the `newtype` keyword 

Next session we'll continue to practice with Applicative Functors.  We should understand:
 * the `<*>` implementation for `Maybe`, `[]`, `((->) a)` and `IO` 
 * the Applicative Laws
 * why `<$>` exists

At the end of the last session, we defined the new type:
```
newtype Validation a = Validation { getValidation :: Either String a }
```

For next session, we should:
 * Define instances of Functor and Applicative for Validation
 * Investigate the ZipList newtype, and its implementations of Functor and Applicative
