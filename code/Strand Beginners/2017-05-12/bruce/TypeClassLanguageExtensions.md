
We created a Doubleable typeclass

```haskell
class Doubleable a where
    double :: a -> a
```

It's easy to make an ```Integer``` instance of this:
```haskell
instance Doubleable Integer where
    double n = 2 * n
```
but what if we want to give a more generic instance definition: we want any type that is an instance of ```Num``` to also be an instance of ```Doubleable```

We would like to do this:

```haskell
instance (Num a) => Doubleable a where
    double n = 2 * n
```

but Haskell says
```
 Illegal instance declaration for `Doubleable a'
      (All instance types must be of the form (T a1 ... an)
       where a1 ... an are *distinct type variables*,
       and each type variable appears at most once in the instance head.
```
The [Haskell Spec](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-770004.3.2) says **"The type (T u1 … uk) must take the form of a type constructor T applied to simple type variables u1, … uk"**.  In our case ```Doubleable``` is not a type constructor (it is a class) and so can't be used as **T**.  So this is simple: we have just broken a syntax rule.

We can change this by using the ```FlexibleInstances``` language option, thusly:

```{-# LANGUAGE FlexibleInstances #-}```

and we get a different error message:
```
  Constraint is no smaller than the instance head
      in the constraint: Num a
```
This is trickier.  It turns out we have violated one of the Paterson conditions.  (See  [this page](https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/type-class-extensions.html))   The part that matters to us is **"The assertion has fewer constructors and variables (taken together and counting repetitions) than the head"**.  The _assertion_ is the part to the left of the ```=>```, with the _head_ being the part to the right.  So, in our case there are the same number of constructors and variables on both sides, and so the condition is violated.

We can lift this condition by using the ```UndecidableInstances``` option:

```{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}```

(See  [the page linked to above](https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/type-class-extensions.html) for some examples of the trouble that ```UndecidableInstances``` can get you into: there are various ways you can send the typechecker into a loop.)

But in our case we're good (see ```05-flexible.hs```):
```
*Main> :l 05-flexible.hs
[1 of 1] Compiling Main             ( 05-flexible.hs, interpreted )
Ok, modules loaded: Main.
*Main> double 10
20
*Main> double 10::Int
20
*Main> double 10.0
20.0
*Main>
```

Or nearly: now we can't do 
```haskell
instance Doubleable [a] where
    double s = s ++ s
```

See ```10-flexbible.hs```
```
*Main> :l 10-flexible.hs
[1 of 1] Compiling Main             ( 10-flexible.hs, interpreted )
Ok, modules loaded: Main.
*Main> double "10.0"

<interactive>:9:1:
    Overlapping instances for Doubleable [Char]
      arising from a use of `double'
    Matching instances:
      instance Num a => Doubleable a -- Defined at 10-flexible.hs:8:10
      instance Doubleable [a] -- Defined at 10-flexible.hs:12:10
    In the expression: double "10.0"
    In an equation for `it': it = double "10.0"
*Main>
```
So we can compile the instance declaration, but the compiler (in the repl) gets confused when we ask it to find an instance for ```[Char]```

**"GHC requires that that it be unambiguous which instance declaration should be used to resolve a type-class constraint."** 

Why does it think that ```Num a => Doubleable a``` and ```Doubleable [a]``` overlap?  In fact it ignores the assertion (the bit to the left of the arrow) when searching for a match.  (See ```15-ignore_asserions.hs``` to verify this.)  Then, since `a` matches anything, ```Doubleable a``` matches anything ```Doubleable [a]``` does (and more).

We fix this with ```OverlappingInstances```; this allows GHC to resolve ambiguous matches by choosing the most specific one.  (It will still fail if there is not a single most specific instance to choose.)

See ```20-overlapping.hs```.  And in fact, 

```{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}```

is enough to make everything we did in last week's session work.






