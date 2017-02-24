# Functors and Applicatives Problems

## Functors

1. Create a tuple (e.g. a pair) datatype with each element of the same type.  Make it an instance of `Data.Functor`
1. Create a tuple (e.g. a pair) datatype with each element not necessarily of the same type.  Make it an instance of `Data.Functor`.  (Hint: you’ll need to pick a side).
1. Verify that the functor laws hold for the types we created in 1 and 2.
1. (Optional) It should be trivial to make the type you created in 2 an instance of Functor on the other side.  Can we make it implement Functor in 2 ways at the same time?
1. Discuss: what do we gain by the extra layer of generalisation

## Applicatives

1. Make the type defined in 2 above an instance of `Control.Applicative`
1. Verify that your new Applicative obeys the applicative laws.

