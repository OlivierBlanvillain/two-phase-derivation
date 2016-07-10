# two-phase-derivation

This project is an experiment with an alternative automatic type class derivation mechanism. The main motivation is to improve the error reporting compared the opaque `implicit not found` that scalac returns with the traditional shapeless way.

This technique work in two phases. First, it computes the *complete* generic representation of a data type as a tree of `HList` and `Coproduct`, using the macros already present in shapeless (plus a [tiny addition](https://github.com/milessabin/shapeless/pull/616)). This phase involves a *total* type class, meaning that it compilation for the first phase never fails, independently of the implicits present in scope. Second, it materializes a type class instance for the given data type by summoning and combining type classes instances for the types at the leaf of the generic representation. This is has some nice consequences:

1. Because all the "unsafe" resolution (which involves external implicits) is isolated in a single, non recursive step, it is possible to provide accurate errors when something is missing. Concretely, the first step involves resolving a `trait FlatGeneric[T] { type Repr <: HList }`, which is analogous to shapeless `Generic`, but computes the recursive representation of a data type, and stores the result in a flat `HList`. Given an instance of `FlatGeneric`, is then possible to use an implicit class (one for each `HList` size) to do all the "unsafe" implicit resolution in a single function call:

    ```scala
    implicit class case3implicits[T, I1, I2, I3]
      (self: DeriveS.Aux[T, I1 :: I2 :: I3 :: HNil]) {
        def materialize[F[_]](implicit I1: F[I1], I2: F[I2], I3: F[I3]): F[T] = ???
      }
    ```

    If something goes wrong while calling `materialize` in `Derive[MyCaseClass].materialize[Encoder]`, user get a nice report saying which implicits are missing.

2. Compile time should be close to constant in the number of type classes derived for the same data type. This is a consequence of being able to reuse the work done in the first phase, which is all the heavy lifting of generating the generic representation with macros. It should also be possible to do all the work of the first phase in a single macro, similarity to what Circe did in https://github.com/travisbrown/circe/pull/247 but in a non specialized way. (This point is hypothetical, I did not benchmarked anything.)

3. The logic for combining parts the derived entity can be logically isolated a `CanDerive` type class. This makes for a clear contract with the type class derivation implementer which now needs to implement a single interface.

Now for the limitations:

1. With traditional shapeless derivation, when deriving a type class `TC[_]`, at each step of the recursion for type `A` if there are both `Generic[A]` and `TC[A]` in scope the mechanism cuts this branch short by using `TC[A]`. In the two phase approach this is not possible as the recursive generic representation is computed upfront, which means that data types will always be decomposed down to the leaf primitive types.

2. The current implementation is limited to a single "type" of derivation. It should be possible to extend the mechanism to use `LabeledGeneric`, and have a `HList` only version with less constrained `CanDerive` requirements. Similarly deriving type classes for other kinds such as `Functor: ((* -> *) -> *)` are out of the scope of this proof of concept.
