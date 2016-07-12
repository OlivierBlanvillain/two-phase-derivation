### Two Phase Derivation

This project is an experiment with an alternative automatic type class derivation mechanism. The main objective was to provide accurate error reporting, instead of the opaque `implicit not found` that are encountered with the traditional shapeless way of doing type class derivation.

The key for this technique is to split the work into two phases. First, it computes the complete generic representation of a data type, as a tree of `HList` and `Coproduct`, using the macros already present in shapeless (plus a [tiny addition](https://github.com/milessabin/shapeless/pull/616)). Second, it materializes the desired type class by summoning and combining instances for all types at leaf position in the generic representation. This separation has some nice consequences:

1. Because all the "unsafe" implicit resolution appends in isolation, and at the very end of the precess, it becomes possible to report accurate errors about which implicits are missing. This can be achieved with a boilerplate alternative to [LiftAll](https://github.com/milessabin/shapeless/blob/92f2d5e3fede4ab189db686620fa175fe4856e1a/core/src/main/scala/shapeless/ops/hlists.scala#L2809-L2838), which, given an `HList` which shape `A :: B :: C :: HNil`, summons three implicits `F[A]`, `F[B]` and `F[C]`:

    ```scala
    implicit class case3implicits[T, I1, I2, I3]
      (self: Lifting[I1 :: I2 :: I3 :: HNil]) {
        def materialize[F[_]](implicit I1: F[I1], I2: F[I2], I3: F[I3]) = ???
      }
    ```

    Most of the complexity explained in the design section comes from doing the back and forth between the flat `HList` generic representation needed for the above lifting, and the `HList` / `Coproduct` tree generic representation which obtained from shapeless macros and aligned the actual data types.

2. Most of the compile time involved in the two-phase derivation process is be spend in the first phase, which is independent of the type class being derived and, for a given data type, can be done "one and for all". It should theoretically be possible to improve incremental computation time and diminish the amount of classes generated (which is significant for Scala.js) by caching this first phase. It should also be possible to improve compilation time by doing all the work of the first phase in a single macro, similarity to what was done for Circe in https://github.com/travisbrown/circe/pull/247, but without the tight coupling to a certain type classes.

3. The logic for combining parts the derived entity can be logically isolated a `CanDerive` type class. This makes for a clear contract with the type class derivation implementer which now needs to implement a single interface.

Now for the limitations:

1. When deriving a type class `TC[_]` with traditional shapeless derivation, each step of the recursion looks for both `Generic[A]` and `TC[A]` instances, and when both are available implicit priority with cuts short the search by using `TC[A]`. In the two-phase approach, this is not possible as the recursive generic representation is computed upfront, which means that data types will always be decomposed down to the leaf primitive types.

2. The current implementation is limited to a single "type" of derivation. It should be possible to extend the mechanism to use `LabeledGeneric`, to have an `HList` only version with a less constrained `CanDerive` requirements, and so one. Similarly deriving type classes for other kinds such as `Functor: ((* -> *) -> *)` are out of the scope of this proof of concept.

### Design

This repository contains two proof of concept implementations of this idea, `DeriveF` (F for Fast) and `DeriveS` (S for Slow). The design of `DeriveF` is briefly explained below:

```
trait DeriveF[A] {
  type Repr

  def derive[F[_]: LiftF : CanDerive]: F[A]
}
```

The `Repr` type represents the `HList` / `Coproduct` generic tree representation of `A`. For example, given these case classes:

```scala
sealed trait AABB
case class AA(a: String) extends AABBs
case class BB(a: String) extends AABB
case class DAABB(d: Double, aabb: AABB)
case class IDAABBS(i: Int, daabb: DAABB, s: String)
```

`DeriveF[IDAABBS].Repr` looks like the following:

```scala
val deriveF = the[DeriveF[IDAABBS]]

implicitly[deriveF.Repr =:= (
  Int ::
    (Double ::
    ((String :: HNil) :+:
     (String :: HNil) :+: CNil) ::
    HNil) ::
  String ::
  HNil
)]
```

The rules to construct a `DeriveF` are the following:

- caseNoGeneric: `h: HasNoGeneric[A] → DeriveF[A] { type Repr = A }`
- caseGeneric: `g: Generic[A], r: DeriveF[g.Repr] → DeriveF[A] { type Repr = g.Repr }`
- caseHLast: `→ DeriveS[CNil] { type Repr = CNil }`
- caseCNil: `d: DeriveF[H] → DeriveF[H :: HNil] { type Repr = d.Repr }`
- caseHCons: `h: DeriveF[H], t: DeriveF[T] → DeriveF[H ::  T] { type Repr = h.Repr ::  t.Repr }`
- caseCCons: `h: DeriveF[H], t: DeriveF[T] → DeriveF[H :+: T] { type Repr = h.Repr :+: t.Repr }`

This construction is total under the assumption that every type has either a `Generic` or a `HasNoGeneric` instance. In addition to the type manipulation describe above, the construction of `DeriveF` also takes care of recursively building implementations of the `derive` method by obtaining `F[_]` instances from `LiftF` (explained later), and combining them using the `CanDerive` type class:

```scala
/** `Xor` version of `Cartesian`... */
@typeclass trait DisjointCartesian[F[_]] {
  def coproduct[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

/** All you need for automatic type class derivation of `F[_]`. */
@typeclass trait CanDerive[F[_]] extends Invariant[F] with Cartesian[F] with DisjointCartesian[F]
```

The next step consists in flattening the tree `Repr` into a single, flat `HList`. This is done with a `trait Leaves[Repr] { type FlatRepr <: HList }` type class which does some (pretty involved) induction on `Repr`, and would end up transforming the tree representation showed above into the following:

```scala
val deriveFFlat = df.flatten
implicitly[deriveFFlat.FlatRepr =:= (Int :: Double :: String :: String :: String :: HNil)]
```

The last piece of the puzzle is also the ugliest, it the `LiftF` type class which takes case of aggregating and restituting `F[_]` instances:

```scala
trait LiftF[F[_]] {
  def get[A: TypeTag]: F[A]
}
```

With `get` being a function from `TypeTag[A]` to `F[A]` it is possible to store all `F[_]` instance for data types of leave position in `Map` indexed by `TypeTag`, and propagate the same `LiftF` instance all the way through the derivation. Constructing a `LiftF` instance is done with a `materialize` method similar to the boilerplate version of [LiftAll](https://github.com/milessabin/shapeless/blob/92f2d5e3fede4ab189db686620fa175fe4856e1a/core/src/main/scala/shapeless/ops/hlists.scala#L2809-L2838) described above:

```scala
implicit class case2implicits[A, I1: TypeTag, I2: TypeTag]
  (self: DeriveFFlat.Aux[A, I1 :: I2 :: HNil]) {
    def materialize[F[_]]
      (implicit I1: F[I1], I2: F[I2] c: CanDerive[F]): F[A] =
        self.d.derive(new LiftF[F] {
          import Predef.{implicitly => i}
          val map: Map[TypeTag[_], F[_]] = Map(i[TypeTag[I1]] -> I1, i[TypeTag[I2]] -> I2)
          def get[T: TypeTag]: F[T]      = map(i[TypeTag[T]]).asInstanceOf[F[T]]
        }, c)
  }
```

Now you are probably thinking that there *must be* a nicer way to do the same construction which does not involves `TypeTag`s...

`DeriveS` is exactly that. It's an equivalent construction to `DeriveF`, but instead of relying on a `Map` of `TypeTag`, it properly propagates the appropriate type classes instances at the type level. Do to so, the `Repr` type member of `DeriveS` flatten "on the fly" (and not in a separate step like what `DeriveF` does with `Leaves`). This makes it possible to use a safe version `LiftS` which keeps track the instances it carries in a `LiftedRepr` `HList`, which holds the exact same element that `Repr` but lifted to `F[_]`:

```scala
trait LiftS[F[_], Repr <: HList, LiftedRepr <: HList] { self =>
  def instances: LiftedRepr
}
```

There is some complicated machinery involved in the derivation (see the `Append` type class) to keep `Repr` and `LiftedRepr` in sync. The gain is that `get` can be defined very elegantly, with a type level guaranty that the requested `F[A]` is the only element present:

```scala
implicit class LiftGet[F[_], A](self: LiftS[F, A :: HNil, F[A] :: HNil]) {
  def get: F[A] = self.instances.head
}
```

### Benchmarks

This table shows compilation time for various pieces of code involved in the derivation of the `IDAABBS` data type given above. The overall process to derive a `Show[IDAABBS]` instance takes ~0.5 seconds with `DeriveF`, ~25 seconds with `DeriveS` and ~0.1 seconds with traditional shapeless derivation (`TShow`).

|          Scala Code           |        Compilation Time       |
|-------------------------------|:-----------------------------:|
|`the[DeriveF[IDAABBS]]        `|         0.4315 seconds        |
|`deriveF.flatten              `|         0.0578 seconds        |
|`deriveFFlat.materialize[Show]`|         0.0435 seconds        |
|`the[DeriveS[IDAABBS]]        `|         25.141 seconds        |
|`deriveS.materialize[Show]    `|         0.0044 seconds        |
|`the[TShow[IDAABBS]]          `|         0.0934 seconds        |
