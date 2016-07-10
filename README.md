### Two Phase Derivation

This project is an experiment with an alternative automatic type class derivation mechanism. The main motivation is to improve the error reporting compared the opaque `implicit not found` that scalac returns with the traditional shapeless way.

This technique works in two phases. First, it computes the *complete* generic representation of a data type as a tree of `HList` and `Coproduct`, using the macros already present in shapeless (plus a [tiny addition](https://github.com/milessabin/shapeless/pull/616)). This phase involves a *total* type class, meaning that the compilation for the first phase never fails, independently of the implicits present in scope. Second, it materializes a type class instance for the given data type by summoning and combining type classes instances for the types at the leaf of the generic representation. This has some nice consequences:

1. Because all the "unsafe" resolution (which involves external implicits) is isolated in a single, non-recursive step, it is possible to provide accurate errors when something is missing. Concretely, the first step involves resolving a `trait FlatGeneric[T] { type Repr <: HList }`, which is analogous to shapeless `Generic`, but computes the recursive representation of a data type, and stores the result in a flat `HList`. Given an instance of `FlatGeneric`, is then possible to use an implicit class (one for each `HList` size) to do all the "unsafe" implicit resolution in a single function call:

    ```scala
    implicit class case3implicits[T, I1, I2, I3]
      (self: DeriveS.Aux[T, I1 :: I2 :: I3 :: HNil]) {
        def materialize[F[_]](implicit I1: F[I1], I2: F[I2], I3: F[I3]): F[T] = ???
      }
    ```

    If something goes wrong while calling `materialize` in `Derive[MyCaseClass].materialize[Encoder]`, user get a nice report saying which implicits are missing.

2. Most of the compile time involved in a derivation is be spend in the first phase. Because of this it should be possible to cache the implicit computation done on the first phase to make incremental compilation faster. In theory one could also gain time by reuse the first phase for several type classes, however with the small example used in the benchmark it would take 10 type classes for the two phase approach. It should also be possible to gain type by doing all the work of the first phase in a single macro, similarity to what was done for Circe in https://github.com/travisbrown/circe/pull/247.

3. The logic for combining parts the derived entity can be logically isolated a `CanDerive` type class. This makes for a clear contract with the type class derivation implementer which now needs to implement a single interface.

Now for the limitations:

1. With traditional shapeless derivation, when deriving a type class `TC[_]`, at each step of the recursion for type `A` if there are both `Generic[A]` and `TC[A]` in scope the mechanism cuts this branch short by using `TC[A]`. In the two-phase approach, this is not possible as the recursive generic representation is computed upfront, which means that data types will always be decomposed down to the leaf primitive types.

2. The current implementation is limited to a single "type" of derivation. It should be possible to extend the mechanism to use `LabeledGeneric`, and have a `HList` only version with less constrained `CanDerive` requirements. Similarly deriving type classes for other kinds such as `Functor: ((* -> *) -> *)` are out of the scope of this proof of concept.

### Design

This repository contains two implementations of this idea, `DeriveF` (F for Fast) and `DeriveS` (F for Slow). The design of each implementation is briefly explained below.


**`DeriveF`** is a defined as follows:

```
trait DeriveF[A] {
  type Repr

  def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[A]
}
```

Where `Repr` is a HList/Coproduct tree of primitive data types of HList. For example, given the following model:

```scala
sealed trait AABB
case class AA(a: String) extends AABBs
case class BB(a: String) extends AABB
case class DAABB(d: Double, aabb: AABB)
case class IDAABBS(i: Int, daabb: DAABB, s: String)
```

`DeriveF[IDAABBS].Repr` looks like the following:

```scala
type Expected1 =
  Int ::
    (Double ::
    ((String :: HNil) :+:
     (String :: HNil) :+: CNil) ::
    HNil) ::
  String ::
  HNil

val df = the[DeriveF[IDAABBS]]
implicitly[df.Repr =:= Expected1]
```

The rules to construct a `DeriveF` are the following:

- caseNoGeneric: `h: HasNoGeneric[A] → DeriveF[A] { type Repr = A }`
- caseGeneric: `g: Generic[A], r: DeriveF[g.Repr] → DeriveF[A] { type Repr = g.Repr }`
- caseHLast: `→ DeriveS[CNil] { type Repr = CNil }`
- caseCNil: `d: DeriveF[H] → DeriveF[H :: HNil] { type Repr = d.Repr }`
- caseHCons: `h: DeriveF[H], t: DeriveF[T] → DeriveF[H :: T] { type Repr = h.Repr :: t.Repr }`
- caseCCons: `h: DeriveF[H], t: DeriveF[T] → DeriveF[H :+: T] { type Repr = h.Repr :+: t.Repr }`

This construction is total under the assumption that every type has either a `Generic` or a `HasNoGeneric` instance. In addition, the type manipulation stated above, this consists also recursively builds implementations of the `derive` method by lifting intermediate results into `F[_]` using `LiftF` (explained later), and combining them using the `CanDerive` type class:

```scala
/** `Xor` version of `Cartesian`... */
@typeclass trait DisjointCartesian[F[_]] {
  def coproduct[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

/** All you need for automatic type class derivation of `F[_]`. */
@typeclass trait CanDerive[F[_]] extends Invariant[F] with Cartesian[F] with DisjointCartesian[F]
```

The next step consists in flattening the `Repr` above into a single, flat `HList`. This is done with a `trait Leaves[Repr] { type FlatRepr <: HList }` type class which does some (pretty involved) induction on `Repr`, end ends up transforming the `type Expected1` above into the following:

```scala
type Expected2 = Int :: Double :: String :: String :: String :: HNil

val dfFlat = df.flatten
implicitly[dfFlat.FlatRepr =:= Expected2]
```

The final step is the ugliest. It's now time to take a deep breath and look at the signature of `LiftF`:

```scala
trait LiftF[F[_], Repr] { self =>
  def get[A: TypeTag]: F[A]
  def cast[R]: LiftF[F, R] = this.asInstanceOf[LiftF[F, R]]
}
```

With `get` being a function from `TypeTag[A]` to `F[A]` it is possible to store all the `F` instance for leaves in a single `Map` indexed by `TypeTag`, and propagate the same `LiftF` instance all the way through the derivation. Constructing a `LiftF` instance is done with a `materialize` method like the following, added with an implicit class on top the `FlatRepr` type:

```scala
implicit class case2implicits[A, I1: TypeTag, I2: TypeTag]
  (self: DeriveFFlat.Aux[A, I1 :: I2 :: HNil]) {
    def materialize[F[_]]
      (implicit I1: F[I1], I2: F[I2] c: CanDerive[F]): F[A] =
        self.d.derive(new LiftF[F, HNil] {
          import Predef.{implicitly => i}
          val map: Map[TypeTag[_], F[_]] = Map(i[TypeTag[I1]] -> I1, i[TypeTag[I2]] -> I2)
          def get[T: TypeTag]: F[T]      = map(i[TypeTag[T]]).asInstanceOf[F[T]]
        }.cast, c)
  }
```

Now you are probably thinking that there should be a way to do the same construction without the need for `TypeTag`.

**`DeriveS`** is an equivalent construction to `DeriveF`, which does not relies on a `Map` of `TypeTag` and properly propagates the leaves type classes instance at the type level. Do to so, the `Repr` of `DeriveS` flatten "on the fly" (and not in a separate phase like for it is in `DeriveF`), which makes it possible to keep track of the corresponding `LiftedRepr` in `LiftS`:

```scala
trait LiftS[F[_], Repr <: HList, LiftedRepr <: HList] { self =>
  def instances: LiftedRepr
}
```

There is some complicated machinery involved (see the `Append` type class) in the derivation to keep `Repr` and `LiftedRepr` in sync, which lets us define `get` very elegantly:

```scala
implicit class LiftGet[F[_], A](self: LiftS[F, A :: HNil, F[A] :: HNil]) {
  def get: F[A] = self.instances.head
}
```

### Benchmarks

This table shows compilation time for various pieces of code involved in the derivation. The overall process take about 0.5 seconds with `DeriveF`, about 25 seconds with `DeriveS` and 0.1 seconds with traditional shapeless derivation (`TShow`).

|          Scala Code           |        Compilation Time       |
|-------------------------------|:-----------------------------:|
|`the[DeriveF[IDAABBS]]        `|         0.4315 seconds        |
|`deriveF.flatten              `|         0.0578 seconds        |
|`deriveFFlat.materialize[Show]`|         0.0435 seconds        |
|`the[DeriveS[IDAABBS]]        `|         25.141 seconds        |
|`deriveS.materialize[Show]    `|         0.0044 seconds        |
|`implicitly[TShow[IDAABBS]]   `|         0.0934 seconds        |
