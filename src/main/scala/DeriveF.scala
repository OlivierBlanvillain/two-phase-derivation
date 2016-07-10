import shapeless._
import shapeless.ops.hlist.{LiftAll, ToTraversable}
import cats.data.Xor
import scala.reflect.runtime.universe.TypeTag

import cats.syntax.invariant._
import cats.syntax.cartesian._
import DisjointCartesian.ops._

trait LiftF[F[_], Repr] { self =>
  def get[A: TypeTag]: F[A]
  def cast[R]: LiftF[F, R] = this.asInstanceOf[LiftF[F, R]]
}

object LiftF {
  implicit def viaLiftAll[F[_], Repr, FlatRepr <: HList, LiftedRepr <: HList, TypeTags <: HList]
    (implicit
      le: Leaves.Aux[Repr, FlatRepr],
      la: LiftAll.Aux[F,       FlatRepr, LiftedRepr],
      tt: LiftAll.Aux[TypeTag, FlatRepr, TypeTags],
      ll: ToTraversable.Aux[LiftedRepr, List, F[_]],
      lt: ToTraversable.Aux[TypeTags,   List, TypeTag[_]]
    ): LiftF[F, Repr] =
      new LiftF[F, Repr] {
        val map: Map[TypeTag[_], F[_]] =
          tt.instances.toList[TypeTag[_]].zip(la.instances.toList[F[_]]).toMap

        def get[A: TypeTag]: F[A] = map(implicitly[TypeTag[A]]).asInstanceOf[F[A]]
      }
}

/** First phase of automatic type class derivation for `A`. */
trait DeriveF[A] { self =>
  type Repr

  def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[A]

  def flatten[FR <: HList](implicit l: Leaves.Aux[Repr, FR]): DeriveFFlat.Aux[A, FR] =
    new DeriveFFlat[A] {
      type FlatRepr = FR
      val d: DeriveF[A] = self
    }
}

object DeriveF {
  type Aux[A, R0] = DeriveF[A] { type Repr = R0 }

  // h: HasNoGeneric[A] → DeriveF[A] { type Repr = A }
  implicit def caseNoGeneric[A: TypeTag](implicit h: HasNoGeneric[A]): Aux[A, A] = new DeriveF[A] {
    type Repr = A
    def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[A] = l.get[A]
  }

  // g: Generic[A], r: DeriveF[g.Repr] → DeriveF[A] { type Repr = g.Repr }
  implicit def caseGeneric[A, G, R]
    (implicit
      g: Generic.Aux[A, G],
      r: Lazy[Aux[G, R]]
    ): Aux[A, R] = new DeriveF[A] {
      type Repr = R
      def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[A] =
        r.value.derive[F].imap(g.from)(g.to)
    }

  // d: DeriveF[H] → DeriveF[H :: HNil] { type Repr = d.Repr }
  implicit def caseHLast[H, R](implicit r: Aux[H, R]): Aux[H :: HNil, R :: HNil] =
    new DeriveF[H :: HNil] {
      type Repr = R :: HNil
      def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[H :: HNil] =
        r.derive[F](l.cast, c).imap { a => a :: HNil } { case a :: HNil => a }
    }

  // This case is impossible as they are not value of type `CNil`.
  implicit def caseCNil: Aux[CNil, CNil] = new DeriveF[CNil] {
    type Repr = CNil
    def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[CNil] = null.asInstanceOf[F[CNil] ]
  }

  // h: DeriveF[H], t: DeriveF[T] → DeriveF[H :: T] { type Repr = h.Repr :: t.Repr }
  implicit def caseHCons[H, HR, T <: HList, TR <: HList]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]]
    ): Aux[H :: T, HR :: TR] =
      new DeriveF[H :: T] {
        type Repr = HR :: TR
        def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[H :: T] =
          h.derive(l.cast, c).product(t.value.derive[F](l.cast, c))
            .imap { case (a, b) => a :: b } { case a :: b => (a, b) }
      }

  // h: DeriveF[H], t: DeriveF[T] → DeriveF[H :+: T] { type Repr = h.Repr :+: t.Repr }
  implicit def caseCCons[H, HR, T <: Coproduct, TR <: Coproduct]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]]
    ): Aux[H :+: T, HR :+: TR] =
      new DeriveF[H :+: T] {
        type Repr = HR :+: TR
        def derive[F[_]](implicit l: LiftF[F, Repr], c: CanDerive[F]): F[H :+: T] =
          h.derive(l.cast, c).coproduct(t.value.derive[F](l.cast, c))
            .imap {
              case Xor.Left(a)  => Inl(a)
              case Xor.Right(b) => Inr(b)
            } {
              case Inl(a) => Xor.Left(a)
              case Inr(b) => Xor.Right(b)
            }
      }
}

/** Second phase of automatic type class derivation for `F[A]`. */
trait DeriveFFlat[A] {
  type FlatRepr <: HList
  def d: DeriveF[A]
}

trait DeriveFFlatBoilerplate {
  // implicit class case1implicits[A, I1]
  // implicit class case2implicits[A, I1, I2]
  // implicit class case3implicits[A, I1, I2, I3]
  // implicit class case4implicits[A, I1, I2, I3, I4]
  implicit class case5implicits[A, I1: TypeTag, I2: TypeTag, I3: TypeTag, I4: TypeTag, I5: TypeTag]
    (self: DeriveFFlat.Aux[A, I1 :: I2 :: I3 :: I4 :: I5 :: HNil]) {
      def materialize[F[_]]
        (implicit
          I1: F[I1],
          I2: F[I2],
          I3: F[I3],
          I4: F[I4],
          I5: F[I5],
          c: CanDerive[F]
        ): F[A] = self.d.derive(new LiftF[F, HNil] {
          import Predef.{implicitly => i}

          val map: Map[TypeTag[_], F[_]] =
            Map(
              i[TypeTag[I1]] -> I1,
              i[TypeTag[I2]] -> I2,
              i[TypeTag[I3]] -> I3,
              i[TypeTag[I4]] -> I4,
              i[TypeTag[I5]] -> I5)

          def get[T: TypeTag]: F[T] = map(i[TypeTag[T]]).asInstanceOf[F[T]]
        }.cast, c)
    }
}

object DeriveFFlat extends DeriveFFlatBoilerplate {
  type Aux[A, R] = DeriveFFlat[A] { type FlatRepr = R }
}

object DeriveFTest {
  import Model._
  import shapeless.test.illTyped
  import cats.Show

  val deriveF = the[DeriveF[IDAABBS]]
  type Expected1 = Int :: (Double :: ((String :: HNil) :+: (String :: HNil) :+: CNil) :: HNil) :: String :: HNil
  implicitly[deriveF.Repr =:= Expected1]

  val deriveFFlat = deriveF.flatten
  type Expected2 = Int :: Double :: String :: String :: String :: HNil
  implicitly[deriveFFlat.FlatRepr =:= Expected2]

  {
    import cats.std.all._
    val showIDAABBS1: Show[IDAABBS] = deriveF.derive[Show]
    assert(showIDAABBS1.show(instance) == showResult)
  }

  {
    import cats.std.all._
    val showIDAABBS2: Show[IDAABBS] = deriveF.flatten.materialize[Show]
    assert(showIDAABBS2.show(instance) == showResult)
  }

  illTyped(
    "deriveF.derive[Show]",
    "could not find implicit value for parameter l: LiftF\\[cats.Show,DeriveFTest.deriveF.Repr\\]")

  illTyped(
    "deriveF.flatten.materialize[Show]",
    "could not find implicit value for parameter I1: cats.Show\\[Int\\].*")
}
