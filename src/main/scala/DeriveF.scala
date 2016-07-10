import shapeless._
import cats.data.Xor
import scala.reflect.runtime.universe.TypeTag
import simulacrum.typeclass

import cats.syntax.invariant._
import cats.syntax.cartesian._
import DisjointCartesian.ops._

@typeclass trait LiftF[F[_]] {
  def get[A: TypeTag]: F[A]
}

/** First phase of automatic type class derivation for `A`. */
trait DeriveF[A] { self =>
  type Repr

  def derive[F[_]: LiftF : CanDerive]: F[A]

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
    def derive[F[_]: LiftF : CanDerive]: F[A] = LiftF[F].get[A]
  }

  // g: Generic[A], r: DeriveF[g.Repr] → DeriveF[A] { type Repr = g.Repr }
  implicit def caseGeneric[A, G, R]
    (implicit
      g: Generic.Aux[A, G],
      r: Lazy[Aux[G, R]]
    ): Aux[A, R] = new DeriveF[A] {
      type Repr = R
      def derive[F[_]: LiftF : CanDerive]: F[A] =
        r.value.derive[F].imap(g.from)(g.to)
    }

  // d: DeriveF[H] → DeriveF[H :: HNil] { type Repr = d.Repr }
  implicit def caseHLast[H, R](implicit r: Aux[H, R]): Aux[H :: HNil, R :: HNil] =
    new DeriveF[H :: HNil] {
      type Repr = R :: HNil
      def derive[F[_]: LiftF : CanDerive]: F[H :: HNil] =
        r.derive[F].imap { a => a :: HNil } { case a :: HNil => a }
    }

  // → DeriveS[CNil] { type Repr = CNil }
  // Note that this case is impossible as they are not value of type `CNil`.
  implicit def caseCNil: Aux[CNil, CNil] = new DeriveF[CNil] {
    type Repr = CNil
    def derive[F[_]: LiftF : CanDerive]: F[CNil] = null.asInstanceOf[F[CNil]]
  }

  // h: DeriveF[H], t: DeriveF[T] → DeriveF[H :: T] { type Repr = h.Repr :: t.Repr }
  implicit def caseHCons[H, HR, T <: HList, TR <: HList]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]]
    ): Aux[H :: T, HR :: TR] =
      new DeriveF[H :: T] {
        type Repr = HR :: TR
        def derive[F[_]: LiftF : CanDerive]: F[H :: T] =
          h.derive.product(t.value.derive[F])
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
        def derive[F[_]: LiftF : CanDerive]: F[H :+: T] =
          h.derive.coproduct(t.value.derive[F])
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
        (implicit I1: F[I1], I2: F[I2], I3: F[I3], I4: F[I4], I5: F[I5], c: CanDerive[F]): F[A] =
          self.d.derive(new LiftF[F] {
            def tt[T](implicit t: TypeTag[T]): TypeTag[T] = t

            val map: Map[TypeTag[_], F[_]] =
              Map(tt[I1] -> I1, tt[I2] -> I2, tt[I3] -> I3, tt[I4] -> I4, tt[I5] -> I5)

            def get[T: TypeTag]: F[T] = map(tt[T]).asInstanceOf[F[T]]
          }, c)
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

  illTyped(
    "deriveF.flatten.materialize[Show]",
    "could not find implicit value for parameter I1: cats.Show\\[Int\\].*")

  import cats.std.all._
  val showIDAABBS: Show[IDAABBS] = deriveF.flatten.materialize[Show]
  assert(showIDAABBS.show(instance) == showResult)
}
