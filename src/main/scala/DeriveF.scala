import shapeless._
import cats.data.Xor
import cats.Later
import shapeless.ops.hlist.Selector
import scala.reflect.runtime.universe.TypeTag

import cats.syntax.invariant._
import cats.syntax.cartesian._

trait LiftF[F[_]] {
  def get[T](implicit t: TypeTag[T]): F[T]
}

object LiftF {
  def apply[F[_]](implicit t: LiftF[F]): LiftF[F] = t
}

/** First phase of automatic type class derivation for `A`. */
trait DeriveF[A, S <: HList] { self =>
  type Repr

  def derive[F[_]: LiftF : CanDerive]: F[A]

  def flatten[FR <: HList](implicit l: Leaves.Aux[Repr, FR]): DeriveFFlat.Aux[A, FR, S] =
    new DeriveFFlat[A, S] {
      type FlatRepr = FR
      val d: DeriveF[A, S] = self
    }
}

trait LowPrioDeriveF {
  type Aux[A, R0, S <: HList] = DeriveF[A, S] { type Repr = R0 }

  // h: NotGeneric[A] → DeriveF[A] { type Repr = A }
  implicit def caseNotGeneric[A: TypeTag, S <: HList](implicit h: NotGeneric[A]): Aux[A, A, S] = new DeriveF[A, S] {
    type Repr = A
    def derive[F[_]: LiftF : CanDerive]: F[A] = {
      LiftF[F].get[A]
    }
  }
}

object DeriveF extends LowPrioDeriveF {
  // g: Generic[A], r: DeriveF[g.Repr] → DeriveF[A] { type Repr = g.Repr }
  implicit def caseGeneric[A: TypeTag, G, R, S <: HList]
    (implicit
      n: NotIn[S, A],
      g: Generic.Aux[A, G],
      r: Lazy[Aux[G, R, A :: S]]
    ): Aux[A, R, S] = new DeriveF[A, S] { self =>
      type Repr = R
      def derive[F[_]: LiftF : CanDerive]: F[A] = {
        val tta = implicitly[TypeTag[A]]

        val memoizedLiftF: LiftF[F] = new LiftF[F] {
          def get[T](implicit t: TypeTag[T]): F[T] =
            if(t == tta) self.derive.asInstanceOf[F[T]] else LiftF[F].get(t)
        }

        r.value.derive[F](memoizedLiftF, CanDerive[F]).imap(g.from)(g.to)
      }
    }

  implicit def caseRecursion[A: TypeTag, S <: HList]
    (implicit s: Selector[S, A]): Aux[A, HNil, S] = new DeriveF[A, S] {
      type Repr = HNil
      def derive[F[_]: LiftF : CanDerive]: F[A] = LiftF[F].get[A]
    }

  // d: DeriveF[H] → DeriveF[H :: HNil] { type Repr = d.Repr }
  implicit def caseHLast[H, R, S <: HList](implicit r: Aux[H, R, S]): Aux[H :: HNil, R :: HNil, S] =
    new DeriveF[H :: HNil, S] {
      type Repr = R :: HNil
      def derive[F[_]: LiftF : CanDerive]: F[H :: HNil] =
        r.derive[F].imap { a => a :: HNil } { case a :: HNil => a }
    }

  // → DeriveS[CNil] { type Repr = CNil }
  // Note that this case is impossible as they are not value of type `CNil`.
  implicit def caseCNil[S <: HList]: Aux[CNil, CNil, S] = new DeriveF[CNil, S] {
    type Repr = CNil
    def derive[F[_]: LiftF : CanDerive]: F[CNil] = null.asInstanceOf[F[CNil]]
  }

  // h: DeriveF[H], t: DeriveF[T] → DeriveF[H :: T] { type Repr = h.Repr :: t.Repr }
  implicit def caseHCons[H, HR, T <: HList, TR <: HList, S <: HList]
    (implicit
      h: Aux[H, HR, S],
      t: Lazy[Aux[T, TR, S]]
    ): Aux[H :: T, HR :: TR, S] =
      new DeriveF[H :: T, S] {
        type Repr = HR :: TR
        def derive[F[_]: LiftF : CanDerive]: F[H :: T] = {
          h.derive.product(t.value.derive[F]).imap { case (a, b) => a :: b } { case a :: b => (a, b) }
        }
      }

  // h: DeriveF[H], t: DeriveF[T] → DeriveF[H :+: T] { type Repr = h.Repr :+: t.Repr }
  implicit def caseCCons[H, HR, T <: Coproduct, TR <: Coproduct, S <: HList]
    (implicit
      h: Aux[H, HR, S],
      t: Lazy[Aux[T, TR, S]]
    ): Aux[H :+: T, HR :+: TR, S] =
      new DeriveF[H :+: T, S] {
        type Repr = HR :+: TR
        def derive[F[_]: LiftF : CanDerive]: F[H :+: T] = {
          CanDerive[F].coproduct(Later(h.derive), Later(t.value.derive[F])).imap {
            case Xor.Left(a)  => Inl(a)
            case Xor.Right(b) => Inr(b)
          } {
            case Inl(a) => Xor.Left(a)
            case Inr(b) => Xor.Right(b)
          }
        }
      }
}

/** Second phase of automatic type class derivation for `F[A]`. */
trait DeriveFFlat[A, S <: HList] {
  type FlatRepr <: HList
  def d: DeriveF[A, S]
}

trait DeriveFFlatBoilerplate {
  // implicit class case1implicits[A, I1]
  // implicit class case3implicits[A, I1, I2, I3]
  // implicit class case4implicits[A, I1, I2, I3, I4]
  implicit class case2implicits[A, I1: TypeTag, I2: TypeTag, S <: HList]
    (self: DeriveFFlat.Aux[A, I1 :: I2 :: HNil, S]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], c: CanDerive[F]): F[A] = {
          self.d.derive(new LiftF[F] {
            def tt[T](implicit t: TypeTag[T]): TypeTag[T] = t

            val map: Map[TypeTag[_], F[_]] =
              Map(tt[I1] -> I1, tt[I2] -> I2)

            def get[T](implicit t: TypeTag[T]): F[T] =
              map(t).asInstanceOf[F[T]]
          }, c)
        }
    }

  implicit class case5implicits[A, I1: TypeTag, I2: TypeTag, I3: TypeTag, I4: TypeTag, I5: TypeTag, S <: HList]
    (self: DeriveFFlat.Aux[A, I1 :: I2 :: I3 :: I4 :: I5 :: HNil, S]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], I3: F[I3], I4: F[I4], I5: F[I5], c: CanDerive[F]): F[A] =
          self.d.derive(new LiftF[F] {
            def tt[T](implicit t: TypeTag[T]): TypeTag[T] = t

            val map: Map[TypeTag[_], F[_]] =
              Map(tt[I1] -> I1, tt[I2] -> I2, tt[I3] -> I3, tt[I4] -> I4, tt[I5] -> I5)

            def get[T](implicit t: TypeTag[T]): F[T] =
              map(t).asInstanceOf[F[T]]
          }, c)
    }
}

object DeriveFFlat extends DeriveFFlatBoilerplate {
  type Aux[A, R, S <: HList] = DeriveFFlat[A, S] { type FlatRepr = R }
}

object DeriveFTest extends App {
  import Model._
  import shapeless.test.illTyped
  import cats.Show

  val deriveF = the[DeriveF[IDAABBS, HNil]]

  implicitly[deriveF.Repr =:= (
    Int ::
      (Double ::
      ((String :: HNil) :+:
       (String :: HNil) :+: CNil) ::
      HNil) ::
    String ::
    HNil
  )]

  val deriveFFlat = deriveF.flatten

  implicitly[deriveFFlat.FlatRepr =:= (Int :: Double :: String :: String :: String :: HNil)]

  illTyped(
    "deriveF.flatten.materialize[Show]",
    "could not find implicit value for parameter I1: cats.Show\\[Int\\].*")

  import cats.std.all._
  val showIDAABBS: Show[IDAABBS] = deriveF.flatten.materialize[Show]
  assert(showIDAABBS.show(instance) == showResult)

  // Recursion -----------------------------------------------------

  case class Dog(age: Long)
  case class Cat(name: String, friend: Either[Cat, Dog])

  val deriveCat = the[DeriveF[Cat, HNil]]
  implicitly[deriveCat.Repr =:=
    (String :: ((HNil :: HNil) :+: ((Long :: HNil) :: HNil) :+: CNil) :: HNil)]

  val deriveCatFlat = deriveCat.flatten
  implicitly[deriveCatFlat.FlatRepr =:= (String :: Long :: HNil)]

  val showCat: Show[Cat] = deriveCatFlat.materialize[Show]

  assert(showCat.show(Cat("sansan", Right(Dog(4)))) ==
    "(sansan, [case: [case: 4]])")

  assert(showCat.show(Cat("sansan", Left(Cat("aslan", Right(Dog(4)))))) ==
    "(sansan, [case: (aslan, [case: [case: 4]])])")

  println(showCat.show(Cat("sansan", Right(Dog(4)))))
  println(showCat.show(Cat("sansan", Left(Cat("aslan", Right(Dog(4)))))))
}
