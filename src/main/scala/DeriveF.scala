package deriving

import scala.reflect.runtime.universe.TypeTag
import Syntax._

trait LiftF[F[_]] {
  def get[T](implicit t: TypeTag[T]): F[T]
}

object LiftF {
  def apply[F[_]](implicit t: LiftF[F]): LiftF[F] = t
}

/** First phase of automatic type class derivation for `A`. */
trait DeriveF[A, S <: HList] { self =>
  type TreeRepr
  def derive[F[_]: LiftF : CanDerive]: F[A]
}

trait LowPrioDeriveF {
  type Aux[A, R0, S <: HList] = DeriveF[A, S] { type TreeRepr = R0 }

  implicit def caseNotGeneric[A: TypeTag, S <: HList](implicit h: NotGeneric[A]): Aux[A, A, S] = new DeriveF[A, S] {
    type TreeRepr = A
    def derive[F[_]: LiftF : CanDerive]: F[A] = {
      LiftF[F].get[A]
    }
  }
}

object DeriveF extends LowPrioDeriveF {
  // Note that `A` is also added to the list of `Seen` generics, which prevents
  // this case to be used more than once for the same A (see NotIn).
  implicit def caseGeneric[A: TypeTag, G, R, S <: HList]
    (implicit
      n: NotIn[S, A],
      g: Generic.Aux[A, G],
      r: shapeless.Lazy[Aux[G, R, A :: S]]
    ): Aux[A, R, S] = new DeriveF[A, S] { self =>
      type TreeRepr = R
      def derive[F[_]: LiftF : CanDerive]: F[A] = {
        val tta = implicitly[TypeTag[A]]

        // Memoize the current `DeriveF` in `LiftF` for potential recursive structure.
        // Note that the `self.derive` invocation is done inside a def, which prevents
        // infinite loops (but is probably not enough to be stack safe...).
        val memoizedLiftF: LiftF[F] = new LiftF[F] {
          def get[T](implicit t: TypeTag[T]): F[T] =
            if(t.tpe =:= tta.tpe) self.derive.asInstanceOf[F[T]] else LiftF[F].get(t)
        }

        r.value.derive[F](memoizedLiftF, CanDerive[F]).imap(g.from)(g.to)
      }
    }

  // When `A ∈ Seen`, we break the recursion by obtaining `F[A]` directly from `LiftF[A]`.
  // This is sound because every element added to `Seen` comes with a new entry in `LiftF`.
  implicit def caseMemoizedGeneric[A: TypeTag, S <: HList]
    (implicit s: Selector[S, A]): Aux[A, HNil, S] = new DeriveF[A, S] {
      type TreeRepr = HNil
      def derive[F[_]: LiftF : CanDerive]: F[A] = LiftF[F].get[A]
    }

  implicit def caseHNil[S <: HList]: Aux[HNil, HNil, S] =
    new DeriveF[HNil, S] {
      type TreeRepr = HNil
      def derive[F[_]: LiftF : CanDerive]: F[HNil] =
        CanDerive[F].unit.imap[HNil](_ => HNil)(_ => ())
    }

  implicit def caseHCons[H, HR, T <: HList, TR <: HList, S <: HList]
    (implicit
      h: Aux[H, HR, S],
      t: shapeless.Lazy[Aux[T, TR, S]]
    ): Aux[H :: T, HR :: TR, S] =
      new DeriveF[H :: T, S] {
        type TreeRepr = HR :: TR
        def derive[F[_]: LiftF : CanDerive]: F[H :: T] = {
          h.derive.product(t.value.derive[F]).imap { case (a, b) => ::(a, b) } { case a :: b => (a, b) }
        }
      }

  implicit def caseCNil[S <: HList]: Aux[CNil, CNil, S] = new DeriveF[CNil, S] {
    type TreeRepr = CNil
    def derive[F[_]: LiftF : CanDerive]: F[CNil] = null.asInstanceOf[F[CNil]]
  }

  implicit def caseCCons[H, HR, T <: Coproduct, TR <: Coproduct, S <: HList]
    (implicit
      h: Aux[H, HR, S],
      t: shapeless.Lazy[Aux[T, TR, S]]
    ): Aux[H :+: T, HR :+: TR, S] =
      new DeriveF[H :+: T, S] {
        type TreeRepr = HR :+: TR
        def derive[F[_]: LiftF : CanDerive]: F[H :+: T] = {
          CanDerive[F].coproduct(h.derive, t.value.derive[F]).imap {
            case Left(a)  => Inl(a)
            case Right(b) => Inr(b)
          } {
            case Inl(a) => Left(a)
            case Inr(b) => Right(b)
          }
        }
      }
}

/** Second phase of automatic type class derivation for `F[A]`. */
trait DerivingBoilerplate {
  implicit class case1implicits[A, I1: TypeTag, T0]
    (self: DerivingF.Aux[A, T0, I1 :: HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], c: CanDerive[F]): F[A] = {
          self.underlying.derive(new LiftF[F] {
            def get[T](implicit t: TypeTag[T]): F[T] = I1.asInstanceOf[F[T]]
          }, c)
        }
    }
  implicit class case2implicits[A, I1: TypeTag, I2: TypeTag, T0]
    (self: DerivingF.Aux[A, T0, I1 :: I2 :: HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], c: CanDerive[F]): F[A] = {
          self.underlying.derive(new LiftF[F] {
            def tt[T](implicit t: TypeTag[T]): TypeTag[T] = t

            val map: Map[TypeTag[_], F[_]] =
              Map(tt[I1] -> I1, tt[I2] -> I2)

            def get[T](implicit t: TypeTag[T]): F[T] =
              map(t).asInstanceOf[F[T]]
          }, c)
        }
    }
  // implicit class case3implicits[A, I1: TypeTag, I2: TypeTag, I3: TypeTag, T0]
  // implicit class case4implicits[A, I1: TypeTag, I2: TypeTag, I3: TypeTag, I4: TypeTag, T0]
  implicit class case5implicits[A, I1: TypeTag, I2: TypeTag, I3: TypeTag, I4: TypeTag, I5: TypeTag, T0]
    (self: DerivingF.Aux[A, T0, I1 :: I2 :: I3 :: I4 :: I5 :: HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], I3: F[I3], I4: F[I4], I5: F[I5], c: CanDerive[F]): F[A] =
          self.underlying.derive(new LiftF[F] {
            def tt[T](implicit t: TypeTag[T]): TypeTag[T] = t

            val map: Map[TypeTag[_], F[_]] =
              Map(tt[I1] -> I1, tt[I2] -> I2, tt[I3] -> I3, tt[I4] -> I4, tt[I5] -> I5)

            def get[T](implicit t: TypeTag[T]): F[T] =
              map(t).asInstanceOf[F[T]]
          }, c)
    }
  // implicit class case6implicits[A, I1: TypeTag, I2: TypeTag, I3: TypeTag, I4: TypeTag, I5: TypeTag, I6: TypeTag, T0]
}

trait DerivingF[A] {
  type TreeRepr
  type FlatRepr <: HList
  def underlying: DeriveF[A, HNil]
}

object DerivingF extends DerivingBoilerplate {
  type Aux[A, T0, F0 <: HList] = DerivingF[A] { type TreeRepr = T0; type FlatRepr = F0 }
  def apply[A] = new DerivingCurried[A]
}

class DerivingCurried[A] {
  def gen[T0, F0 <: HList]
    (implicit
      d: DeriveF.Aux[A, T0, HNil],
      l: Leaves.Aux[T0, F0]
    ): DerivingF.Aux[A, T0, F0] =
      new DerivingF[A] {
        type TreeRepr = T0
        type FlatRepr = F0
        val underlying: DeriveF[A, HNil] = d
      }
}
