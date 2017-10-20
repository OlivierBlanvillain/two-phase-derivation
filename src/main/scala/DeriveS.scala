package deriving

import Syntax._

trait LiftS[F[_], Repr <: HList] {
  // There is a trick here which greatly simplifies the implementation. The
  // type of instances is actually each element of `Repr` lifted with `F[_]`.
  // In order keep the two lists aligned (which is particularely usefull when
  // splitting `instances` using `Append[L, R, Repr]`), this implementation
  // uses `.asInstanceOf[F[A]]` when putting/pulling stuff from `instances`.
  def instances: Repr
}

object LiftS {
  implicit class LiftSplit[F[_], H <: HList](self: LiftS[F, H]) {
    def leftSide[L <: HList, R <: HList](implicit a: Append.Aux[L, R, H]): LiftS[F, L] =
      new LiftS[F, L] { val instances: L = a.split(self.instances)._1 }

    def rightSide[L <: HList, R <: HList](implicit a: Append.Aux[L, R, H]): LiftS[F, R] =
      new LiftS[F, R] { val instances: R = a.split(self.instances)._2 }
  }

  implicit class LiftGet[F[_], A](self: LiftS[F, A :: HNil]) {
    def get: F[A] = self.instances.head.asInstanceOf[F[A]]
  }

  def empty[F[_]]: LiftS[F, HNil] = new LiftS[F, HNil] { val instances = HNil }
}

/** First phase of automatic type class derivation for `A`.
 *
 * @tparam A     The targeted type
 * @tparam Seen  The Generic types seens so far during derivation
 */
trait DeriveS[A, Seen <: HList] {
  /** A flat representation of the targeted type */
  type Repr <: HList

  def derive[F[_]](implicit l: LiftS[F, Repr], s: LiftS[F, Seen], c: CanDerive[F]): F[A]
}

/** Second phase of automatic type class derivation for `F[A]`. */
trait DeriveSBoilerplate {
  // implicit class case1implicits[A, I1]
  implicit class case2implicits[A, I1, I2]
    (self: DeriveS.Aux[A, I1 :: I2 :: HNil, HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], c: CanDerive[F]): F[A] =
          self.derive(new LiftS[F, I1 :: I2 :: HNil] {
            def instances = ::(I1, ::(I2, HNil))
               .asInstanceOf[I1 :: I2 :: HNil]
          }, LiftS.empty, c)
    }
  // implicit class case3implicits[A, I1, I2, I3]
  // implicit class case4implicits[A, I1, I2, I3, I4]
  implicit class case5implicits[A, I1, I2, I3, I4, I5]
    (self: DeriveS.Aux[A, I1 :: I2 :: I3 :: I4 :: I5 :: HNil, HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], I3: F[I3], I4: F[I4], I5: F[I5], c: CanDerive[F]): F[A] =
          self.derive(new LiftS[F, I1 :: I2 :: I3 :: I4 :: I5 :: HNil] {
            def instances = ::(I1, ::(I2, ::(I3, ::(I4, ::(I5, HNil)))))
               .asInstanceOf[I1 :: I2 :: I3 :: I4 :: I5 :: HNil]
          }, LiftS.empty, c)
    }
}

trait LowerPriorityDeriveS {
  type Aux[A, R <: HList, S <: HList] = DeriveS[A, S] { type Repr = R }

  implicit def caseNotGeneric[A, S <: HList](implicit h: NotGeneric[A]): DeriveS.Aux[A, A :: HNil, S] =
    new DeriveS[A, S] {
      type Repr = A :: HNil
      def derive[F[_]](implicit l: LiftS[F, A :: HNil], s: LiftS[F, S], c: CanDerive[F]): F[A] = l.get
    }
}

trait LowPriorityDeriveS extends LowerPriorityDeriveS {
  implicit def caseHCons[H, HR <: HList, T <: HList, TR <: HList, LR <: HList, S <: HList]
    (implicit
      t: => DeriveS.Aux[T, TR, S],
      h: => DeriveS.Aux[H, HR, S],
      p: Append.Aux[HR, TR, LR]
    ): DeriveS.Aux[H :: T, LR, S] =
      new DeriveS[H :: T, S] {
        type Repr = LR
        def derive[F[_]](implicit l: LiftS[F, LR], s: LiftS[F, S], c: CanDerive[F]): F[H :: T] =
          h.derive(l.leftSide[HR, TR], s, c).product(t.derive[F](l.rightSide[HR, TR], s, c))
            .imap { case (a, b) => ::(a, b) } { case ::(a, b) => (a, b) }
      }

  implicit def caseCCons[H, HR <: HList, T <: Coproduct, TR <: HList, LR <: HList, S <: HList]
    (implicit
      t: => DeriveS.Aux[T, TR, S],
      h: => DeriveS.Aux[H, HR, S],
      p: Append.Aux[HR, TR, LR]
    ): DeriveS.Aux[H :+: T, LR, S] =
      new DeriveS[H :+: T, S] {
        type Repr = LR
        def derive[F[_]](implicit l: LiftS[F, LR], s: LiftS[F, S], c: CanDerive[F]): F[H :+: T] =
          CanDerive[F].coproduct(h.derive(l.leftSide[HR, TR], s, c), t.derive[F](l.rightSide[HR, TR], s, c))
            .imap {
              case Left (a) => Inl(a)
              case Right(b) => Inr(b)
            } {
              case Inl(a) => Left (a)
              case Inr(b) => Right(b)
            }
      }
}

object DeriveS extends DeriveSBoilerplate with LowPriorityDeriveS {
  implicit def caseGeneric[A, G, R <: HList, S <: HList]
    (implicit
      n: NotIn[S, A],
      g: Generic[A] { type Repr = G },
      r: => DeriveS.Aux[G, R, A :: S]
    ): DeriveS.Aux[A, R, S] = new DeriveS[A, S] {
      type Repr = R
      def derive[F[_]](implicit l: LiftS[F, R], s: LiftS[F, S], c: CanDerive[F]): F[A] = {
        var fa: F[A] = null.asInstanceOf[F[A]]
        val seen = new LiftS[F, A :: S] { def instances = ::(fa.asInstanceOf[A], s.instances) }
        fa = r.derive[F](l, seen, c).imap(g.from)(g.to)
        fa
      }
    }

  implicit def caseMemoizedGeneric[A, S <: HList]
    (implicit i: Selector[S, A]): DeriveS.Aux[A, HNil, S] = new DeriveS[A, S] {
      type Repr = HNil
      def derive[F[_]](implicit l: LiftS[F, HNil], s: LiftS[F, S], c: CanDerive[F]): F[A] =
        i(s.instances).asInstanceOf[F[A]]
    }

  implicit def caseHNil[S <: HList]: DeriveS.Aux[HNil, HNil, S] =
    new DeriveS[HNil, S] {
      type Repr = HNil
      def derive[F[_]](implicit l: LiftS[F, HNil], s: LiftS[F, S], c: CanDerive[F]): F[HNil] =
        CanDerive[F].unit.imap[HNil](_ => HNil)(_ => ())
    }

  implicit def caseCNil[S <: HList]: DeriveS.Aux[CNil, HNil, S] = new DeriveS[CNil, S] {
    type Repr = HNil
    def derive[F[_]](implicit l: LiftS[F, HNil], s: LiftS[F, S], c: CanDerive[F]): F[CNil] = ???
  }
}

object DerivingS {
  def apply[A] = new DerivingSCurried[A]
}

class DerivingSCurried[A] {
  def gen[R <: HList](implicit d: DeriveS.Aux[A, R, HNil]): DeriveS.Aux[A, R, HNil] = d
}
