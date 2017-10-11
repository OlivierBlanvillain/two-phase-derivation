import shapeless._
import shapeless.ops.coproduct.ToHList
import cats.Later

import cats.syntax.invariant._
import cats.syntax.cartesian._

// ---------------------------------------------------------------------------------------
// Compiles 50x slower than `DeriveF`, uses type level computations instead of `TypeTag`s.
// ---------------------------------------------------------------------------------------

/** `shapeless.ops.hlist.Prepend` extended with a `split` method to reverse the appending. */
trait Append[P <: HList, S <: HList] extends DepFn2[P, S] with Serializable {
  type Out <: HList
  def split(o: Out): (P, S)
}

trait LowPriorityAppend {
  type Aux[P <: HList, S <: HList, Out0 <: HList] = Append[P, S] { type Out = Out0 }

  implicit def hlistAppend[PH, PT <: HList, S <: HList, AOut <: HList]
    (implicit a: Aux[PT, S, AOut]): Aux[PH :: PT, S, PH :: AOut] =
      new Append[PH :: PT, S] {
        type Out = PH :: AOut
        def apply(p: PH :: PT, s: S): Out = p.head :: a(p.tail, s)
        def split(o: Out): (PH :: PT, S) = {
          val ph = o.head
          val (pt, s) = a.split(o.tail)
          (ph :: pt, s)
        }
      }
}

object Append extends LowPriorityAppend {
  implicit def hnilAppend[S <: HList]: Aux[HNil, S, S] =
    new Append[HNil, S] {
      type Out = S
      def apply(p: HNil, s: S): S = s
      def split(o: Out): (HNil, S) = (HNil, o)
    }
}

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
    def leftSide [L <: HList, R <: HList](implicit a: Append.Aux[L, R, H]): LiftS[F, L] =
      new LiftS[F, L] { val instances: L = a.split(self.instances)._1 }

    def rightSide[L <: HList, R <: HList](implicit a: Append.Aux[L, R, H]): LiftS[F, R] =
      new LiftS[F, R] { val instances: R = a.split(self.instances)._2 }
  }

  implicit class LiftGet[F[_], A](self: LiftS[F, A :: HNil]) {
    def get: F[A] = self.instances.head.asInstanceOf[F[A]]
  }
}

/** First phase of automatic type class derivation for `A`. */
trait DeriveS[A] {
  type Repr <: HList

  def derive[F[_]](implicit l: LiftS[F, Repr], c: CanDerive[F]): F[A]
}

/** Second phase of automatic type class derivation for `F[A]`. */
trait DeriveSBoilerplate {
  // implicit class case1implicits[A, I1]
  // implicit class case2implicits[A, I1, I2]
  // implicit class case3implicits[A, I1, I2, I3]
  // implicit class case4implicits[A, I1, I2, I3, I4]
  implicit class case5implicits[A, I1, I2, I3, I4, I5]
    (self: DeriveS.Aux[A, I1 :: I2 :: I3 :: I4 :: I5 :: HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], I3: F[I3], I4: F[I4], I5: F[I5], c: CanDerive[F]): F[A] =
          self.derive(new LiftS[F, I1 :: I2 :: I3 :: I4 :: I5 :: HNil] {
            def instances = (I1 :: I2 :: I3 :: I4 :: I5 :: HNil)
               .asInstanceOf[I1 :: I2 :: I3 :: I4 :: I5 :: HNil]
          }, c)
    }
}

trait LowPrioDeriveS {
  type Aux[A, R0 <: HList] = DeriveS[A] { type Repr = R0 }

  // h: NotGeneric[A] → DeriveS[A] { type Repr = A :: HNil }
  implicit def caseNoGeneric[A](implicit h: NotGeneric[A]): Aux[A, A :: HNil] =
    new DeriveS[A] {
      type Repr = A :: HNil
      def derive[F[_]](implicit l: LiftS[F, Repr], c: CanDerive[F]): F[A] = l.get
    }
}

object DeriveS extends DeriveSBoilerplate with LowPrioDeriveS {
  // g: Generic[A], r: DeriveS[g.Repr] → DeriveS[A] { type Repr = g.Repr }
  // when g.Repr: HList
  implicit def caseGenericProduct[A, G <: HList, R <: HList]
    (implicit
      g: Generic.Aux[A, G],
      r: Lazy[Aux[G, R]]
    ): Aux[A, R] = new DeriveS[A] {
      type Repr = R
      def derive[F[_]] (implicit l: LiftS[F, Repr], c: CanDerive[F]): F[A] =
        r.value.derive[F].imap(g.from)(g.to)
    }

  // g: Generic[A], r: DeriveS[g.Repr] → DeriveS[A] { type Repr = g.Repr.toHList }
  // when g.Repr: Coproduct
  implicit def caseGenCoproduct[A, G <: Coproduct, GL <: HList, R <: HList]
    (implicit
      g: Generic.Aux[A, G],
      t: ToHList.Aux[G, GL],
      r: Lazy[Aux[G, R]]
    ): Aux[A, R] = new DeriveS[A] {
      type Repr = R
      def derive[F[_]] (implicit l: LiftS[F, Repr], c: CanDerive[F]): F[A] =
        r.value.derive[F].imap(g.from)(g.to)
    }

  // r: DeriveS[H] → DeriveS[H :: HNil] { type Repr = r.Repr }
  implicit def caseHLast[H, R <: HList](implicit r: Lazy[Aux[H, R]]): Aux[H :: HNil, R] =
    new DeriveS[H :: HNil] {
      type Repr = R
      def derive[F[_]](implicit l: LiftS[F, Repr], c: CanDerive[F]): F[H :: HNil] =
        r.value.derive[F].imap { a => a :: HNil } { case a :: HNil => a }
    }

  // → DeriveS[CNil] { type Repr = HNil }
  // Note that this case is impossible as they are not value of type `CNil`.
  implicit def caseCNil: Aux[CNil, HNil] = new DeriveS[CNil] {
    type Repr = HNil
    def derive[F[_]](implicit l: LiftS[F, Repr], c: CanDerive[F]): F[CNil] = unexpected
  }

  // h: DeriveS[H], t: DeriveS[T] → DeriveS[H :: T] { type Repr = h.Repr ++ t.Repr }
  implicit def caseHCons[H, HR <: HList, T <: HList, TR <: HList, LR <: HList]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]],
      p: Append.Aux[HR, TR, LR]
    ): Aux[H :: T, LR] =
      new DeriveS[H :: T] {
        type Repr = LR
        def derive[F[_]](implicit l: LiftS[F, Repr], c: CanDerive[F]): F[H :: T] =
          h.derive(l.leftSide[HR, TR], c).product(t.value.derive[F](l.rightSide[HR, TR], c))
            .imap { case (a, b) => a :: b } { case a :: b => (a, b) }
      }

  // h: DeriveS[H], t: DeriveS[T] → DeriveS[H :+: T] { type Repr = h.Repr ++ t.Repr }
  implicit def caseCCons[H, HR <: HList, T <: Coproduct, TR <: HList, LR <: HList]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]],
      p: Append.Aux[HR, TR, LR]
    ): Aux[H :+: T, LR] =
      new DeriveS[H :+: T] {
        type Repr = LR
        def derive[F[_]] (implicit l: LiftS[F, Repr], c: CanDerive[F]): F[H :+: T] =
          CanDerive[F].coproduct(Later(h.derive(l.leftSide[HR, TR], c)), Later(t.value.derive[F](l.rightSide[HR, TR], c)))
            .imap {
              case Left (a) => Inl(a)
              case Right(b) => Inr(b)
            } {
              case Inl(a) => Left (a)
              case Inr(b) => Right(b)
            }
      }
}

// // Compiles in 16 seconds...
// object DeriveSTest {
//   import Model._
//   import shapeless.test.illTyped
//   import cats.Show

//   val deriveS = the[DeriveS[IDAABBS]]
//   type Expected = Int :: Double :: String :: String :: String :: HNil
//   implicitly[deriveS.Repr =:= Expected]

//   illTyped(
//     "deriveS.materialize[Show]",
//     "could not find implicit value for parameter I1: cats.Show\\[Int\\].*")

//   import cats.std.all._
//   val showIDAABBS: Show[IDAABBS] = deriveS.materialize[Show]
//   assert(showIDAABBS.show(instance) == showResult)
// }
