package deriving

import Syntax._

trait LiftM[F[_], Repr <: HList] {
  def instances: Repr
}

trait DeriveM[A] {
  /** A flat representation of the targeted type */
  type Repr <: HList

  def derive[F[_]](implicit l: LiftM[F, Repr], c: CanDerive[F]): F[A]
}

object DeriveM extends DeriveMBoilerplate {
  type Aux[A, R <: HList] = DeriveM[A] { type Repr = R }
}

trait DeriveMBoilerplate {
  // implicit class case1implicits[A, I1]
  implicit class case2implicits[A, I1, I2]
    (self: DeriveM.Aux[A, I1 :: I2 :: HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], c: CanDerive[F]): F[A] =
          self.derive(new LiftM[F, I1 :: I2 :: HNil] {
            def instances = ::(I1, ::(I2, HNil))
               .asInstanceOf[I1 :: I2 :: HNil]
          }, c)
    }
  // implicit class case3implicits[A, I1, I2, I3]
  // implicit class case4implicits[A, I1, I2, I3, I4]
  implicit class case5implicits[A, I1, I2, I3, I4, I5]
    (self: DeriveM.Aux[A, I1 :: I2 :: I3 :: I4 :: I5 :: HNil]) {
      def materialize[F[_]]
        (implicit I1: F[I1], I2: F[I2], I3: F[I3], I4: F[I4], I5: F[I5], c: CanDerive[F]): F[A] =
          self.derive(new LiftM[F, I1 :: I2 :: I3 :: I4 :: I5 :: HNil] {
            def instances = ::(I1, ::(I2, ::(I3, ::(I4, ::(I5, HNil)))))
               .asInstanceOf[I1 :: I2 :: I3 :: I4 :: I5 :: HNil]
          }, c)
    }
}

object DerivingM {
  def apply[A] = new DerivingMCurried[A]
}

class DerivingMCurried[A] {
  def gen[R <: HList](implicit d: DeriveM.Aux[A, R]): DeriveM.Aux[A, R] = d
}
