import shapeless._

/** Flattens a tree of `HList` / `Coproduct` into a single `HList` contains all types at leave position. */
trait Leaves[Repr] {
  type FlatRepr <: HList
}

trait LeavesCandies {
  type Aux[Repr, R0 <: HL] = Leaves[Repr] { type FlatRepr = R0 }
  protected type HL = HList
  protected type CO = Coproduct
  protected def aux[Repr, R0 <: HL]: Aux[Repr, R0] = new Leaves[Repr] { type FlatRepr = R0 }
  // TODO instance/evidence
}

trait LeavesLowerPriority extends LeavesCandies {
  implicit def hCons[H, T <: HL, R <: HL](implicit t: Aux[T, R]): Aux[H ::  T, H :: R] = aux
  implicit def cCons[H, T <: CO, R <: HL](implicit t: Aux[T, R]): Aux[H :+: T, H :: R] = aux
}

trait LeavesLowPriority extends LeavesLowerPriority {
  implicit def hhCons[H, T <: HL, U <: HL, R <: HL](implicit h: Aux[H ::  (T ::  U), R]): Aux[(H ::  T) ::  U, R] = aux
  implicit def chCons[H, T <: CO, U <: HL, R <: HL](implicit h: Aux[H ::  (T ::  U), R]): Aux[(H :+: T) ::  U, R] = aux
  implicit def hcCons[H, T <: HL, U <: CO, R <: HL](implicit h: Aux[H :+: (T :+: U), R]): Aux[(H ::  T) :+: U, R] = aux
  implicit def ccCons[H, T <: CO, U <: CO, R <: HL](implicit h: Aux[H :+: (T :+: U), R]): Aux[(H :+: T) :+: U, R] = aux
}

object Leaves extends LeavesLowPriority {
  implicit def hNil: Aux[HNil, HNil] = aux
  implicit def cNil: Aux[CNil, HNil] = aux
  implicit def hhNil[L <: HL, R <: HL](implicit h: Aux[L, R]): Aux[HNil ::  L, R] = aux
  implicit def hcNil[L <: HL, R <: HL](implicit h: Aux[L, R]): Aux[CNil ::  L, R] = aux
  implicit def chNil[L <: CO, R <: HL](implicit h: Aux[L, R]): Aux[HNil :+: L, R] = aux
  implicit def ccNil[L <: CO, R <: HL](implicit h: Aux[L, R]): Aux[CNil :+: L, R] = aux
}

object LeavesTest {
  type TestTree = Int :: (Double :: (Boolean :+: Byte :+: CNil) :: HNil) :: String :: HNil
  type Expected = Int :: Double :: Boolean :: Byte :: String :: HNil

  val leafs = the[Leaves[TestTree]]
  implicitly[leafs.FlatRepr =:= Expected]
}
