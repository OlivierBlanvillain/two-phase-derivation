package deriving

/** Flattens a tree of `HList` / `Coproduct` into a single `HList` contains all types at leave position. */
trait Leaves[Repr] {
  type FlatRepr <: HList
}

trait LeavesCandies {
  type Aux[Repr, R0 <: HL] = Leaves[Repr] { type FlatRepr = R0 }
  protected type HL = HList
  protected type CO = Coproduct
  protected val instance: Aux[Any, HNil] = new Leaves[Any] { type FlatRepr = HNil }
  protected def ev[Repr, R0 <: HL]: Aux[Repr, R0] = instance.asInstanceOf[Aux[Repr, R0]]
}

trait LeavesLowerPriority extends LeavesCandies {
  implicit def hCons[H, T <: HL, R <: HL](implicit t: Aux[T, R]): Aux[H ::  T, H :: R] = ev
  implicit def cCons[H, T <: CO, R <: HL](implicit t: Aux[T, R]): Aux[H :+: T, H :: R] = ev
}

trait LeavesLowPriority extends LeavesLowerPriority {
  implicit def hhCons[H, T <: HL, U <: HL, R <: HL](implicit h: Aux[H ::  (T ::  U), R]): Aux[(H ::  T) ::  U, R] = ev
  implicit def chCons[H, T <: CO, U <: HL, R <: HL](implicit h: Aux[H ::  (T ::  U), R]): Aux[(H :+: T) ::  U, R] = ev
  implicit def hcCons[H, T <: HL, U <: CO, R <: HL](implicit h: Aux[H :+: (T :+: U), R]): Aux[(H ::  T) :+: U, R] = ev
  implicit def ccCons[H, T <: CO, U <: CO, R <: HL](implicit h: Aux[H :+: (T :+: U), R]): Aux[(H :+: T) :+: U, R] = ev
}

object Leaves extends LeavesLowPriority {
  implicit def hNil: Aux[HNil, HNil] = ev
  implicit def cNil: Aux[CNil, HNil] = ev
  implicit def hhNil[L <: HL, R <: HL](implicit h: Aux[L, R]): Aux[HNil ::  L, R] = ev
  implicit def hcNil[L <: HL, R <: HL](implicit h: Aux[L, R]): Aux[CNil ::  L, R] = ev
  implicit def chNil[L <: CO, R <: HL](implicit h: Aux[L, R]): Aux[HNil :+: L, R] = ev
  implicit def ccNil[L <: CO, R <: HL](implicit h: Aux[L, R]): Aux[CNil :+: L, R] = ev
}
