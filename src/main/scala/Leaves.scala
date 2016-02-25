import shapeless._

trait Leaves[L] {
  type Out <: HList
}

trait LeavesCandies {
  type Aux[L, R0 <: HList] = Leaves[L] { type Out = R0 }
  protected type Copro = Coproduct
  protected def aux[L, R0 <: HList]: Aux[L, R0] = new Leaves[L] { type Out = R0 }
}

trait LeavesLowerPriority extends LeavesCandies {
  implicit def hCons[H, T <: HList, R <: HList](implicit t: Aux[T, R]): Aux[H ::  T, H :: R] = aux
  implicit def cCons[H, T <: Copro, R <: HList](implicit t: Aux[T, R]): Aux[H :+: T, H :: R] = aux
}

trait LeavesLowPriority extends LeavesLowerPriority {
  implicit def hhCons[H, T <: HList, U <: HList, R <: HList](implicit h: Aux[H ::  (T ::  U), R]): Aux[(H ::  T) ::  U, R] = aux
  implicit def chCons[H, T <: Copro, U <: HList, R <: HList](implicit h: Aux[H ::  (T ::  U), R]): Aux[(H :+: T) ::  U, R] = aux
  implicit def hcCons[H, T <: HList, U <: Copro, R <: HList](implicit h: Aux[H :+: (T :+: U), R]): Aux[(H ::  T) :+: U, R] = aux
  implicit def ccCons[H, T <: Copro, U <: Copro, R <: HList](implicit h: Aux[H :+: (T :+: U), R]): Aux[(H :+: T) :+: U, R] = aux
}

object Leaves extends LeavesLowPriority {
  implicit def hNil: Aux[HNil, HNil] = aux
  implicit def cNil: Aux[CNil, HNil] = aux
  implicit def hhNil[L <: HList, R <: HList](implicit h: Aux[L, R]): Aux[HNil ::  L, R] = aux
  implicit def hcNil[L <: HList, R <: HList](implicit h: Aux[L, R]): Aux[CNil ::  L, R] = aux
  implicit def chNil[L <: Copro, R <: HList](implicit h: Aux[L, R]): Aux[HNil :+: L, R] = aux
  implicit def ccNil[L <: Copro, R <: HList](implicit h: Aux[L, R]): Aux[CNil :+: L, R] = aux
}

object LeavesTest {
  type TestTree = Int :: (Double :: (Boolean :+: Byte :+: CNil) :: HNil) :: String :: HNil
  type Expected = Int :: Double :: Boolean :: Byte :: String :: HNil
  
  val leafs = the[Leaves[TestTree]]
  implicitly[leafs.Out =:= Expected]
}
