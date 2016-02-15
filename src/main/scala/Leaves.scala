import shapeless.{HList, Coproduct, ::, :+:, HNil, CNil}

trait Leaves[L] {
  type Repr <: HList
}

trait LeavesLowerPriority {
  type Copro = Coproduct // For source code alignment...
  type Aux[L, R0 <: HList] = Leaves[L] { type Repr = R0 }
  val empty = new Leaves[HNil] { type Repr = HNil }
  def aux[L, R0 <: HList] = empty.asInstanceOf
  
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

// object LeavesTest {
//   type TestTree = Int :: (Double :: (Boolean :+: Byte :+: CNil) :: HNil) :: String :: HNil
//   type Expected = Int :: Double :: Boolean :: Byte :: String :: HNil
  
//   val leafs = the[Leaves[TestTree]]
//   implicitly[leafs.Repr =:= Expected]
// }
