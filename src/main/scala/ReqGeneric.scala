import shapeless._

trait ReqGeneric[T] {
  type Repr
}

object ReqGeneric {
  type Aux[T, R0] = ReqGeneric[T] { type Repr = R0 }
  protected def aux[T, R0] = new ReqGeneric[T] { type Repr = R0 }
  protected type Copro = Coproduct
  
  implicit def hbase[T](implicit h: HasNoGeneric[T]): Aux[T, T] = aux
  implicit def hnil: Aux[HNil, HNil] = aux
  implicit def cnil: Aux[CNil, CNil] = aux
  implicit def gen[A, G, R](implicit g: Generic.Aux[A, G], r: Lazy[Aux[G, R]]): Aux[A, R] = aux
  implicit def hcons[H, HR, T <: HList, TR <: HList](implicit h: Aux[H, HR], t: Lazy[Aux[T, TR]]): Aux[H ::  T, HR ::  TR] = aux
  implicit def ccons[H, HR, T <: Copro, TR <: Copro](implicit h: Aux[H, HR], t: Lazy[Aux[T, TR]]): Aux[H :+: T, HR :+: TR] = aux
}

object ReqGenericTest {
  sealed trait AABB
  case class AA(a: String) extends AABB
  case class BB(a: String) extends AABB
  case class DAABB(d: Double, aabb: AABB)
  case class IDAABBS(i: Int, daabb: DAABB, s: String)

  object TheTest {
    val rg = the[ReqGeneric[IDAABBS]]
    type Expected = Int :: (Double :: ((String :: HNil) :+: (String :: HNil) :+: CNil) :: HNil) :: String :: HNil
    implicitly[rg.Repr =:= Expected]
  }
}

