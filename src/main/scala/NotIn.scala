package deriving

/** Negation of `hlist.Selector`, only exists when `L` does not contain `U`. */
trait NotIn[L <: HList, U]

object NotIn {
  def apply[L <: HList, U](implicit n: NotIn[L, U]): NotIn[L, U] = n
  private val instance: NotIn[HNil, Any] = new NotIn[HNil, Any] {}
  private def evidence[L <: HList, U]: NotIn[L, U] = instance.asInstanceOf[NotIn[L, U]]

  implicit def notInAmbiguity1[H, T <: HList]: NotIn[H :: T, H] = ???
  implicit def notInAmbiguity2[H, T <: HList]: NotIn[H :: T, H] = ???
  implicit def notInHNil[H]: NotIn[HNil, H] = evidence
  implicit def notInHConsrecurse[H, T <: HList, U](implicit st: NotIn[T, U]): NotIn[H :: T, U] = evidence
}

/** Negation of `hlist.Selector`, only exists when `L` does not contain `U`. */
trait NotIn1[L[t] <: Prod[t], U[_]]

object NotIn1 {
  def apply[L[t] <: Prod[t], U[_]](implicit n: NotIn1[L, U]): NotIn1[L, U] = n
  private val instance: NotIn1[Prod.N, Id] = new NotIn1[Prod.N, Id] {}
  private def evidence[L[t] <: Prod[t], U[_]]: NotIn1[L, U] = instance.asInstanceOf[NotIn1[L, U]]

  implicit def notInAmbiguity1[H[_], T[t] <: Prod[t]]: NotIn1[H :*: T, H] = ???
  implicit def notInAmbiguity2[H[_], T[t] <: Prod[t]]: NotIn1[H :*: T, H] = ???
  implicit def notInHNil[H[_]]: NotIn1[Prod.N, H] = evidence
  implicit def notInHConsrecurse[H[_], T[t] <: Prod[t], U[_]](implicit st: NotIn1[T, U]): NotIn1[H :*: T, U] = evidence
}
