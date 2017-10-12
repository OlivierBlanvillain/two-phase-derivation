package deriving

import shapeless._

/** Negation of `shapeless.ops.hlist.Selector`, only exists when `L` does not contain `U`. */
trait NotIn[L <: HList, U]

object NotIn {
  def apply[L <: HList, U](implicit n: NotIn[L, U]): NotIn[L, U] = n
  private val instance: NotIn[HNil, Any] = new NotIn[HNil, Any] {}
  private def evidence[L <: HList, U]: NotIn[L, U] = instance.asInstanceOf[NotIn[L, U]]

  implicit def notInAmbiguity1[H, T <: HList]: NotIn[H :: T, H] = unexpected
  implicit def notInAmbiguity2[H, T <: HList]: NotIn[H :: T, H] = unexpected
  implicit def notInHNil[H]: NotIn[HNil, H] = evidence
  implicit def notInHConsrecurse[H, T <: HList, U](implicit st : NotIn[T, U]): NotIn[H :: T, U] = evidence
}
