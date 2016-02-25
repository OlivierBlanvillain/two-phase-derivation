import shapeless._

case class Shadow[T]()

trait Shadowed[L] {
  type Out
  def instances: Out
}

object Shadowed {
  type Aux[L, Out0] = Shadowed[L] { type Out = Out0 }
  def apply[L](implicit s: Shadowed[L]): Shadowed[L] = s
  
  implicit val wrapHnil: Aux[HNil, HNil] =
    new Shadowed[HNil] {
      type Out = HNil
      def instances: Out = HNil
    }
  
  implicit def wrapHcons[H, T <: HList, O <: HList]
    (implicit w: Aux[T, O]): Aux[H :: T, Shadow[H] :: O] =
      new Shadowed[H :: T] {
        type Out = Shadow[H] :: O
        def instances: Out = Shadow[H]() :: w.instances
      }
  
  implicit val wrapCnil: Aux[CNil, CNil] =
    new Shadowed[CNil] {
      type Out = CNil
      def instances: Out = ???
    }
  
  // implicit def wrapCcons[H, T <: Coproduct, O <: Coproduct]
  //   (implicit w: Aux[T, O]): Aux[H :+: T, Shadow[H] :+: O] =
  //     new Shadowed[H :+: T] {
  //       type Out = Shadow[H] :+: O
  //         def instances: Out = Shadow[H]() :+: (w.instances)
  //     }
}
