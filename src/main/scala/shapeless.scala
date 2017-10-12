package deriving

// import shapeless._
import annotation.implicitNotFound

// https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/hlists.scala
sealed trait HList extends Product with Serializable
final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
sealed trait HNil extends HList
final case object HNil extends HNil

// https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/coproduct.scala
sealed trait Coproduct extends Product with Serializable
sealed trait :+:[+H, +T <: Coproduct] extends Coproduct
final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T]
final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T]
sealed trait CNil extends Coproduct

// https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/generic.scala
trait Generic[T] {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}

object Generic {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }
}

// https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/ops/hlists.scala
/**
 * Type class supporting access to the first element of this `HList` of type `U`. Available only if this `HList`
 * contains an element of type `U`.
 *
 * @author Miles Sabin
 */
@implicitNotFound("Implicit not found: shapeless.Ops.Selector[${L}, ${U}]. You requested an element of type ${U}, but there is none in the HList ${L}.")
trait Selector[L <: HList, U] {
  type Out = U
  def apply(l: L): U
}

object Selector {

  def apply[L <: HList, U](implicit selector: Selector[L, U]): Selector[L, U] = selector

  implicit def select[H, T <: HList]: Selector[H :: T, H] =
    new Selector[H :: T, H] {
      def apply(l: H :: T) = l.head
    }

  implicit def recurse[H, T <: HList, U]
    (implicit st : Selector[T, U]): Selector[H :: T, U] =
      new Selector[H :: T, U] {
        def apply(l: H :: T) = st(l.tail)
      }
}
