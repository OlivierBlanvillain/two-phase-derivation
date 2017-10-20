package deriving

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

// https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/ops/hlists.scala
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

// https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/ops/coproduct.scala
trait ToHList[L <: Coproduct] { type Out <: HList }

object ToHList {
  def apply[L <: Coproduct](implicit thl: ToHList[L]): Aux[L, thl.Out] = thl

  type Aux[L <: Coproduct, Out0 <: HList] = ToHList[L] { type Out = Out0 }

  implicit val cnilToHList: Aux[CNil, HNil] =
    new ToHList[CNil] {
      type Out = HNil
    }

  implicit def cconsToHList[H, T <: Coproduct](implicit ut: ToHList[T]): Aux[H :+: T, H :: ut.Out] =
    new ToHList[H :+: T] {
      type Out = H :: ut.Out
    }
}

// https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/typeoperators.scala
// In Dotty we can remove the <: AnyRef, making this effectifely equivalant to the shapeless macro!
object the {
  def apply[A <: AnyRef](implicit a: A): a.type = a
}

/** Custom version of `shapeless.ops.hlist.Prepend` extended with a `split`
 *  method to reverse the appending.
 */
trait Append[P <: HList, S <: HList] {
  type Out <: HList
  def apply(p: P, s: S): Out
  def split(o: Out): (P, S)
}

trait LowPriorityAppend {
  type Aux[P <: HList, S <: HList, Out0 <: HList] = Append[P, S] { type Out = Out0 }

  implicit def hlistAppend[PH, PT <: HList, S <: HList, AOut <: HList]
    (implicit a: Aux[PT, S, AOut]): Aux[PH :: PT, S, PH :: AOut] =
      new Append[PH :: PT, S] {
        type Out = PH :: AOut
        def apply(p: PH :: PT, s: S): Out = ::(p.head, a(p.tail, s))
        def split(o: Out): (PH :: PT, S) = {
          val ph = o.head
          val (pt, s) = a.split(o.tail)
          (::(ph, pt), s)
        }
      }
}

object Append extends LowPriorityAppend {
  implicit def hnilAppend[S <: HList]: Aux[HNil, S, S] =
    new Append[HNil, S] {
      type Out = S
      def apply(p: HNil, s: S): S = s
      def split(o: Out): (HNil, S) = (HNil, o)
    }
}
