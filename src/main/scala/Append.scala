import shapeless._

trait Append[P <: HList, S <: HList] extends DepFn2[P, S] with Serializable {
  type Out <: HList
  def split(o: Out): (P, S)
}

trait LowPriorityAppend {
  type Aux[P <: HList, S <: HList, Out0 <: HList] = Append[P, S] { type Out = Out0 }

  implicit def hlistAppend[PH, PT <: HList, S <: HList, AOut <: HList]
    (implicit a: Aux[PT, S, AOut]): Aux[PH :: PT, S, PH :: AOut] =
      new Append[PH :: PT, S] {
        type Out = PH :: AOut
        def apply(p: PH :: PT, s: S): Out = p.head :: a(p.tail, s)
        def split(o: Out): (PH :: PT, S) = {
          val ph = o.head
          val (pt, s) = a.split(o.tail)
          (ph :: pt, s)
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
