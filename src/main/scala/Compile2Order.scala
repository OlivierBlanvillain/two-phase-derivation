package felis.catus

import cats.arrow.{NaturalTransformation => ~>}
import cats.Order
import cats.std.all._
import felis.catus.lib.FreeObjects
import felis.catus.BaseAlgebra._
import felis.catus.ObjectsInstances.orderObjects

object Compile2Order {
  def naturalTransformation[M[_]]: BaseAlgebra ~> Order =
    new ~>[BaseAlgebra, Order] {
      def apply[A](fa: BaseAlgebra[A]): Order[A] = fa match {
        case IntAL    (label: String) => implicitly
        case StringAL (label: String) => implicitly
        case ShortAL  (label: String) => implicitly
        case LongAL   (label: String) => implicitly
        case FloatAL  (label: String) => implicitly
        case DoubleAL (label: String) => implicitly
        case BooleanAL(label: String) => implicitly

        // case BigDecimalAL(label: String) =>
        //   new Order[BigDecimal] {
        //     def compare(x: BigDecimal, y: BigDecimal) = x.compare(y)
        //   }

        case ObjectAL(label: String, value: FreeObjects[BaseAlgebra, A]) =>
          value.foldMap(this)(orderObjects)
      }
    }
}
