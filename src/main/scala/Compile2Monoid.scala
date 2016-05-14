package felis.catus

import cats.arrow.{NaturalTransformation => ~>}
import cats.Monoid
import cats.std.all._
import felis.catus.lib.FreeObjects
import felis.catus.BaseAlgebra._
import felis.catus.ObjectsInstances.monoidObjects

object Compile2Monoid {
  def naturalTransformation[M[_]]: BaseAlgebra ~> Monoid =
    new ~>[BaseAlgebra, Monoid] {
      def apply[A](fa: BaseAlgebra[A]): Monoid[A] = fa match {
        case IntAL   (label: String) => implicitly
        case StringAL(label: String) => implicitly
        case ShortAL (label: String) => implicitly
        case LongAL  (label: String) => implicitly
        case FloatAL (label: String) => implicitly
        case DoubleAL(label: String) => implicitly

        case BooleanAL(label: String) =>
          new Monoid[Boolean] {
            val empty = false
            def combine(x: Boolean, y: Boolean) = x || y
          }

        // case BigDecimalAL(label: String) =>
        //   new Monoid[BigDecimal] {
        //     val empty = BigDecimal(0)
        //     def combine(x: BigDecimal, y: BigDecimal) = x + y
        //   }

        case ObjectAL(label: String, value: FreeObjects[BaseAlgebra, A]) =>
          value.foldMap(this)(monoidObjects)
      }
    }
}
