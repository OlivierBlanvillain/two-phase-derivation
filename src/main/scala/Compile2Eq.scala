package felis.catus

import cats.arrow.{NaturalTransformation => ~>}
import cats.Eq
import cats.std.all._
import felis.catus.Algebra._
import felis.catus.CatusInstances.eqCatus

object Compile2Eq {
  def naturalTransformation[M[_]]: BaseAlgebra ~> Eq =
    new ~>[BaseAlgebra, Eq] {
      def apply[A](fa: BaseAlgebra[A]): Eq[A] = fa match {
        case IntAL    (label: String) => implicitly
        case StringAL (label: String) => implicitly
        case ShortAL  (label: String) => implicitly
        case LongAL   (label: String) => implicitly
        case FloatAL  (label: String) => implicitly
        case DoubleAL (label: String) => implicitly
        case BooleanAL(label: String) => implicitly

        case BigDecimalAL(label: String) =>
          new Eq[BigDecimal] {
            def eqv(x: BigDecimal, y: BigDecimal): Boolean = x == y
          }

        case ObjectAL(label: String, value: FreeCatus[BaseAlgebra, A]) =>
          value.foldMap(this)
      }
    }
}
