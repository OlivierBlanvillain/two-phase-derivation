package felis.catus

import cats.arrow.{NaturalTransformation => ~>}
import cats.Show
import felis.catus.Algebra._
import felis.catus.CatusInstances.showCatus

object Compile2Show {
  private def mkShow[A](f: A => String): Show[A] =
    new Show[A] {
      def show(x: A): String = f(x)
    }

  def naturalTransformation[M[_]]: BaseAlgebra ~> Show =
    new ~>[BaseAlgebra, Show] {
      def apply[A](fa: BaseAlgebra[A]): Show[A] = fa match {
        case IntAL       (label: String) => mkShow(s"$label = ".+)
        case StringAL    (label: String) => mkShow(s"$label = ".+)
        case ShortAL     (label: String) => mkShow(s"$label = ".+)
        case LongAL      (label: String) => mkShow(s"$label = ".+)
        case FloatAL     (label: String) => mkShow(s"$label = ".+)
        case DoubleAL    (label: String) => mkShow(s"$label = ".+)
        case BooleanAL   (label: String) => mkShow(s"$label = ".+)
        case BigDecimalAL(label: String) => mkShow(s"$label = ".+)

        case ObjectAL(label: String, value: FreeCatus[BaseAlgebra, A]) =>
          mkShow(x => s"$label = { " + value.foldMap(this).show(x) + " }")
      }
    }
}
