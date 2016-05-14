package felis.catus

import cats.free.Inject
import felis.catus.lib.FreeObjects

trait BaseAlgebra[A]

object BaseAlgebra {
  case class IntAL       (label: String) extends BaseAlgebra[Int]
  case class StringAL    (label: String) extends BaseAlgebra[String]
  case class BooleanAL   (label: String) extends BaseAlgebra[Boolean]
  case class ShortAL     (label: String) extends BaseAlgebra[Short]
  case class LongAL      (label: String) extends BaseAlgebra[Long]
  case class FloatAL     (label: String) extends BaseAlgebra[Float]
  // case class BigDecimalAL(label: String) extends BaseAlgebra[BigDecimal]
  case class DoubleAL    (label: String) extends BaseAlgebra[Double]
  case class ObjectAL [A](label: String, value: FreeObjects[BaseAlgebra, A]) extends BaseAlgebra[A]
}

class BaseAlgebraDsl[F[_]](implicit I: Inject[BaseAlgebra, F]) {
  import BaseAlgebra._
  private def lift[A] = FreeObjects.lift[BaseAlgebra, A] _

  def int(label: String): FreeObjects[BaseAlgebra, Int] = lift(IntAL(label))
  def string(label: String): FreeObjects[BaseAlgebra, String] = lift(StringAL(label))
  def boolean(label: String): FreeObjects[BaseAlgebra, Boolean] = lift(BooleanAL(label))
  def short(label: String): FreeObjects[BaseAlgebra, Short] = lift(ShortAL(label))
  def long(label: String): FreeObjects[BaseAlgebra, Long] = lift(LongAL(label))
  def float(label: String): FreeObjects[BaseAlgebra, Float] = lift(FloatAL(label))
  // def bigd(label: String): FreeObjects[BaseAlgebra, BigDecimal] = lift(BigDecimalAL(label))
  def double(label: String): FreeObjects[BaseAlgebra, Double] = lift(DoubleAL(label))
  def obj[A](label: String)(value: FreeObjects[BaseAlgebra, A]): FreeObjects[BaseAlgebra, A] =
    lift(ObjectAL(label, value))
}
