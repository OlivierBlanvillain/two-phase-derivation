package felis.catus

object Algebra {
  sealed trait BaseAlgebra[A]
  case class IntAL       (label: String) extends BaseAlgebra[Int]
  case class StringAL    (label: String) extends BaseAlgebra[String]
  case class BooleanAL   (label: String) extends BaseAlgebra[Boolean]
  case class ShortAL     (label: String) extends BaseAlgebra[Short]
  case class LongAL      (label: String) extends BaseAlgebra[Long]
  case class FloatAL     (label: String) extends BaseAlgebra[Float]
  case class BigDecimalAL(label: String) extends BaseAlgebra[BigDecimal]
  case class DoubleAL    (label: String) extends BaseAlgebra[Double]
  case class ObjectAL [A](label: String, value: FreeCatus[BaseAlgebra, A]) extends BaseAlgebra[A]
}

object Dsl {
  import Algebra._
  private def lift[A] = FreeCatus.lift[BaseAlgebra, A] _

  def int(label: String): FreeCatus[BaseAlgebra, Int] = lift(IntAL(label))
  def string(label: String): FreeCatus[BaseAlgebra, String] = lift(StringAL(label))
  def boolean(label: String): FreeCatus[BaseAlgebra, Boolean] = lift(BooleanAL(label))
  def short(label: String): FreeCatus[BaseAlgebra, Short] = lift(ShortAL(label))
  def long(label: String): FreeCatus[BaseAlgebra, Long] = lift(LongAL(label))
  def float(label: String): FreeCatus[BaseAlgebra, Float] = lift(FloatAL(label))
  def bigd(label: String): FreeCatus[BaseAlgebra, BigDecimal] = lift(BigDecimalAL(label))
  def double(label: String): FreeCatus[BaseAlgebra, Double] = lift(DoubleAL(label))
  def obj[A](label: String)(value: FreeCatus[BaseAlgebra, A]): FreeCatus[BaseAlgebra, A] =
    lift(ObjectAL(label, value))
}
