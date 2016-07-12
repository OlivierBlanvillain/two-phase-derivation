import shapeless._

trait NotGeneric[A]

trait NotGenericLowPrio {
  def apply[A](implicit n: NotGeneric[A]): NotGeneric[A] = n
  private def instance: NotGeneric[Any] = new NotGeneric[Any] {}

  implicit def notGeneric[A]: NotGeneric[A] = instance.asInstanceOf[NotGeneric[A]]
}

object NotGeneric extends NotGenericLowPrio {
  implicit def notGenericAmbiguity1[A: Generic]: NotGeneric[A] = unexpected
  implicit def notGenericAmbiguity2[A: Generic]: NotGeneric[A] = unexpected
}

object NotGenericTest {
  import shapeless.test.illTyped
  case class MYCC1(a: String, i: Int)

  sealed trait MYST1
  case object MYST1A extends MYST1
  case object MYST1B extends MYST1

  implicitly[NotGeneric[String]]
  implicitly[NotGeneric[String :: Int :: HNil]]
  illTyped("implicitly[NotGeneric[MYCC1]]")
  illTyped("implicitly[NotGeneric[MYST1]]")
}
