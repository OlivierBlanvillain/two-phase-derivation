import shapeless._

/** Negation of `shapeless.Generic`, only exists when `A` does not have a `Generic` representation. */
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
  import Model._

  implicitly[NotGeneric[String]]
  implicitly[NotGeneric[String :: Int :: HNil]]
  illTyped("implicitly[NotGeneric[AA]]")
  illTyped("implicitly[NotGeneric[IDAABBS]]")
}
