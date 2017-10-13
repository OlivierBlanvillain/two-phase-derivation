package deriving

/** Negation of `Generic`, only exists when `A` does not have a `Generic` representation. */
trait NotGeneric[A]

trait NotGenericLowPrio {
  def apply[A](implicit n: NotGeneric[A]): NotGeneric[A] = n
  private val instance: NotGeneric[Any] = new NotGeneric[Any] {}
  implicit def notGeneric[A]: NotGeneric[A] = instance.asInstanceOf[NotGeneric[A]]
}

object NotGeneric extends NotGenericLowPrio {
  implicit def notGenericAmbiguity1[A: Generic]: NotGeneric[A] = ???
  implicit def notGenericAmbiguity2[A: Generic]: NotGeneric[A] = ???
}
