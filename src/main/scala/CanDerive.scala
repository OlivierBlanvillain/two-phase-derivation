package deriving

/** All you need for automatic type class derivation of `F[_]`. */
trait CanDerive[F[_]] extends Invariant[F] with Cartesian[F] with Cocartesian[F]

object CanDerive {
  def apply[F[_]](implicit f: CanDerive[F]): CanDerive[F] = f

  implicit val canDeriveShow: CanDerive[Show] = new CanDerive[Show] {
    val unit: Show[Unit] = new Show[Unit] { def show(u: Unit): String = "+" }

    def product[A, B](fa: Show[A], fb: Show[B]): Show[(A, B)] = new Show[(A, B)] {
      def show(ab: (A, B)): String = "(" + fa.show(ab._1) + ", " + fb.show(ab._2) + ")"
    }

    def imap[A, B](fa: Show[A])(f: A => B)(g: B => A): Show[B] = new Show[B] {
      def show(b: B): String = fa.show(g(b))
    }

    def coproduct[A, B](fa: => Show[A], fb: => Show[B]): Show[Either[A, B]] = new Show[Either[A, B]] {
      def show(ab: Either[A, B]): String = "[case: " + (ab match {
        case Left (a) => fa.show(a)
        case Right(b) => fb.show(b)
      }) + "]"
    }
  }
}
