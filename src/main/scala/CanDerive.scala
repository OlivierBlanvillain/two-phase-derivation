import cats.{Cartesian, Show}
import cats.functor.Invariant
import cats.Eval

/** Lazy `Either` version of `Cartesian`. */
trait DisjointCartesian[F[_]] {
  def coproduct[A, B](fa: Eval[F[A]], fb: Eval[F[B]]): F[Either[A, B]]
}

/** All you need for automatic type class derivation of `F[_]`. */
trait CanDerive[F[_]] extends Invariant[F] with Cartesian[F] with DisjointCartesian[F]

object CanDerive {
  def apply[F[_]](implicit f: CanDerive[F]): CanDerive[F] = f

  implicit val canDeriveShow: CanDerive[Show] = new CanDerive[Show] {
    def product[A, B](fa: Show[A], fb: Show[B]): Show[(A, B)] = new Show[(A, B)] {
      def show(ab: (A, B)): String = "(" + fa.show(ab._1) + ", " + fb.show(ab._2) + ")"
    }

    def imap[A, B](fa: Show[A])(f: A => B)(g: B => A): Show[B] = new Show[B] {
      def show(b: B): String = fa.show(g(b))
    }

    def coproduct[A, B](fa: Eval[Show[A]], fb: Eval[Show[B]]): Show[Either[A, B]] = new Show[Either[A, B]] {
      def show(ab: Either[A, B]): String = "[case: " + (ab match {
        case Left (a) => fa.value.show(a)
        case Right(b) => fb.value.show(b)
      }) + "]"
    }
  }
}
