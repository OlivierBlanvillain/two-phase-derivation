package deriving

// cats.Show
trait Show[A] {
  def show(a: A): String
}

// cats.Cartesian
trait Cartesian[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def unit: F[Unit]
}

// cats.functor.Invariant
trait Invariant[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

// Not actually parts of cats, but you got the idea...
/** The `Either` version of `Cartesian`. */
trait Cocartesian[F[_]] {
  def coproduct[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

object Syntax {
  implicit class InvariantSyntax[F[_]: Invariant, A](fa: F[A]) {
    def imap[B](f: A => B)(g: B => A): F[B] = implicitly[Invariant[F]].imap(fa)(f)(g)
  }
  implicit class CartesianSyntax[F[_]: Cartesian, A](fa: F[A]) {
    def product[B](fb: F[B]): F[(A, B)] = implicitly[Cartesian[F]].product(fa, fb)
  }
  implicit class CocartesianSyntax[F[_]: Cocartesian, A](fa: F[A]) {
    def coproduct[B](fb: F[B]): F[Either[A, B]] = implicitly[Cocartesian[F]].coproduct(fa, fb)
  }
}

