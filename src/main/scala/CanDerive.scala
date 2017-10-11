trait Show[A] {
  def show(a: A): String
}

object Implicits {
  implicit val catsStdShowForString: Show[String] = new Show[String] { def show(x: String) = x }
  implicit val catsStdShowForDouble: Show[Double] = new Show[Double] { def show(x: Double) = x.toString }
  implicit val catsStdShowForInt: Show[Int]       = new Show[Int]    { def show(x: Int)    = x.toString }
  implicit val catsStdShowForLong: Show[Long]     = new Show[Long]   { def show(x: Long)   = x.toString }
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
