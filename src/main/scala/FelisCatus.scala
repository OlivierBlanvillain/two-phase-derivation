import cats.{Cartesian, Show}
import cats.functor.Invariant
import cats.data.Xor
import simulacrum.typeclass

/** `Xor` version of `Cartesian`... */
@typeclass trait DisjointCartesian[F[_]] {
  def coproduct[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

/** All you need for automatic type class derivation of `F[_]`. */
@typeclass trait CanDerive[F[_]] extends Invariant[F] with Cartesian[F] with DisjointCartesian[F]

object CanDeriveShow {
  implicit val canDeriveShow: CanDerive[Show] = new CanDerive[Show] {
    def product[A, B](fa: Show[A], fb: Show[B]): Show[(A, B)] = new Show[(A, B)] {
      def show(ab: (A, B)): String = "(" + fa.show(ab._1) + ", " + fb.show(ab._2) + ")"
    }

    def imap[A, B](fa: Show[A])(f: A => B)(g: B => A): Show[B] = new Show[B] {
      def show(b: B): String = fa.show(g(b))
    }

    def coproduct[A, B](fa: Show[A], fb: Show[B]): Show[Xor[A, B]] = new Show[Xor[A, B]] {
      def show(ab: Xor[A, B]): String = "[case: " + ab.fold(fa.show, fb.show) + "]"
    }
  }
}
