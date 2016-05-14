package felis.catus.lib

import cats.arrow.NaturalTransformation
import cats.free.Inject

sealed abstract class FreeProducts[F[_], A] extends Product with Serializable { self =>
  import FreeProducts.{FA, PZip, Imap, lift}

  def imap[B](f: A => B)(g: B => A): FA[F, B] =
    Imap(this, f, g)

  def product[B](fb: FA[F, B]): FA[F, (A, B)] =
    PZip(this, fb)

  /** Interprets/Runs the sequence of operations using the semantics of `Products[G]` */
  def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Products[G]): G[A]
  // Note that implementing a concrete `foldMap` here does not work because
  // `PZip extends G[(A, B)]` confuses the type inferance when pattern matching on `this`.

  /** Interpret/run the operations using the semantics of `Products[F]`. */
  final def fold(implicit F: Products[F]): F[A] =
    foldMap(NaturalTransformation.id[F])

  /** Interpret this algebra into another Products */
  final def compile[G[_]](f: NaturalTransformation[F, G]): FA[G, A] =
    foldMap[FA[G, ?]] {
      new NaturalTransformation[F, FA[G, ?]] {
        def apply[B](fa: F[B]): FA[G, B] = lift(f(fa))
      }
    }
}

object FreeProducts {
  type FA[F[_], A] = FreeProducts[F, A]

  def inject[G[_], H[_]]: FreeProductsInjectCurried[G, H] = new FreeProductsInjectCurried

  final class FreeProductsInjectCurried[F[_], G[_]] {
    def apply[A](fa: F[A])(implicit I: Inject[F, G]): FreeProducts[G, A] =
      FreeProducts.lift(I.inj(fa))
  }

  case class Suspend[F[_], A](fa: F[A]) extends FA[F, A] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Products[G]): G[A] =
      nt(fa)
  }

  case class PZip[F[_], A, B](fa: FA[F, A], fb: FA[F, B]) extends FA[F, (A, B)] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Products[G]): G[(A, B)] =
      c.product(fa.foldMap(nt), fb.foldMap(nt))
  }

  case class Imap[F[_], A, B](fa: FA[F, A], f: A => B, g: B => A) extends FA[F, B] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Products[G]): G[B] =
      c.imap(fa.foldMap(nt))(f)(g)
  }

  def lift[F[_], A](fa: F[A]): FA[F, A] =
    Suspend(fa)

  /** `FreeProducts[S, ?]` has a Products for any type constructor `S[_]`. */
  implicit def freeInvariant[S[_]]: Products[FA[S, ?]] =
    new Products[FA[S, ?]] {
      def imap[A, B](fa: FA[S, A])(f: A => B)(g: B => A): FA[S, B] = fa.imap(f)(g)
      def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] = fa.product(fb)
    }
}
