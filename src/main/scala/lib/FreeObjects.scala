package felis.catus.lib

import cats.data.Xor
import cats.arrow.NaturalTransformation
import cats.free.Inject

sealed abstract class FreeObjects[F[_], A] extends Product with Serializable { self =>
  import FreeObjects.{FA, PZip, DZip, Imap, lift}

  def imap[B](f: A => B)(g: B => A): FA[F, B] =
    Imap(this, f, g)

  def product[B](fb: FA[F, B]): FA[F, (A, B)] =
    PZip(this, fb)

  def disjoint[B](fb: FA[F, B]): FA[F, Xor[A, B]] =
    DZip(this, fb)

  /** Interprets/Runs the sequence of operations using the semantics of `Objects[G]` */
  def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Objects[G]): G[A]
  // Note that implementing a concrete `foldMap` here does not work because
  // `PZip extends G[(A, B)]` confuses the type inferance when pattern matching on `this`.

  /** Interpret/run the operations using the semantics of `Objects[F]`. */
  final def fold(implicit F: Objects[F]): F[A] =
    foldMap(NaturalTransformation.id[F])

  /** Interpret this algebra into another Objects */
  final def compile[G[_]](f: NaturalTransformation[F, G]): FA[G, A] =
    foldMap[FA[G, ?]] {
      new NaturalTransformation[F, FA[G, ?]] {
        def apply[B](fa: F[B]): FA[G, B] = lift(f(fa))
      }
    }
}

object FreeObjects {
  type FA[F[_], A] = FreeObjects[F, A]

  def inject[G[_], H[_]]: FreeObjectsInjectCurried[G, H] = new FreeObjectsInjectCurried

  final class FreeObjectsInjectCurried[F[_], G[_]] {
    def apply[A](fa: F[A])(implicit I: Inject[F, G]): FreeObjects[G, A] =
      FreeObjects.lift(I.inj(fa))
  }

  case class Suspend[F[_], A](fa: F[A]) extends FA[F, A] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Objects[G]): G[A] =
      nt(fa)
  }

  case class PZip[F[_], A, B](fa: FA[F, A], fb: FA[F, B]) extends FA[F, (A, B)] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Objects[G]): G[(A, B)] =
      c.product(fa.foldMap(nt), fb.foldMap(nt))
  }

  case class DZip[F[_], A, B](fa: FA[F, A], fb: FA[F, B]) extends FA[F, Xor[A, B]] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Objects[G]): G[Xor[A, B]] =
      c.disjoint(fa.foldMap(nt), fb.foldMap(nt))
  }

  case class Imap[F[_], A, B](fa: FA[F, A], f: A => B, g: B => A) extends FA[F, B] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Objects[G]): G[B] =
      c.imap(fa.foldMap(nt))(f)(g)
  }

  def lift[F[_], A](fa: F[A]): FA[F, A] =
    Suspend(fa)

  /** `FreeObjects[S, ?]` has a Objects for any type constructor `S[_]`. */
  implicit def freeInvariant[S[_]]: Objects[FA[S, ?]] =
    new Objects[FA[S, ?]] {
      def imap[A, B](fa: FA[S, A])(f: A => B)(g: B => A): FA[S, B] = fa.imap(f)(g)
      def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] = fa.product(fb)
      def disjoint[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, Xor[A, B]] = fa.disjoint(fb)
    }
}
