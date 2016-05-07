package felis.catus

import cats.data.Xor
import cats.arrow.NaturalTransformation
import cats.free.Inject

sealed abstract class FreeCatus[F[_], A] extends Product with Serializable { self =>
  import FreeCatus.{FA, PZip, DZip, Imap, lift}

  def imap[B](f: A => B)(g: B => A): FA[F, B] =
    Imap(this, f, g)

  def product[B](fb: FA[F, B]): FA[F, (A, B)] =
    PZip(this, fb)

  def disjoint[B](fb: FA[F, B]): FA[F, Xor[A, B]] =
    DZip(this, fb)

  /** Interprets/Runs the sequence of operations using the semantics of `Catus[G]` */
  def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Catus[G]): G[A]
  // Note that implementing a concrete `foldMap` here does not work because
  // `PZip extends G[(A, B)]` confuses the type inferance when pattern matching on `this`.

  /** Interpret/run the operations using the semantics of `Catus[F]`. */
  final def fold(implicit F: Catus[F]): F[A] =
    foldMap(NaturalTransformation.id[F])

  /** Interpret this algebra into another Catus */
  final def compile[G[_]](f: NaturalTransformation[F, G]): FA[G, A] =
    foldMap[FA[G, ?]] {
      new NaturalTransformation[F, FA[G, ?]] {
        def apply[B](fa: F[B]): FA[G, B] = lift(f(fa))
      }
    }
}

object FreeCatus {
  type FA[F[_], A] = FreeCatus[F, A]

  def inject[G[_], H[_]]: FreeCatusInjectCurried[G, H] = new FreeCatusInjectCurried

  final class FreeCatusInjectCurried[F[_], G[_]] {
    def apply[A](fa: F[A])(implicit I: Inject[F, G]): FreeCatus[G, A] =
      FreeCatus.lift(I.inj(fa))
  }

  case class Suspend[F[_], A](fa: F[A]) extends FA[F, A] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Catus[G]): G[A] =
      nt(fa)
  }

  case class PZip[F[_], A, B](fa: FA[F, A], fb: FA[F, B]) extends FA[F, (A, B)] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Catus[G]): G[(A, B)] =
      c.product(fa.foldMap(nt), fb.foldMap(nt))
  }

  case class DZip[F[_], A, B](fa: FA[F, A], fb: FA[F, B]) extends FA[F, Xor[A, B]] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Catus[G]): G[Xor[A, B]] =
      c.disjoint(fa.foldMap(nt), fb.foldMap(nt))
  }

  case class Imap[F[_], A, B](fa: FA[F, A], f: A => B, g: B => A) extends FA[F, B] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit c: Catus[G]): G[B] =
      c.imap(fa.foldMap(nt))(f)(g)
  }

  def lift[F[_], A](fa: F[A]): FA[F, A] =
    Suspend(fa)

  /** `FreeCatus[S, ?]` has a Catus for any type constructor `S[_]`. */
  implicit def freeInvariant[S[_]]: Catus[FA[S, ?]] =
    new Catus[FA[S, ?]] {
      def imap[A, B](fa: FA[S, A])(f: A => B)(g: B => A): FA[S, B] = fa.imap(f)(g)
      def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] = fa.product(fb)
      def disjoint[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, Xor[A, B]] = fa.disjoint(fb)
    }
}
