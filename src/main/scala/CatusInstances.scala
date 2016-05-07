package felis.catus

import cats.data.Xor
import cats.Eq
import cats.Monoid
import cats.Order
import cats.Show

object CatusInstances {
  implicit val showCatus: Catus[Show] = new Catus[Show] {
    def product[A, B](fa: Show[A], fb: Show[B]): Show[(A, B)] =
      new Show[(A, B)] {
        def show(x: (A, B)): String = fa.show(x._1) + ", " + fb.show(x._2)
      }

    def disjoint[A, B](fa: Show[A], fb: Show[B]): Show[Xor[A, B]] =
      new Show[Xor[A, B]] {
        def show(x: Xor[A, B]): String = x.fold(fa.show, fb.show)
      }

    def imap[A, B](fa: Show[A])(f: A => B)(g: B => A): Show[B] =
      new Show[B] {
        def show(x: B): String = fa.show(g(x))
      }
  }

  implicit val orderCatus: Catus[Order] = new Catus[Order] {
    def product[A, B](fa: Order[A], fb: Order[B]): Order[(A, B)] =
      new Order[(A, B)] {
        def compare(x: (A, B), y: (A, B)): Int = {
          val ca = fa.compare(x._1, y._1)
          if (ca != 0) ca else fb.compare(x._2, y._2)
        }
      }

    def disjoint[A, B](fa: Order[A], fb: Order[B]): Order[Xor[A, B]] =
      new Order[Xor[A, B]] {
        def compare(x: Xor[A, B], y: Xor[A, B]): Int =
          (x, y) match {
            case (Xor.Right(xx), Xor.Right(yy)) => fb.compare(xx, yy)
            case (Xor.Left(xx), Xor.Right(_)) => 1
            case (Xor.Right(_), Xor.Left(yy)) => -1
            case (Xor.Left(xx), Xor.Left(yy)) => fa.compare(xx, yy)
          }
      }

    def imap[A, B](fa: Order[A])(f: A => B)(g: B => A): Order[B] =
      new Order[B] {
        def compare(x: B, y: B): Int = fa.compare(g(x), g(y))
      }
  }

  implicit val monoidCatus: Catus[Monoid] = new Catus[Monoid] {
    def product[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[(A, B)] =
      new Monoid[(A, B)] {
        def empty: (A, B) = fa.empty -> fb.empty
        def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
      }

    def disjoint[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[Xor[A, B]] =
      new Monoid[Xor[A, B]] {
        def empty: Xor[A, B] = Xor.Left(fa.empty)
        def combine(x: Xor[A, B], y: Xor[A, B]): Xor[A, B] =
          (x, y) match {
            case (Xor.Right(xx), Xor.Right(yy)) => Xor.Right(fb.combine(xx, yy))
            case (Xor.Left(xx), Xor.Right(_)) => Xor.Left(xx)
            case (Xor.Right(_), Xor.Left(yy)) => Xor.Left(yy)
            case (Xor.Left(xx), Xor.Left(yy)) => Xor.Left(fa.combine(xx, yy))
          }
      }

    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] =
      new Monoid[B] {
        def empty: B = f(fa.empty)
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      }
  }

  implicit val eqCatus: Catus[Eq] = new Catus[Eq] {
    def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
      new Eq[(A, B)] {
        def eqv(x: (A, B), y: (A, B)): Boolean = fa.eqv(x._1, y._1) && fb.eqv(x._2, y._2)
      }

    def disjoint[A, B](fa: Eq[A], fb: Eq[B]): Eq[Xor[A, B]] =
      new Eq[Xor[A, B]] {
        def eqv(x: Xor[A, B], y: Xor[A, B]): Boolean =
          (x, y) match {
            case (Xor.Right(xx), Xor.Right(yy)) => fb.eqv(xx, yy)
            case (Xor.Left(xx),  Xor.Left(yy))  => fa.eqv(xx, yy)
            case _ => false
          }
      }

    def imap[A, B](fa: Eq[A])(f: A => B)(g: B => A): Eq[B] =
      new Eq[B] {
        def eqv(x: B, y: B): Boolean = fa.eqv(g(x), g(y))
      }
  }
}
