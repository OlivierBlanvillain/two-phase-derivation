import cats._
import cats.data.Xor
import cats.functor.{Invariant, Contravariant}
import shapeless._
import simulacrum.typeclass

import cats.std.all._

@typeclass trait Cocartesian[F[_]] {
  def coproduct[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

trait Pure[F[_]] {
  def pure[A](a: A): F[A]
}

object ShowInstances {
  implicit val showInvariant: Invariant[Show] = implicitly[Contravariant[Show]]
  
  implicit val showPure: Pure[Show] = new Pure[Show] {
    def pure[A](a: A): Show[A] = new Show[A] {
      def show(a: A): String = ""
    }
  }
  
  implicit val showCartesian: Cartesian[Show] = new Cartesian[Show] {
    def product[A, B](fa: Show[A], fb: Show[B]): Show[(A, B)] = new Show[(A, B)] {
      def show(ab: (A, B)): String = fa.show(ab._1) + ", " + fb.show(ab._2)
    }
  }
  
  implicit val showCocartesian: Cocartesian[Show] = new Cocartesian[Show] {
    def coproduct[A, B](fa: Show[A], fb: Show[B]): Show[Xor[A, B]] = new Show[Xor[A, B]] {
      def show(ab: Xor[A, B]): String = ab.fold(fa.show, fb.show)
    }
  }
}

sealed trait Animal
case class Cat(name: String, fish: Int) extends Animal
case class Dog(name: String, bones: Int) extends Animal

// sealed trait Lis
// case class Cons(hd: Int, l: Lis) extends Lis
// case object Nal extends Lis

class FelisCatus[C[_]](implicit a: Cartesian[C], o: Cocartesian[C], i: Invariant[C], u: Pure[C]) {
    import cats.syntax.all._
    import Cocartesian.ops._
    
    implicit def deriveGeneric[F, G](implicit g: Generic.Aux[F, G], c: C[G]): C[F] =
      c.imap(g.from)(g.to)
    
    implicit def deriveHNil: C[HNil] = u.pure(HNil)
    
    implicit def deriveHCons[H, T <: HList](implicit h: Lazy[C[H]], t: Lazy[C[T]]): C[H :: T] =
        h.value.product(t.value).imap(x => x._1 :: x._2)(x => x.head -> x.tail)
    
    implicit def deriveCCNil: C[CNil] = u.pure(null)
       
    implicit def deriveCCons[H, T <: Coproduct](implicit h: Lazy[C[H]], t: Lazy[C[T]]): C[H :+: T] =
      h.value.coproduct(t.value).imap[H :+: T](_.fold(Inl.apply, Inr.apply)) {
        case Inl(l) => Xor.Left(l)
        case Inr(r) => Xor.Right(r)
      }
  }

object Run extends App {
  import ShowInstances._
  
  val felix = Cat("Felix", 2)
  val tigger = Dog("Tigger", 2)
  
  val fc = new FelisCatus[Show]()
  
  val showAnimal: Show[Animal] = {
    import fc._
    implicitly[Show[Animal]]
  }
  
  // val showList: Show[Lis] = {
  //   import fc._
  //   implicitly[Show[Lis]]
  // }
  // val l: Lis = Cons(1, Cons(2, Cons(3, null)))
  
  println(showAnimal.show(felix))
  println(showAnimal.show(tigger))
}
