import cats._
import cats.data.Xor
import cats.functor.{Invariant, Contravariant}
import shapeless._
import simulacrum.typeclass

import cats.std.all._
import cats.syntax.all._
import Cocartesian.ops._

@typeclass trait Cocartesian[F[_]] {
  def coproduct[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

object Cocartesian

object ShowInstance {
  implicit val showInvariant: Invariant[Show] = implicitly[Contravariant[Show]]
  
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

class FelisCatus[C[_]](implicit a: Cartesian[C], o: Cocartesian[C], i: Invariant[C]) {
  private implicit class coproductFold[+H, +T <: Coproduct](c: H :+: T) {
    def fold[F](lf: H => F, rf: T => F): F = c match {
      case Inl(l) => lf(l)
      case Inr(r) => rf(r)
    }
  }
  
  def deriveInstance[F] = new DeriveInstanceCurried[F] {}
  
  trait DeriveInstanceCurried[F] {
    def apply[G]()(implicit g: Generic.Aux[F, G], c: C[G]): C[F] =
      c.imap(g.from)(g.to)
  }
  
  implicit def deriveHNil: C[HNil] = ???
      // h.product(t.value).imap(x => x._1 :: x._2)(x => x.head -> x.tail)

  implicit def deriveHCons[H, T <: HList](implicit h: C[H], t: C[T]): C[H :: T] =
    h.product(t).imap(x => x._1 :: x._2)(x => x.head -> x.tail)
  
  implicit def deriveHLast[H](implicit h: C[H] ): C[H :: HNil] = h.imap(HNil.::)(_.head)
  
  implicit def deriveCCons[H, T <: Coproduct](implicit h: C[H], t: C[T]): C[H :+: T] =
    h.coproduct(t).imap[H :+: T](
      _.fold(Inl.apply, Inr.apply))(
      _.fold(Xor.Left.apply, Xor.Right.apply))
  
  // implicit def deriveCLast[H](implicit h: Lazy[C[H]]): C[H :+: CNil] =
  //   h.imap[H :+: CNil](Inl.apply)(_.fold(identity, null))
    
  // implicit def deriveCNil: C[CNil] = ???
}

sealed trait Animal
case class Cat(name: String, age: Int) extends Animal
case class Dog(name: String, age: Int) extends Animal
case class DoubleCD(c: Cat, d: Dog)

object DeriveShow extends App {
  import ShowInstance._
  
  private implicit class coproductFold[+H, +T <: Coproduct](c: H :+: T) {
    def fold[F](lf: H => F, rf: T => F): F = c match {
      case Inl(l) => lf(l)
      case Inr(r) => rf(r)
    }
  }
  
  // val showFelisCatus = new FelisCatus[Show] {}
  // import showFelisCatus._
  implicit def derive[F, G]()(implicit g: Generic.Aux[F, G], c: Show[G]): Show[F] =
    c.imap(g.from)(g.to)
    
  implicit def deriveHCons[H, T <: HList](implicit h: Show[H], t: Show[T]): Show[H :: T] =
    h.product(t).imap(x => x._1 :: x._2)(x => x.head -> x.tail)
  
  implicit def deriveHLast[H](implicit h: Show[H]): Show[H :: HNil] =
    h.imap(HNil.::)(_.head)
  
  // implicit def deriveCCons[H, T <: Coproduct](implicit h: Show[H], t: Show[T]): Show[H :+: T] =
  //   h.coproduct(t).imap[H :+: T](
  //     _.fold(Inl.apply, Inr.apply))(
  //     _.fold(Xor.Left.apply, Xor.Right.apply))
  
  // sealed trait Tree[T]
  // case class Leaf[T](t: T) extends Tree[T]
  // case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]
    
  // val tree =
  //   Node(
  //     Leaf("quux"),
  //     Node(
  //       Leaf("foo"),
  //       Leaf("wibble")
  //     )
  //   )
    
  // val showTreeString = deriveInstance[Show, Tree[String]]()
  
  val felix = Cat("Felix", 2)
  val tigger = Dog("Tigger", 2)
  
  // implicit val genericAnimal = the[Generic[Animal]]
  val showAnimal: Show[DoubleCD] = implicitly[Show[Cat]]
  
  println(showAnimal.show(felix))
}
