import cats._
import cats.data.Xor
import cats.functor.{Invariant, Contravariant}
import shapeless._
import simulacrum.typeclass

import cats.std.all._

@typeclass trait Cocartesian[F[_]] {
  def coproduct[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

object ShowInstances {
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

sealed trait Animal
case class Cat(name: String, fish: Int) extends Animal
case class Dog(name: String, bones: Int) extends Animal

class FelisCatus[C[_]](implicit a: Cartesian[C], o: Cocartesian[C], i: Invariant[C]) {
  import cats.syntax.all._
  import Cocartesian.ops._
  
  implicit def deriveGeneric[F, G](implicit g: Generic.Aux[F, G], c: Lazy[C[G]]): C[F] =
    c.value.imap(g.from)(g.to)

  implicit def deriveHLast[H](implicit h: C[H]): C[H :: HNil] =
    h.imap(HNil.::)(_.head)
  
  implicit def deriveHCons[H, T <: HList](implicit h: Lazy[C[H]], t: Lazy[C[T]]): C[H :: T] =
      h.value.product(t.value).imap(x => x._1 :: x._2)(x => x.head -> x.tail)
  
  implicit def deriveCLast[H](implicit h: Lazy[C[H]]): C[H :+: CNil] =
    h.value.imap[H :+: CNil](Inl.apply) {
      case Inl(l) => l
      case Inr(_) => throw new Exception("YOLO")
    }
  
  implicit def deriveCCons[H, T <: Coproduct](implicit h: C[H], t: C[T]): C[H :+: T] =
    h.coproduct(t).imap[H :+: T](_.fold(Inl.apply, Inr.apply)) {
      case Inl(l) => Xor.Left(l)
      case Inr(r) => Xor.Right(r)
    }
}

object Run extends App {
  val felix = Cat("Felix", 2)
  val tigger = Dog("Tigger", 2)
  
  val showAnimal: Show[Animal] = {
    import ShowInstances._
    val fc = new FelisCatus[Show]()
    import fc._
    implicitly
  }
  
  println(showAnimal.show(felix))
  println(showAnimal.show(tigger))
}
