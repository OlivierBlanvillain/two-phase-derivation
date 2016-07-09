import cats._
import cats.data.Xor
import cats.functor.{Invariant, Contravariant}
import shapeless._
import shapeless.ops.hlist.LiftAll
import simulacrum.typeclass
// import cats.std.all._

// See https://github.com/typelevel/cats/pull/845
@typeclass trait InvariantMonoidal[F[_]] extends Cartesian[F] with Invariant[F] {
  def pure[A](x: A): F[A]
}

// `Cartesian` for `Either`...
@typeclass trait DisjointCartesian[F[_]] {
  def coproduct[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

trait Felis[F[_]] extends InvariantMonoidal[F] with DisjointCartesian[F]

object ShowInstances {
  implicit val showFelis: Felis[Show] = new Felis[Show] {
    def pure[A](a: A): Show[A] = new Show[A] {
      def show(a: A): String = ""
    }

    def product[A, B](fa: Show[A], fb: Show[B]): Show[(A, B)] = new Show[(A, B)] {
      def show(ab: (A, B)): String = fa.show(ab._1) + ", " + fb.show(ab._2)
    }

    def imap[A, B](fa: Show[A])(f: A => B)(g: B => A): Show[B] = new Show[B] {
      def show(b: B): String = fa.show(g(b))
    }

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

class DeriveFC[C[_]](implicit i: InvariantMonoidal[C], o: DisjointCartesian[C]) {
  import cats.syntax.all._
  import DisjointCartesian.ops._

  implicit def deriveGeneric[F, G](implicit g: Generic.Aux[F, G], c: C[G]): C[F] =
    c.imap(g.from)(g.to)

  implicit def deriveHNil: C[HNil] = i.pure(HNil)

  implicit def deriveHCons[H, T <: HList](implicit h: Lazy[C[H]], t: Lazy[C[T]]): C[H :: T] =
      h.value.product(t.value).imap(x => x._1 :: x._2)(x => x.head -> x.tail)

  implicit def deriveCCNil: C[CNil] = i.pure(null)

  implicit def deriveCCons[H, T <: Coproduct](implicit h: Lazy[C[H]], t: Lazy[C[T]]): C[H :+: T] =
    h.value.coproduct(t.value).imap[H :+: T](_.fold(Inl.apply, Inr.apply)) {
      case Inl(l) => Xor.Left(l)
      case Inr(r) => Xor.Right(r)
    }
}

object DeriveFC {
  def derive[C[_], F] = new DeriveFCCurried[C, F]

  class DeriveFCCurried[C[_], F] {
    def apply[G, L <: HList, LL <: HList, S]()
      (implicit
        i: InvariantMonoidal[C],
        o: DisjointCartesian[C],
        // g: ReqGeneric.Aux[F, G],
        // l: Leaves.Aux[G, L],
        f: LiftAll.Aux[C, L, LL]
      ): C[F] = {
        // println(s.instances)
        ???
      }
  }
}

// object Run extends App {
//   import ShowInstances._
//   val felix = Cat("Felix", 2)
//   val tigger = Dog("Tigger", 2)

//   // val la = the[Leaves[ga.Repr]]
//   // val a: la.Out = ""
//   // LiftAll[Show, la.Out]
//   // new DeriveFC.DeriveFCCurried[Show, Animal]()()

//   DeriveFC.derive[Show, Animal]()

//   val fc = new DeriveFC[Show]()

//   val showAnimal: Show[Animal] = {
//     import fc._
//     implicitly[Show[Animal]]
//   }

//   // val showList: Show[Lis] = {
//   //   import fc._
//   //   implicitly[Show[Lis]]
//   // }
//   // val l: Lis = Cons(1, Cons(2, Cons(3, null)))

//   println(showAnimal.show(felix))
//   println(showAnimal.show(tigger))
// }
