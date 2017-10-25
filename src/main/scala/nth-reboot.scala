object lib {
  sealed trait Product[X]
  final case class Pcons[X, H[_], T[t] <: Product[t]](head: H[X], tail: T[X]) extends Product[X]
  final case class Pnil[X]() extends Product[X]

  sealed trait Sum[X]
  sealed trait Scons[X, H[_], T[t] <: Sum[t]] extends Sum[X]
  final case class Inl[X, H[_], T[t] <: Sum[t]](head: H[X]) extends Scons[X, H, T]
  final case class Inr[X, H[_], T[t] <: Sum[t]](tail: T[X]) extends Scons[X, H, T]
  sealed trait Snil[X] extends Sum[X]

  type :*:[H[_], T[t] <: Product[t]] = [X] => Pcons[X, H, T]
  type :+:[H[_], T[t] <: Sum[t]] = [X] => Scons[X, H, T]
  type Id[t] = t

  trait Generic[A] {
    type Repr[t]

    def to[T](a: A): Repr[T]
    def from[T](r: Repr[T]): A
  }

  trait Generic1[A[_]] {
    type Repr[t]

    def to[T](a: A[T]): Repr[T]
    def from[T](r: Repr[T]): A[T]
  }
}

import lib._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait LowPriorityFunctor {
  implicit def generic[F[_], R[_]](implicit g: Generic1[F] { type Repr = R }, i: => Functor[R]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        g.from(i.map(g.to(fa))(f))
    }
  implicit def pcons[H[_], T[t] <: Product[t]](implicit h: => Functor[H], t: => Functor[T]): Functor[H :*: T] =
    new Functor[H :*: T] {
      def map[A, B](fa: (H :*: T)[A])(f: A => B): (H :*: T)[B] = {
        Pcons[B, H, T](h.map(fa.head)(f), t.map(fa.tail)(f))
      }
    }

  implicit def scons[H[_], T[t] <: Sum[t]](implicit h: => Functor[H], t: => Functor[T]): Functor[H :+: T] =
    new Functor[H :+: T] {
      def map[A, B](fa: (H :+: T)[A])(f: A => B): (H :+: T)[B] = {
        fa match {
          case Inl(x) => Inl[B, H, T](h.map(x)(f))
          case Inr(x) => Inr[B, H, T](t.map(x)(f))
        }
      }
    }
}

object Functor extends LowPriorityFunctor {
  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

  implicit val pnil: Functor[Pnil] =
    new Functor[Pnil] {
      def map[A, B](fa: Pnil[A])(f: A => B): Pnil[B] = {
        Pnil[B]()
      }
    }

  implicit def snil: Functor[Snil] = ???

    // new Functor[Pnil] {
    //   def map[A, B](fa: Pnil[A])(f: A => B): Pnil[B] = {
    //     Pnil[B]()
    //   }
    // }
}

object demo {
  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
  }

  case class Box[T](t: T)

  implicit val gen1Box: Generic1[Box] { type Repr = Id :*: Pnil } = new Generic1[Box] {
    type Repr = Id :*: Pnil

    def to[T](a: Box[T]): Repr[T] = a match {
      case Box(x) => Pcons[T, Id, Pnil](x, Pnil[T]())
    }

    def from[T](r: Repr[T]): Box[T] = r match {
      case Pcons(x, Pnil()) => Box(x)
    }
  }

  implicitly[Functor[Box]] // Functor.generic[Box, Id :*: Pnil](gen1Box, Functor.pcons(Functor.idFunctor, Functor.pnil))


  sealed trait Opt[A]
  case class Non[A]()     extends Opt[A]
  case class Som[A](a: A) extends Opt[A]

  implicit val gen1Non: Generic1[Non] { type Repr = Pnil } = new Generic1[Non] {
    type Repr = Pnil
    def to[T](a: Non[T]): Repr[T] = a match {
      case Non() => Pnil[T]()
    }

    def from[T](r: Repr[T]): Non[T] = r match {
      case Pnil() => Non()
    }
  }


  implicit val gen1Som: Generic1[Som] { type Repr = Id :*: Pnil } = new Generic1[Som] {
    type Repr = Id :*: Pnil

    def to[T](a: Som[T]): Repr[T] = a match {
      case Som(x) => Pcons(x, Pnil[T]())
    }

    def from[T](r: Repr[T]): Som[T] = r match {
      case Pcons(x, Pnil()) => Som(x)
    }
  }

  implicit val gen1Opt: Generic1[Opt] { type Repr = Non :+: Som :+: Snil } = new Generic1[Opt] {
    type Repr = Non :+: Som :+: Snil

    def to[T](a: Opt[T]): Repr[T] = a match {
      case Non()  => Inl[T, Non, Som :+: Snil](Non())
      case Som(x) => Inr[T, Non, Som :+: Snil](Inl[T, Som, Snil](Som(x)))
    }

    def from[T](r: Repr[T]): Opt[T] = r match {
      case Inl(x)      => x
      case Inr(Inl(x)) => x
      case _           => ???
    }
  }

  implicitly[Functor[Opt]]


  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  implicit val gen1Node: Generic1[Node] { type Repr = Tree :*: Tree :*: Pnil } = new Generic1[Node] {
    type Repr = Tree :*: Tree :*: Pnil

    def to[T](a: Node[T]): Repr[T] = a match {
      case Node(l, r) => Pcons(l, Pcons(r, Pnil[T]()))
    }

    def from[T](r: Repr[T]): Node[T] = r match {
      // Workaround #3381, this is simply `case Pcons(l, Pcons(r, Pnil())) =>`
      case Pcons(l, cons) => cons match {
        case Pcons(r, Pnil()) =>
          Node(l, r)
      }
    }
  }

  implicit val gen1Leaf: Generic1[Leaf] { type Repr = Id :*: Pnil } = new Generic1[Leaf] {
    type Repr = Id :*: Pnil

    def to[T](a: Leaf[T]): Repr[T] = a match {
      case Leaf(x) => Pcons(x, Pnil[T]())
    }

    def from[T](r: Repr[T]): Leaf[T] = r match {
      case Pcons(x, Pnil()) => Leaf(x)
    }
  }

  implicit val gen1Tree: Generic1[Tree] { type Repr = Leaf :+: Node :+: Snil } = new Generic1[Tree] {
    type Repr = Leaf :+: Node :+: Snil

    def to[T](a: Tree[T]): Repr[T] = a match {
      case Leaf(x)  => Inl[T, Leaf, Node :+: Snil](Leaf(x))
      case Node(x, y) => Inr[T, Leaf, Node :+: Snil](Inl[T, Node, Snil](Node(x, y)))
    }

    def from[T](r: Repr[T]): Tree[T] = r match {
      case Inl(x)      => x
      case Inr(Inl(x)) => x
      case _           => ???
    }
  }

  val treeFunctor = implicitly[Functor[Tree]]

  def main(args: Array[String]): Unit = {
    val tree1 =
      Node(
        Leaf("quux"),
        Node(
          Leaf("foo"),
          Leaf("wibble")
        )
      )

    val tree2 =
      Node(
        Leaf(4),
        Node(
          Leaf(3),
          Leaf(6)
        )
      )

    assert(treeFunctor.map(tree1)(_.size) == tree2)
  }
}
