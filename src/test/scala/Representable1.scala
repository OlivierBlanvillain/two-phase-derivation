package deriving

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait LowPriorityFunctor {
  implicit def generic[F[_], R[_]](implicit g: Representable1[F] { type Repr = R }, i: => Functor[R]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        g.from(i.map(g.to(fa))(f))
    }
  implicit def pcons[H[_], T[t] <: Prod[t]](implicit h: => Functor[H], t: => Functor[T]): Functor[H :*: T] =
    new Functor[H :*: T] {
      def map[A, B](fa: (H :*: T)[A])(f: A => B): (H :*: T)[B] = {
        h.map(fa.head)(f) :*: t.map(fa.tail)(f)
      }
    }

  implicit def scons[H[_], T[t] <: Sum[t]](implicit h: => Functor[H], t: => Functor[T]): Functor[H :-: T] =
    new Functor[H :-: T] {
      def map[A, B](fa: (H :-: T)[A])(f: A => B): (H :-: T)[B] = {
        fa match {
          case Sum.L(x) => Sum.L(h.map(x)(f))
          case Sum.R(x) => Sum.R(t.map(x)(f))
        }
      }
    }
}

object Functor extends LowPriorityFunctor {
  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

  implicit val pnil: Functor[Prod.N] =
    new Functor[Prod.N] {
      def map[A, B](fa: Prod.N[A])(f: A => B): Prod.N[B] = {
        Prod.N()
      }
    }

  implicit def snil: Functor[Sum.E] = ???
}

object Representable1Tests {
  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
  }

  case class Box[T](t: T)

  implicit val gen1Box: Representable1[Box] { type Repr = Id :*: Prod.N } = new Representable1[Box] {
    type Repr = Id :*: Prod.N

    def to[T](a: Box[T]): Repr[T] = a match {
      case Box(x) => x :*: Prod.N()
    }

    def from[T](r: Repr[T]): Box[T] = r match {
      case x :*: Prod.N() => Box(x)
    }
  }

  implicitly[Functor[Box]] // Functor.generic[Box, Id :*: Prod.N](gen1Box, Functor.pcons(Functor.idFunctor, Functor.pnil))


  sealed trait Opt[A]
  case class Non[A]()     extends Opt[A]
  case class Som[A](a: A) extends Opt[A]

  implicit val gen1Non: Representable1[Non] { type Repr = Prod.N } = new Representable1[Non] {
    type Repr = Prod.N
    def to[T](a: Non[T]): Repr[T] = a match {
      case Non() => Prod.N[T]()
    }

    def from[T](r: Repr[T]): Non[T] = r match {
      case Prod.N() => Non()
    }
  }


  implicit val gen1Som: Representable1[Som] { type Repr = Id :*: Prod.N } = new Representable1[Som] {
    type Repr = Id :*: Prod.N

    def to[T](a: Som[T]): Repr[T] = a match {
      case Som(x) => Prod.C(x, Prod.N[T]())
    }

    def from[T](r: Repr[T]): Som[T] = r match {
      case Prod.C(x, Prod.N()) => Som(x)
    }
  }

  implicit val gen1Opt: Representable1[Opt] { type Repr = Non :-: Som :-: Sum.E } = new Representable1[Opt] {
    type Repr = Non :-: Som :-: Sum.E

    def to[T](a: Opt[T]): Repr[T] = a match {
      case Non()  => Sum.L(Non())
      case Som(x) => Sum.R(Sum.L(Som(x)))
    }

    def from[T](r: Repr[T]): Opt[T] = r match {
      case Sum.L(x)      => x
      case Sum.R(Sum.L(x)) => x
      case _           => ???
    }
  }

  implicitly[Functor[Opt]]


  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  implicit val gen1Node: Representable1[Node] { type Repr = Tree :*: Tree :*: Prod.N } = new Representable1[Node] {
    type Repr = Tree :*: Tree :*: Prod.N

    def to[T](a: Node[T]): Repr[T] = a match {
      case Node(l, r) =>
        val x = (r :*: Prod.N())
        l :*: x
    }

    def from[T](r: Repr[T]): Node[T] = r match {
      // Workaround #3381, this is simply `case Prod.C(l, Prod.C(r, Prod.N())) =>`
      case Prod.C(l, cons) => cons match {
        case Prod.C(r, Prod.N()) =>
          Node(l, r)
      }
    }
  }

  implicit val gen1Leaf: Representable1[Leaf] { type Repr = Id :*: Prod.N } = new Representable1[Leaf] {
    type Repr = Id :*: Prod.N

    def to[T](a: Leaf[T]): Repr[T] = a match {
      case Leaf(x) => Prod.C(x, Prod.N[T]())
    }

    def from[T](r: Repr[T]): Leaf[T] = r match {
      case Prod.C(x, Prod.N()) => Leaf(x)
    }
  }

  implicit val gen1Tree: Representable1[Tree] { type Repr = Leaf :-: Node :-: Sum.E } = new Representable1[Tree] {
    type Repr = Leaf :-: Node :-: Sum.E

    def to[T](a: Tree[T]): Repr[T] = a match {
      case Leaf(x)  => Sum.L(Leaf(x))
      case Node(x, y) => Sum.R(Sum.L(Node(x, y)))
    }

    def from[T](r: Repr[T]): Tree[T] = r match {
      case Sum.L(x)      => x
      case Sum.R(Sum.L(x)) => x
      case _           => ???
    }
  }

  val treeFunctor = implicitly[Functor[Tree]]

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
