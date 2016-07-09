import shapeless._
import cats.Show
import cats.data.Xor
import cats.implicits._
import simulacrum.typeclass

trait Lift[F[_], Repr] {
  def get[A](implicit f: Felis[F]): F[A]
}

object Lift {
  implicit class HConsLift[F[_], H, T <: HList](self: Lift[F, H :: T]) {
    def head: Lift[F, H] = new Lift[F, H] {
      def get[A](implicit f: Felis[F]): F[A] = self.get
    }

    def tail: Lift[F, T] = new Lift[F, T] {
      def get[A](implicit f: Felis[F]): F[A] = self.get
    }
  }

  implicit class CConsLift[F[_], H, T <: Coproduct](self: Lift[F, H :+: T]) {
    def head: Lift[F, H] = new Lift[F, H] {
      def get[A](implicit f: Felis[F]): F[A] = self.get
    }

    def tail: Lift[F, T] = new Lift[F, T] {
      def get[A](implicit f: Felis[F]): F[A] = self.get
    }
  }
}

trait Felis[F[_]] extends InvariantMonoidal[F] with DisjointCartesian[F]

trait Derive[T] {
  type Repr

  def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[T]
}

object Derive {
  type Aux[T, R0] = Derive[T] { type Repr = R0 }
  protected def aux[T, R0] = null

  implicit def hbase[T](implicit h: HasNoGeneric[T]): Aux[T, T] = new Derive[T] {
    type Repr = T
    def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[T] = l.get[T]
  }

  implicit def hnil: Aux[HNil, HNil] = new Derive[HNil] {
    type Repr = HNil
    def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[HNil] = f.pure(HNil)
  }

  implicit def cnil: Aux[CNil, CNil] = new Derive[CNil] {
    type Repr = CNil
    def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[CNil] = f.pure(null)
  }

  implicit def gen[A, G, R]
    (implicit
      g: Generic.Aux[A, G],
      r: Lazy[Aux[G, R]]
    ): Aux[A, R] = new Derive[A] {
      type Repr = R
      def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[A] =
        r.value.derive[F].imap(g.from)(g.to)
    }

  implicit def hcons[H, HR, T <: HList, TR <: HList]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]]
    ): Aux[H :: T, HR :: TR] =
      new Derive[H :: T] {
        type Repr = HR :: TR
        def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[H :: T] =
          h.derive(f, l.head).product(t.value.derive[F](f, l.tail))
            .imap { case (a, b) => a :: b } { case a :: b => (a, b) }
      }

  implicit def ccons[H, HR, T <: Coproduct, TR <: Coproduct]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]]
    ): Aux[H :+: T, HR :+: TR] =
      new Derive[H :+: T] {
        import DisjointCartesian.ops._
        type Repr = HR :+: TR
        def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[H :+: T] =
          h.derive(f, l.head).coproduct(t.value.derive[F](f, l.tail))
            .imap {
              case Xor.Left(a)  => Inl(a)
              case Xor.Right(b) => Inr(b)
            } {
              case Inl(a) => Xor.Left(a)
              case Inr(b) => Xor.Right(b)
            }
      }
}

object DeriveTest {
  sealed trait AABB
  case class AA(a: String) extends AABB
  case class BB(a: String) extends AABB
  case class DAABB(d: Double, aabb: AABB)
  case class IDAABBS(i: Int, daabb: DAABB, s: String)

  object TheTest {
    val rg = the[Derive[IDAABBS]]
    type Expected = Int :: (Double :: ((String :: HNil) :+: (String :: HNil) :+: CNil) :: HNil) :: String :: HNil
    implicitly[rg.Repr =:= Expected]
  }
}

