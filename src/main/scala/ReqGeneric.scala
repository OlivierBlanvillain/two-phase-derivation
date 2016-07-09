import shapeless._
import shapeless.ops.hlist.{LiftAll, ToTraversable}
import cats.Show
import cats.data.Xor
import cats.implicits._
import simulacrum.typeclass
import scala.reflect.runtime.universe.TypeTag

trait Lift[F[_], Repr] { self =>
  def get[A: TypeTag]: F[A]

  def cast[R]: Lift[F, R] =
    new Lift[F, R] {
      def get[A: TypeTag]: F[A] = self.get
    }
}

object Lift {
  implicit def viaLiftAll[F[_], Repr, FlatRepr <: HList, LiftedRepr <: HList, TypeTags <: HList]
    (implicit
      le: Leaves.Aux[Repr, FlatRepr],
      la: LiftAll.Aux[F,       FlatRepr, LiftedRepr],
      tt: LiftAll.Aux[TypeTag, FlatRepr, TypeTags],
      ll: ToTraversable.Aux[LiftedRepr, List, F[_]],
      lt: ToTraversable.Aux[TypeTags,   List, TypeTag[_]]
    ): Lift[F, Repr] =
      new Lift[F, Repr] {
        val map: Map[TypeTag[_], F[_]] =
          tt.instances.toList[TypeTag[_]].zip(la.instances.toList[F[_]]).toMap

        println(map)

        def get[A: TypeTag]: F[A] = map(implicitly[TypeTag[A]]).asInstanceOf[F[A]]
      }

  implicit class HConsLift[F[_], H, T <: HList](self: Lift[F, H :: T]) {
    def head: Lift[F, H] = self.cast
    def tail: Lift[F, T] = self.cast
  }

  implicit class CConsLift[F[_], H, T <: Coproduct](self: Lift[F, H :+: T]) {
    def head: Lift[F, H] = self.cast
    def tail: Lift[F, T] = self.cast
  }
}

trait Derive[T] {
  type Repr

  def derive[F[_]](implicit f: Felis[F], l: Lift[F, Repr]): F[T]
}

object Derive {
  type Aux[T, R0] = Derive[T] { type Repr = R0 }

  implicit def hbase[T: TypeTag](implicit h: HasNoGeneric[T]): Aux[T, T] = new Derive[T] {
    type Repr = T
    def derive[F[_]] (implicit f: Felis[F], l: Lift[F, Repr]): F[T] = l.get[T]
  }

  implicit def hnil: Aux[HNil, HNil] = new Derive[HNil] {
    type Repr = HNil
    def derive[F[_]] (implicit f: Felis[F], l: Lift[F, Repr]): F[HNil] = f.pure(HNil)
  }

  implicit def cnil: Aux[CNil, CNil] = new Derive[CNil] {
    type Repr = CNil
    def derive[F[_]] (implicit f: Felis[F], l: Lift[F, Repr]): F[CNil] = f.pure(null)
  }

  implicit def gen[A, G, R]
    (implicit
      g: Generic.Aux[A, G],
      r: Lazy[Aux[G, R]]
    ): Aux[A, R] = new Derive[A] {
      type Repr = R
      def derive[F[_]] (implicit f: Felis[F], l: Lift[F, Repr]): F[A] =
        r.value.derive[F].imap(g.from)(g.to)
    }

  implicit def hcons[H, HR, T <: HList, TR <: HList]
    (implicit
      h: Aux[H, HR],
      t: Lazy[Aux[T, TR]]
    ): Aux[H :: T, HR :: TR] =
      new Derive[H :: T] {
        type Repr = HR :: TR
        def derive[F[_]] (implicit f: Felis[F], l: Lift[F, Repr]): F[H :: T] =
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
        def derive[F[_]] (implicit f: Felis[F], l: Lift[F, Repr]): F[H :+: T] =
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

sealed trait AABB
case class AA(a: String) extends AABB
case class BB(a: String) extends AABB
case class DAABB(d: Double, aabb: AABB)
case class IDAABBS(i: Int, daabb: DAABB, s: String)

object DeriveTest extends App {
  val rg = the[Derive[IDAABBS]]
  type Expected = Int :: (Double :: ((String :: HNil) :+: (String :: HNil) :+: CNil) :: HNil) :: String :: HNil
  implicitly[rg.Repr =:= Expected]

  import ShowInstances._
  import cats.std.all._

  val showIDAABBS: Show[IDAABBS] = rg.derive[Show]
  println(showIDAABBS.show(IDAABBS(1, DAABB(1.1, AA("aa")), "s")))
}

