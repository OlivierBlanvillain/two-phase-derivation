package deriving

object ADTs {
  sealed trait AABB
  case class AA(a: String) extends AABB
  case class BB(a: String) extends AABB
  case class DAABB(d: Double, aabb: AABB)
  case class IDAABBS(i: Int, daabb: DAABB, s: String)

  implicit val genAABB: Generic[AABB] { type Repr = AA :+: BB :+: CNil } = new Generic[AABB] {
    type Repr = AA :+: BB :+: CNil
    def to(t: AABB): Repr = ???
    def from(r: Repr): AABB = ???
  }
  implicit val genAA: Generic[AA] { type Repr = String :: HNil } = new Generic[AA] {
    type Repr = String :: HNil
    def to(t: AA): Repr = ???
    def from(r: Repr): AA = ???
  }
  implicit val genBB: Generic[BB] { type Repr = String :: HNil } = new Generic[BB] {
    type Repr = String :: HNil
    def to(t: BB): Repr = ???
    def from(r: Repr): BB = ???
  }
  implicit val genDAABB: Generic[DAABB] { type Repr = Double :: AABB :: HNil } = new Generic[DAABB] {
    type Repr = Double :: AABB :: HNil
    def to(t: DAABB): Repr = ???
    def from(r: Repr): DAABB = ???
  }
  implicit val genIDAABBS: Generic[IDAABBS] { type Repr = Int :: DAABB :: String :: HNil } = new Generic[IDAABBS] {
    type Repr = Int :: DAABB :: String :: HNil
    def to(t: IDAABBS): Repr = ???
    def from(r: Repr): IDAABBS = ???
  }

  val instance: IDAABBS = IDAABBS(1, DAABB(1.1, AA("aa")), "s")
  val showResult: String = "(1, ((1.1, [case: aa]), s))"

  // https://github.com/milessabin/kittens/blob/v1.0.0-M3/core/src/test/scala/cats/derived/adtdefns.scala
  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object IList {
    def fromSeq[T](ts: Seq[T]): IList[T] =
      ts.foldRight(INil[T](): IList[T])(ICons(_, _))
  }

  sealed trait Snoc[A]
  final case class SCons[A](init: Snoc[A], last: A) extends Snoc[A]
  final case class SNil[A]() extends Snoc[A]

  object Snoc {
    def fromSeq[T](ts: Seq[T]): Snoc[T] =
      ts.foldLeft(SNil[T](): Snoc[T])(SCons(_, _))
  }

  sealed trait Tree[T]
  final case class Leaf[T](t: T) extends Tree[T]
  final case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  case class CaseClassWOption[T](a: Option[T])
}
