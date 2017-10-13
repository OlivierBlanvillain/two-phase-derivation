package deriving

object ADTs {
  sealed trait AABB
  case class AA(a: String) extends AABB
  case class BB(a: String) extends AABB
  case class DAABB(d: Double, aabb: AABB)
  case class IDAABBS(i: Int, daabb: DAABB, s: String)

  val IDAABBSInstance: IDAABBS = IDAABBS(1, DAABB(1.1, AA("aa")), "s")
  val IDAABBSShowResult: String = "(1, ((1.1, [case: aa]), s))"

  // https://github.com/milessabin/kittens/blob/v1.0.0-M3/core/src/test/scala/cats/derived/adtdefns.scala
  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object IList {
    def fromSeq[A](ts: Seq[A]): IList[A] =
      ts.foldRight(INil[A](): IList[A])(ICons(_, _))
  }

  sealed trait Snoc[A]
  final case class SCons[A](init: Snoc[A], last: A) extends Snoc[A]
  final case class SNil[A]() extends Snoc[A]

  object Snoc {
    def fromSeq[A](ts: Seq[A]): Snoc[A] =
      ts.foldLeft(SNil[A](): Snoc[A])(SCons(_, _))
  }

  sealed trait Tree[A]
  final case class Leaf[A](a: A) extends Tree[A]
  final case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  case class CaseClassWOption[A](a: Option[A])

  case class Dog(age: Long)
  case class Cat(name: String, friend: Either[Cat, Dog])


  implicit val genDog: Generic[Dog] { type Repr = Long :: HNil } =
    new Generic[Dog] {
      type Repr = Long :: HNil
      def to(t: Dog): Repr = t match { case Dog(x) => ::(x, HNil) }
      def from(r: Repr): Dog = r match { case ::(x, HNil) => Dog(x) }
    }

  implicit val genCat: Generic[Cat] { type Repr = String :: Either[Cat, Dog] :: HNil } =
    new Generic[Cat] {
      type Repr = String :: Either[Cat, Dog] :: HNil
      def to(t: Cat): Repr = t match { case Cat(x, y) => ::(x, ::(y, HNil)) }
      def from(r: Repr): Cat = r match { case ::(x, ::(y, HNil)) => Cat(x, y) }
    }

  implicit def genEither[A, B]: Generic[Either[A, B]] { type Repr = Left[A, B] :+: Right[A, B] :+: CNil } =
    new Generic[Either[A, B]] {
      type Repr = Left[A, B] :+: Right[A, B] :+: CNil
      def to(t: Either[A, B]): Repr = t match {
        case x: Left[A, B] => Inl(x)
        case x: Right[A, B] => Inr(Inl(x))
      }
      def from(r: Repr): Either[A, B] = r match {
        case Inl(x) => x
        case Inr(Inl(x)) => x
        case _ => ???
      }
    }

  implicit def genLeft[A, B]: Generic[Left[A, B]] { type Repr = A :: HNil } =
    new Generic[Left[A, B]] {
      type Repr = A :: HNil
      def to(t: Left[A, B]): Repr = t match { case Left(x) => ::(x, HNil) }
      def from(r: Repr): Left[A, B] = r match { case ::(x, HNil) => Left(x) }
    }

  implicit def genRight[A, B]: Generic[Right[A, B]] { type Repr = B :: HNil } =
    new Generic[Right[A, B]] {
      type Repr = B :: HNil
      def to(t: Right[A, B]): Repr = t match { case Right(x) => ::(x, HNil) }
      def from(r: Repr): Right[A, B] = r match { case ::(x, HNil) => Right(x) }
    }

  implicit val genAABB: Generic[AABB] { type Repr = AA :+: BB :+: CNil } =
    new Generic[AABB] {
      type Repr = AA :+: BB :+: CNil
      def to(t: AABB): Repr = t match {
        case x: AA => Inl(x)
        case x: BB => Inr(Inl(x))
      }
      def from(r: Repr): AABB = r match {
        case Inl(x) => x
        case Inr(Inl(x)) => x
        case _ => ???
      }
    }

  implicit val genAA: Generic[AA] { type Repr = String :: HNil } =
    new Generic[AA] {
      type Repr = String :: HNil
      def to(t: AA): Repr = t match { case AA(x) => ::(x, HNil) }
      def from(r: Repr): AA = r match { case ::(x, HNil) => AA(x) }
    }

  implicit val genBB: Generic[BB] { type Repr = String :: HNil } =
    new Generic[BB] {
      type Repr = String :: HNil
      def to(t: BB): Repr = t match { case BB(x) => ::(x, HNil) }
      def from(r: Repr): BB = r match { case ::(x, HNil) => BB(x) }
    }

  implicit val genDAABB: Generic[DAABB] { type Repr = Double :: AABB :: HNil } =
    new Generic[DAABB] {
      type Repr = Double :: AABB :: HNil
      def to(t: DAABB): Repr = t match { case DAABB(x, y) => ::(x, ::(y, HNil)) }
      def from(r: Repr): DAABB = r match { case ::(x, ::(y, HNil)) => DAABB(x, y) }
    }

  implicit val genIDAABBS: Generic[IDAABBS] { type Repr = Int :: DAABB :: String :: HNil } =
    new Generic[IDAABBS] {
      type Repr = Int :: DAABB :: String :: HNil
      def to(t: IDAABBS): Repr = t match { case IDAABBS(x, y, z) => ::(x, ::(y, ::(z, HNil))) }
      def from(r: Repr): IDAABBS = r match { case ::(x, ::(y, ::(z, HNil))) => IDAABBS(x, y, z) }
    }

  implicit def genIList[A]: Generic[IList[A]] { type Repr = ICons[A] :+: INil[A] :+: CNil } =
    new Generic[IList[A]] {
      type Repr = ICons[A] :+: INil[A] :+: CNil
      def to(t: IList[A]): Repr = t match {
        case x: ICons[A] => Inl(x)
        case x: INil[A] => Inr(Inl(x))
      }
      def from(r: Repr): IList[A] = r match {
        case Inl(x) => x
        case Inr(Inl(x)) => x
        case _ => ???
      }
    }

  implicit def genICons[A]: Generic[ICons[A]] { type Repr = A :: IList[A] :: HNil } =
    new Generic[ICons[A]] {
      type Repr = A :: IList[A] :: HNil
      def to(t: ICons[A]): Repr = t match { case ICons(x, y) => ::(x, ::(y, HNil)) }
      def from(r: Repr): ICons[A] = r match { case ::(x, ::(y, HNil)) => ICons(x, y) }
    }

  implicit def genINil[A]: Generic[INil[A]] { type Repr = HNil } =
    new Generic[INil[A]] {
      type Repr = HNil
      def to(t: INil[A]): Repr = HNil
      def from(r: Repr): INil[A] = INil()
    }

  implicit def genSnoc[A]: Generic[Snoc[A]] { type Repr =  SCons[A] :+: SNil[A] :+: CNil } =
    new Generic[Snoc[A]] {
      type Repr =  SCons[A] :+: SNil[A] :+: CNil
      def to(t: Snoc[A]): Repr = t match {
        case x: SCons[A] => Inl(x)
        case x: SNil[A] => Inr(Inl(x))
      }
      def from(r: Repr): Snoc[A] = r match {
        case Inl(x) => x
        case Inr(Inl(x)) => x
        case _ => ???
      }
    }

  implicit def genSCons[A]: Generic[SCons[A]] { type Repr = Snoc[A] :: A :: HNil } =
    new Generic[SCons[A]] {
      type Repr = Snoc[A] :: A :: HNil
      def to(t: SCons[A]): Repr = t match { case SCons(x, y) => ::(x, ::(y, HNil)) }
      def from(r: Repr): SCons[A] = r match { case ::(x, ::(y, HNil)) => SCons(x, y) }
    }

  implicit def genSNil[A]: Generic[SNil[A]] { type Repr = HNil } =
    new Generic[SNil[A]] {
      type Repr = HNil
      def to(t: SNil[A]): Repr = HNil
      def from(r: Repr): SNil[A] = SNil()
    }

  implicit def genTree[A]: Generic[Tree[A]] { type Repr = Leaf[A] :+: Node[A] :+: CNil } =
    new Generic[Tree[A]] {
      type Repr = Leaf[A] :+: Node[A] :+: CNil
      def to(t: Tree[A]): Repr = t match {
        case x: Leaf[A] => Inl(x)
        case x: Node[A] => Inr(Inl(x))
      }
      def from(r: Repr): Tree[A] = r match {
        case Inl(x) => x
        case Inr(Inl(x)) => x
        case _ => ???
      }
    }

  implicit def genLeaf[A]: Generic[Leaf[A]] { type Repr = A :: HNil } =
    new Generic[Leaf[A]] {
      type Repr = A :: HNil
      def to(t: Leaf[A]): Repr = t match { case Leaf(x) => ::(x, HNil) }
      def from(r: Repr): Leaf[A] = r match { case ::(x, HNil) => Leaf(x) }
    }

  implicit def genNode[A]: Generic[Node[A]] { type Repr = Tree[A] :: Tree[A] :: HNil } =
    new Generic[Node[A]] {
      type Repr = Tree[A] :: Tree[A] :: HNil
      def to(t: Node[A]): Repr = t match { case Node(x, y) => ::(x, ::(y, HNil)) }
      def from(r: Repr): Node[A] = r match { case ::(x, ::(y, HNil)) => Node(x, y) }
    }

  implicit def genCaseClassWOption[A]: Generic[CaseClassWOption[A]] { type Repr = Option[A] :: HNil } =
    new Generic[CaseClassWOption[A]] {
      type Repr = Option[A] :: HNil
      def to(t: CaseClassWOption[A]): Repr = t match { case CaseClassWOption(x) => ::(x, HNil) }
      def from(r: Repr): CaseClassWOption[A] = r match { case ::(x, HNil) => CaseClassWOption(x) }
    }
}
