package deriving

import shapeless._

object DeriveFTest extends App {
  import shapeless.test.illTyped
  import ADTs._

  // Non Recursive HList/Coproduct structure ----------------------------------

  val derivingIDAABS = Deriving[IDAABBS].gen

  implicitly[derivingIDAABS.TreeRepr =:= (
    Int ::
      (Double ::
      ((String :: HNil) :+:
       (String :: HNil) :+: CNil) ::
      HNil) ::
    String ::
    HNil
  )]

  implicitly[derivingIDAABS.FlatRepr =:= (
    Int :: Double :: String :: String :: String :: HNil
  )]

  illTyped(
    "derivingIDAABS.materialize[Show]",
    "could not find implicit value for parameter I1: deriving.Show\\[Int\\].*")

  object Implicits {
    implicit val catsStdShowForString: Show[String] = new Show[String] { def show(x: String) = x }
    implicit val catsStdShowForDouble: Show[Double] = new Show[Double] { def show(x: Double) = x.toString }
    implicit val catsStdShowForInt: Show[Int]       = new Show[Int]    { def show(x: Int)    = x.toString }
    implicit val catsStdShowForLong: Show[Long]     = new Show[Long]   { def show(x: Long)   = x.toString }
  }
  import Implicits._

  val showIDAABBS: Show[IDAABBS] = derivingIDAABS.materialize[Show]
  /*assert*/(showIDAABBS.show(instance) == showResult)

  // Either Recursion ---------------------------------------------------------

  case class Dog(age: Long)
  case class Cat(name: String, friend: Either[Cat, Dog])

  val deriveingCat = Deriving[Cat].gen
  implicitly[deriveingCat.TreeRepr =:= (String :: ((HNil :: HNil) :+: ((Long :: HNil) :: HNil) :+: CNil) :: HNil)]
  implicitly[deriveingCat.FlatRepr =:= (String :: Long :: HNil)]

  val showCat: Show[Cat] = deriveingCat.materialize[Show]
  /*assert*/(showCat.show(Cat("sansan", Right(Dog(4)))) == "(sansan, [case: [case: 4]])")
  /*assert*/(showCat.show(Cat("sansan", Left(Cat("aslan", Right(Dog(4)))))) == "(sansan, [case: (aslan, [case: [case: 4]])])")

  // TestDefns ----------------------------------------------------------------

  val derivingIList = Deriving[IList[String]].gen
  implicitly[derivingIList.TreeRepr =:= ((String :: HNil :: HNil) :+: HNil :+: CNil)]
  implicitly[derivingIList.FlatRepr =:= (String :: HNil)]

  val derivingdSnoc = Deriving[Snoc[String]].gen
  implicitly[derivingdSnoc.TreeRepr =:= ((HNil :: String :: HNil) :+: HNil :+: CNil)]
  implicitly[derivingdSnoc.FlatRepr =:= (String :: HNil)]

  val derivingdTree = Deriving[Tree[String]].gen
  implicitly[derivingdTree.TreeRepr =:= ((String :: HNil) :+: (HNil :: HNil :: HNil) :+: CNil)]
  implicitly[derivingdTree.FlatRepr =:= (String :: HNil)]

  /*assert*/(derivingIList.materialize[Show].show(ICons("foo", INil[String]())) == "[case: (foo, ([case: [case: +]], +))]")
  /*assert*/(derivingdSnoc.materialize[Show].show(SCons(SNil[String](), "bar")) == "[case: (SNil(), bar)]")
  /*assert*/(derivingdTree.materialize[Show].show(Node(Leaf("l1"), Leaf("l2"))) == "[case: [case: (Leaf(l1), Leaf(l2))]]")
}
