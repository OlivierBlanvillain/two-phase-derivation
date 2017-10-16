package deriving

object DeriveFTests {
  import ADTs._

  // Non Recursive HList/Coproduct structure ----------------------------------

  val derivingIDAABS = DerivingF[IDAABBS].gen

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

  // illTyped derivingIDAABS.materialize[Show]
  // "could not find implicit value for parameter I1: deriving.Show\\[Int\\].*"

  import Instances._

  val showIDAABBS: Show[IDAABBS] = derivingIDAABS.materialize[Show]
  /*assert*/(showIDAABBS.show(IDAABBSInstance) == IDAABBSShowResult)

  // Either Recursion ---------------------------------------------------------

  val deriveingCat = DerivingF[Cat].gen
  implicitly[deriveingCat.TreeRepr =:= (String :: ((HNil :: HNil) :+: ((Long :: HNil) :: HNil) :+: CNil) :: HNil)]
  implicitly[deriveingCat.FlatRepr =:= (String :: Long :: HNil)]

  val showCat: Show[Cat] = deriveingCat.materialize[Show]
  /*assert*/println(showCat.show(Cat("sansan", Right(Dog(4))))) //  == "(sansan, [case: [case: 4]])")
  /*assert*/println(showCat.show(Cat("sansan", Left(Cat("aslan", Right(Dog(4))))))) //  == "(sansan, [case: (aslan, [case: [case: 4]])])")

  // TestDefns ----------------------------------------------------------------

  val derivingIList = DerivingF[IList[String]].gen
  implicitly[derivingIList.TreeRepr =:= ((String :: HNil :: HNil) :+: HNil :+: CNil)]
  implicitly[derivingIList.FlatRepr =:= (String :: HNil)]

  val derivingdSnoc = DerivingF[Snoc[String]].gen
  implicitly[derivingdSnoc.TreeRepr =:= ((HNil :: String :: HNil) :+: HNil :+: CNil)]
  implicitly[derivingdSnoc.FlatRepr =:= (String :: HNil)]

  val derivingdTree = DerivingF[Tree[String]].gen
  implicitly[derivingdTree.TreeRepr =:= ((String :: HNil) :+: (HNil :: HNil :: HNil) :+: CNil)]
  implicitly[derivingdTree.FlatRepr =:= (String :: HNil)]

  // /*assert*/(derivingIList.materialize[Show].show(ICons("foo", INil[String]())) == "[case: (foo, ([case: [case: +]], +))]")
  /*assert*/(derivingdSnoc.materialize[Show].show(SCons(SNil[String](), "bar")) == "[case: (SNil(), bar)]")
  /*assert*/(derivingdTree.materialize[Show].show(Node(Leaf("l1"), Leaf("l2"))) == "[case: [case: (Leaf(l1), Leaf(l2))]]")
}
