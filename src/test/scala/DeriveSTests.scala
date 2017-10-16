package deriving

object DeriveSTests {
  import ADTs._
  import Instances._

  val deriveIDAABBS = DerivingS[IDAABBS].gen
  implicitly[deriveIDAABBS.Repr =:= (Int :: Double :: String :: String :: String :: HNil)]

  val showIDAABBS: Show[IDAABBS] = deriveIDAABBS.materialize[Show]
  /*assert*/(showIDAABBS.show(IDAABBSInstance) == IDAABBSShowResult)

  val deriveingDog = DerivingS[Dog].gen
  implicitly[deriveingDog.Repr =:= (Long :: HNil)]

  val deriveingCat = DerivingS[Cat].gen
  implicitly[deriveingCat.Repr =:= (String :: Long :: HNil)]

  val showCat: Show[Cat] = deriveingCat.materialize[Show]
  /*assert*/println(showCat.show(Cat("sansan", Right(Dog(4))))) //  == "(sansan, [case: [case: 4]])")
  /*assert*/println(showCat.show(Cat("sansan", Left(Cat("aslan", Right(Dog(4))))))) //  == "(sansan, [case: (aslan, [case: [case: 4]])])")

  // TestDefns ----------------------------------------------------------------

  // val derivingIList = DerivingS[IList[String]].gen
  // implicitly[derivingIList.Repr =:= (String :: HNil)]

  // val derivingdSnoc = DerivingS[Snoc[String]].gen
  // implicitly[derivingdSnoc.Repr =:= (String :: HNil)]

  // val derivingdTree = DerivingS[Tree[String]].gen
  // implicitly[derivingdTree.Repr =:= (String :: HNil)]
}
