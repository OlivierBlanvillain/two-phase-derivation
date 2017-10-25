// package deriving

// object DeriveMTests {
//   import ADTs._
//   import Instances._
//   import Syntax._

//   implicit val derivingDogInstance: DeriveM.Aux[Dog, Long :: HNil] = new DeriveM[Dog] {
//     type Repr = Long :: HNil
//     def derive[F[_]](implicit l: LiftM[F, Repr], c: CanDerive[F]): F[Dog] = {
//       val (fLong :: HNil) = l.instances.asInstanceOf[F[Long] :: HNil]
//       fLong.imap(Dog.apply)(_.age)
//     }
//   }

//   // val deriveIDAABBS = DerivingM[IDAABBS].gen
//   // implicitly[deriveIDAABBS.Repr =:= (Int :: Double :: String :: String :: String :: HNil)]

//   // val showIDAABBS: Show[IDAABBS] = deriveIDAABBS.materialize[Show]
//   // /*assert*/(showIDAABBS.show(IDAABBSInstance) == IDAABBSShowResult)

//   val derivingDog = DerivingM[Dog].gen
//   implicitly[derivingDog.Repr =:= (Long :: HNil)]

//   val derivingCat = DerivingM[Cat].gen
//   val s: DeriveS[Cat, HNil]{ type Repr = String :: Long :: HNil } = derivingCat
//   implicitly[derivingCat.Repr =:= (String :: Long :: HNil)]

//   // val showCat: Show[Cat] = derivingCat.materialize[Show]
//   // /*assert*/println(showCat.show(Cat("sansan", Right(Dog(4))))) //  == "(sansan, [case: [case: 4]])")
//   // /*assert*/println(showCat.show(Cat("sansan", Left(Cat("aslan", Right(Dog(4))))))) //  == "(sansan, [case: (aslan, [case: [case: 4]])])")

//   // // TestDefns ----------------------------------------------------------------

//   // val derivingIList = DerivingM[IList[String]].gen
//   // implicitly[derivingIList.Repr =:= (String :: HNil)]

//   // val derivingdSnoc = DerivingM[Snoc[String]].gen
//   // implicitly[derivingdSnoc.Repr =:= (String :: HNil)]

//   // val derivingdTree = DerivingM[Tree[String]].gen
//   // implicitly[derivingdTree.Repr =:= (String :: HNil)]
// }
