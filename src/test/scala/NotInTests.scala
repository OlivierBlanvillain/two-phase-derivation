package deriving

object NotInTests {
  implicitly[NotIn[HNil, Boolean]]
  implicitly[NotIn[String :: Int :: HNil, Boolean]]
  implicitly[NotIn[String :: Int :: HNil, Long]]
  // illTyped implicitly[NotIn[String :: Int :: HNil, String]]
  // illTyped implicitly[NotIn[String :: Int :: HNil, Int]]

  implicitly[NotIn1[Prod.N, Const[Boolean]]]
  implicitly[NotIn1[Const[String] :*: Const[Int] :*: Prod.N, Const[Boolean]]]
  implicitly[NotIn1[Const[String] :*: Const[Int] :*: Prod.N, Const[Long]]]
  // illTyped implicitly[NotIn1[String :*: Int :: Prod.N, String]]
  // illTyped implicitly[NotIn1[String :*: Int :: Prod.N, Int]]
}
