package deriving

object NotInTests {
  implicitly[NotIn[HNil, Boolean]]
  implicitly[NotIn[String :: Int :: HNil, Boolean]]
  implicitly[NotIn[String :: Int :: HNil, Long]]
  // illTyped implicitly[NotIn[String :: Int :: HNil, String]]
  // illTyped implicitly[NotIn[String :: Int :: HNil, Int]]
}
