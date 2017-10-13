package deriving

object NotGenericTests {
  implicitly[NotGeneric[String]]
  implicitly[NotGeneric[String :: Int :: HNil]]
  // illTyped implicitly[NotGeneric[ADTs.AA]]
  // illTyped implicitly[NotGeneric[ADTs.IDAABBS]]
}
