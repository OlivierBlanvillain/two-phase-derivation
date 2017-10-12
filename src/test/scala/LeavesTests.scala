package deriving

import shapeless._

object LeavesTest {
  type TestTree = Int :: (Double :: (Boolean :+: Byte :+: CNil) :: HNil) :: String :: HNil
  type Expected = Int :: Double :: Boolean :: Byte :: String :: HNil

  val leafs = the[Leaves[TestTree]]
  implicitly[leafs.FlatRepr =:= Expected]
}
