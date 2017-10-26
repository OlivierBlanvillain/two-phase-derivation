// package deriving

// object LeavesTests {
//   type TestTree = Int :: (Double :: (Boolean :+: Byte :+: CNil) :: HNil) :: String :: HNil
//   type Expected = Int :: Double :: Boolean :: Byte :: String :: HNil

//   val leafs = the[Leaves[TestTree]]
//   implicitly[leafs.FlatRepr =:= Expected]
// }
