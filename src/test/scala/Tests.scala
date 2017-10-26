package deriving

object Tests {
  def main(args: Array[String]): Unit = {
    Benchmarks
    DeriveSTests
    Representable1Tests
    // LeavesTests
    NotGenericTests
    NotInTests
    println("Done")
  }
}
