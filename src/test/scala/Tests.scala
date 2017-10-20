package deriving

object Tests {
  def main(args: Array[String]): Unit = {
    Benchmarks
    DeriveFTests
    DeriveSTests
    LeavesTests
    NotGenericTests
    NotInTests
    println("Done")
  }
}
