package deriving

object Instances {
  implicit val catsStdShowForString: Show[String] = new Show[String] { def show(x: String) = x }
  implicit val catsStdShowForDouble: Show[Double] = new Show[Double] { def show(x: Double) = x.toString }
  implicit val catsStdShowForInt: Show[Int]       = new Show[Int]    { def show(x: Int)    = x.toString }
  implicit val catsStdShowForLong: Show[Long]     = new Show[Long]   { def show(x: Long)   = x.toString }
}
