object Model {
  sealed trait AABB
  case class AA(a: String) extends AABB
  case class BB(a: String) extends AABB
  case class DAABB(d: Double, aabb: AABB)
  case class IDAABBS(i: Int, daabb: DAABB, s: String)

  val instance: IDAABBS = IDAABBS(1, DAABB(1.1, AA("aa")), "s")
  val showResult: String = "(1, ((1.1, [case: aa]), s))"
}
