import sbt._

object Boilerplate {
  import scala.StringContext._

  implicit final class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map { _ dropWhile (_.isWhitespace) }
      trimmedLines mkString "\n"
    }
  }

  val templates: Seq[Template] = Seq(
    GenEitherN,
    GenDisjointCartesianBuilder
  )

  val header = "// auto-generated boilerplate"

  def gen(dir : File) = for(t <- templates) yield {
    val tgtFile = t.filename(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxArity = 2

  final class TemplateVals(val arity: Int) {
    val synTypes     = (0 until arity) map (n => s"A$n")
    val synVals      = (0 until arity) map (n => s"a$n")
    val synTypedVals = (synVals zip synTypes) map { case (v,t) => v + ":" + t}
    val `+A..+N`     = synTypes.map("+".+).mkString(", ")
    val `A..N`       = synTypes.mkString(", ")
    val `a..n`       = synVals.mkString(", ")
    val `_.._`       = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)`     = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`     = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`     = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`   = synTypedVals mkString ", "
  }

  trait Template {
    def filename(root: File):File
    def content(tv: TemplateVals): String
    def range = 1 to maxArity
    def body: String = {
      val headerLines = header split '\n'
      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty) }
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {_ filter (_ startsWith "-") map (_.tail) }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (headerLines ++ preBody ++ instances ++ postBody) mkString "\n"
    }
  }

  object GenEitherN extends Template {
    def filename(root: File) = root / "EitherN.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val eitherList: String =
        (arity to maxArity).map { n =>
          (1 to n).map(i => if (i == arity) "A" else "N").mkString(s"Either$n[", ",", "]")
        }.reduce(_ + " with " + _)

      block"""
        |package felis.catus.lib
        |
        |object NothingTypeAlias { type N = Nothing }
        |import NothingTypeAlias.N
        |
        -sealed trait Either${arity}[${`+A..+N`}] extends Product with Serializable
        -
        -final case class Case${arity}[+A](value: A) extends ${eitherList}
        -
        |
      """
    }
  }

  object GenDisjointCartesianBuilder extends Template {
    def filename(root: File) = root / "DisjointCartesianBuilder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val `A: ClassTag..N: ClassTag` = synTypes.map(_ + ": ClassTag").mkString(", ")

      val tpes = synTypes map { tpe => s"F[$tpe]" }
      val params = (synVals zip tpes) map { case (v, t) => s"$v: $t"} mkString ", "

      val next =
        if (arity + 1 <= maxArity)
          s"def |#|[Z: ClassTag](z: F[Z]) = new DisjointCartesianBuilder${arity + 1}(${`a..n`}, z)"
        else
          "// no next method at this arity"

      val evidences = (0 until arity) map (n => s"ev$n: A$n <:< Z") mkString ", "

      val xors =
        (0 until arity).map { n =>
          (0 until n).foldRight(if (n == arity - 1) "x" else "Xor.Left(x)") {
            (_, acc) => s"Xor.Right($acc)"
          }
        }

      val nestedDisjonctions =
        (0 until (arity - 2)).foldRight(s"dis.disjoint(a${arity - 2}, a${arity - 1})") {
          (i, acc) => s"dis.disjoint(a$i, $acc)"
        }

      val fromXors = xors.zipWithIndex.map { case (xor, i) => s"case $xor => ev$i(x)" }.mkString("; ")

      val toXors = xors.zipWithIndex.map { case (xor, i) => s"case x: A$i => $xor" }.mkString("; ")

      val as =
        if (arity <= 1) "// no as method at this arity"
        else s"def as[Z](implicit inv: Invariant[F], dis: DisjointCartesian[F], ${evidences}): F[Z] = inv.imap(${nestedDisjonctions}) { ${fromXors} } { ${toXors} }"

      val xorsCases = xors.zipWithIndex.map { case (xor, i) => s"case ${xor} => Case${i + 1}(x)" }.mkString("; ")

      val casesXors = xors.zipWithIndex.map { case (xor, i) => s"case Case${i + 1}(x) => ${xor}" }.mkString("; ")

      val disjointed =
        if (arity <= 1) "// no disjointed method at this arity"
        else s"def disjointed(implicit inv: Invariant[F], dis: DisjointCartesian[F]): F[Either${arity}[${`A..N`}]] = inv.imap(${nestedDisjonctions}) { ${xorsCases} } { ${casesXors} }"

      block"""
        |package felis.catus.lib
        |
        |import cats.functor.Invariant
        |import cats.data.Xor
        |import scala.reflect.ClassTag
        |
        |class DisjointCartesianBuilder[F[_]] {
        |  def |#|[Z: ClassTag](a: F[Z]) = new DisjointCartesianBuilder1(a)
        |
        -  final class DisjointCartesianBuilder$arity[${`A: ClassTag..N: ClassTag`}]($params) {
        -    $next
        -
        -    $as
        -
        -    $disjointed
        -  }
        -
        |}
      """
    }
  }
}
