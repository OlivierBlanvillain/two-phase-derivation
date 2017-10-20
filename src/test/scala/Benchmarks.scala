package deriving

// import scala.concurrent.duration.Duration

object Benchmarks { // extends App {
  // Waiting for 2.3.2...

  import ADTs._
  import Instances._
  // import shapeless.test.{compileTime => c}
  // val deriveF = DerivingF[IDAABBS].gen

  // val deriveS = the[DeriveS[IDAABBS]]

  // def c(s: String): Duration = ???

  // def s(d: => Duration): String = s"        ${(d.toNanos / 1E9).toString.take(6)} seconds        |"

  // println("|          Scala Code           | " +     "       Compilation Time       |")
  // println("|-------------------------------| " +     "----------------------------- |")
  // println("|`Deriving[IDAABBS].gen        `| " + s(c("Deriving[IDAABBS].gen        ")))
  // println("|`deriveF.materialize[Show]    `| " + s(c("deriveF.materialize[Show]    ")))
  // println("|`the[DeriveS[IDAABBS]]        `| " + s(c("the[DeriveS[IDAABBS]]        ")))
  // println("|`deriveS.materialize[Show]    `| " + s(c("deriveS.materialize[Show]    ")))
  // println("|`the[TShow[IDAABBS]]          `| " + s(c("the[TShow[IDAABBS]]          ")))


  // Deriving[IDAABBS].gen

  // deriveF.materialize[Show]

  // the[DeriveS[IDAABBS]]

  // deriveS.materialize[Show]

  // the[TShow[IDAABBS]]

  {
    import DirectShowDerivation._
    the[Show[Cat]]
  }
}

object DirectShowDerivation {

  implicit def showGeneric[F, G]
    (implicit
      gen: Generic[F] { type Repr = G },
      sg: => Show[G]
    ): Show[F] =
      new Show[F] {
        def show(f: F) = sg.show(gen.to(f))
      }

  implicit def showHNil: Show[HNil] =
    new Show[HNil] {
      def show(p: HNil): String = ""
    }

  implicit def showHCons[H, T <: HList]
    (implicit
      sv: => Show[H],
      st: => Show[T]
    ): Show[H :: T] =
      new Show[H :: T] {
        def show(p: H :: T): String = {
          val head = sv.show(p.head)
          val tail = st.show(p.tail)
          if(tail.isEmpty) head else s"($head, $tail)"
        }
      }

  implicit def showCNil: Show[CNil] =
    new Show[CNil] {
      def show(p: CNil): String = ""
    }

  implicit def showCCons[H, T <: Coproduct]
    (implicit
      sv: => Show[H],
      st: => Show[T]
    ): Show[H :+: T] =
      new Show[H :+: T] {
        def show(c: H :+: T): String =
          c match {
            case Inl(l) => s"[case: ${sv.show(l)}]"
            case Inr(r) => st.show(r)
          }
      }
}
