// import cats.Show
// import cats.std.all._
// import Model._
// import shapeless._
// import shapeless.test.{compileTime => c}
// import scala.concurrent.duration.Duration

// object Benchmarks extends App {
//   val deriveF = the[DeriveF[IDAABBS]]
//   val deriveS = the[DeriveS[IDAABBS]]
//   val deriveFFlat = deriveF.flatten

//   def s(d: => Duration): String = s"        ${(d.toNanos / 1E9).toString.take(6)} seconds        |"

//   println("|          Scala Code           | " +     "       Compilation Time       |")
//   println("|-------------------------------| " +     "----------------------------- |")
//   println("|`the[DeriveF[IDAABBS]]        `| " + s(c("the[DeriveF[IDAABBS]]        ")))
//   println("|`deriveF.flatten              `| " + s(c("deriveF.flatten              ")))
//   println("|`deriveFFlat.materialize[Show]`| " + s(c("deriveFFlat.materialize[Show]")))
//   println("|`the[DeriveS[IDAABBS]]        `| " + s(c("the[DeriveS[IDAABBS]]        ")))
//   println("|`deriveS.materialize[Show]    `| " + s(c("deriveS.materialize[Show]    ")))
//   println("|`the[TShow[IDAABBS]]          `| " + s(c("the[TShow[IDAABBS]]          ")))
// }

// trait TShow[F] {
//   def show(f: F): String
// }

// /** Traditional Shapeless type class derivation */
// object TShow {
//   implicit def fromCatsShow[A](implicit s: Show[A]): TShow[A] =
//     new TShow[A] {
//       def show(a: A): String = s.show(a)
//     }

//   implicit def showGeneric[F, G](implicit gen: Generic.Aux[F, G], sg: Lazy[TShow[G]]): TShow[F] =
//     new TShow[F] {
//       def show(f: F) = sg.value.show(gen.to(f))
//     }

//   implicit def showHNil: TShow[HNil] =
//     new TShow[HNil] {
//       def show(p: HNil): String = ""
//     }

//   implicit def showHCons[H, T <: HList]
//     (implicit
//       sv: Lazy[TShow[H]],
//       st: Lazy[TShow[T]]
//     ): TShow[H :: T] =
//       new TShow[H :: T] {
//         def show(p: H :: T): String = {
//           val head = sv.value.show(p.head)
//           val tail = st.value.show(p.tail)
//           if(tail.isEmpty) head else s"($head, $tail)"
//         }
//       }

//   implicit def showCNil: TShow[CNil] =
//     new TShow[CNil] {
//       def show(p: CNil): String = ""
//     }

//   implicit def showCCons[H, T <: Coproduct]
//     (implicit
//       sv: Lazy[TShow[H]],
//       st: Lazy[TShow[T]]
//     ): TShow[H :+: T] =
//       new TShow[H :+: T] {
//         def show(c: H :+: T): String =
//           c match {
//             case Inl(l) => s"[case: ${sv.value.show(l)}]"
//             case Inr(r) => st.value.show(r)
//           }
//       }
// }
