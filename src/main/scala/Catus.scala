package felis.catus

import cats.Cartesian
import cats.data.Xor
import cats.functor.Invariant
import scala.reflect.ClassTag
import simulacrum.typeclass

// `Cartesian` for `Xor`.
@typeclass trait DisjointCartesian[F[_]] {
  def disjoint[A, B](fa: F[A], fb: F[B]): F[Xor[A, B]]
}

object DisjointCartesianSyntax {
  implicit class DisjointCartesianOps[F[_], A: ClassTag](fa: F[A]) extends DisjointCartesianBuilder {
    def |#|[B: ClassTag](fb: F[B]): DisjointCartesianBuilder[F]#DisjointCartesianBuilder2[A, B] =
      new DisjointCartesianBuilder[F] |#| fa |#| fb
  }
}

@typeclass trait Catus[F[_]] extends Cartesian[F] with DisjointCartesian[F] with Invariant[F]
