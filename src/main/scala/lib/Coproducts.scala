package felis.catus.lib

import cats.functor.Invariant
import simulacrum.typeclass

@typeclass trait Coproducts[F[_]] extends DisjointCartesian[F] with Invariant[F]
