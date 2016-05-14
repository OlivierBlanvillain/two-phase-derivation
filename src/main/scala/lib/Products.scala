package felis.catus.lib

import cats.Cartesian
import cats.functor.Invariant
import simulacrum.typeclass

@typeclass trait Products[F[_]] extends Cartesian[F] with Invariant[F]
