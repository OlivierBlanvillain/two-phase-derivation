package felis.catus.lib

import simulacrum.typeclass

@typeclass trait Objects[F[_]] extends Products[F] with Coproducts[F]
