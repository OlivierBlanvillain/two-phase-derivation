package deriving

sealed trait Prod[X]
object Prod {
  final case class C[H[_], T[t] <: Prod[t], X](head: H[X], tail: T[X]) extends Prod[X]
  final case class N[X]() extends Prod[X]
}

sealed trait Sum[X]
object Sum  {
  sealed trait N[H[_], T[t] <: Sum[t], X] extends Sum[X]
  final case class L[H[_], T[t] <: Sum[t], X](head: H[X]) extends N[H, T, X]
  final case class R[H[_], T[t] <: Sum[t], X](tail: T[X]) extends N[H, T, X]
  sealed trait E[X] extends Sum[X]
}

trait Representable[A] {
  type Repr[t]

  def to[T](a: A): Repr[T]
  def from[T](r: Repr[T]): A
}

trait Representable1[A[_]] {
  type Repr[t]

  def to[T](a: A[T]): Repr[T]
  def from[T](r: Repr[T]): A[T]
}
