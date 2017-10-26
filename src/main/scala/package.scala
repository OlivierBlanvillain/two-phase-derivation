package object deriving {
  implicit class ProdSyntax1[T[t] <: Prod[t], X](t: T[X]) extends AnyVal {
    def :*:[H[_]](h: H[X]): Prod.C[H, T, X] = Prod.C(h, t)
  }
  implicit class ProdSyntax2[H[_], T[t] <: Prod[t], X](t: Prod.C[H, T, X]) extends AnyVal {
    def :*:[U[_]](h: U[X]): Prod.C[U, H :*: T, X] = Prod.C(h, t)
  }
  object :*: {
    def unapply[H[_], T[t] <: Prod[t], X](s: Prod.C[H, T, X]) = (s.head, s.tail)
  }
  type :*:[H[_], T[t] <: Prod[t]] = [X] => Prod.C[H, T, X]
  type :-:[H[_], T[t] <: Sum[t]] = [X] => Sum.N[H, T, X]

  type Id[t] = t
  type Const[t] = [X] => t
}
