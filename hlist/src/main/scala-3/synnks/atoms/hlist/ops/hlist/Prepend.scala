package synnks.atoms.hlist.ops.hlist

import synnks.atoms.hlist.*

trait Prepend[L <: HList, R <: HList] {
  type Out <: HList

  def apply(left: L, right: R): Out
}

object Prepend {
  type Aux[L <: HList, R <: HList, Out0 <: HList] = Prepend[L, R] { type Out = Out0 }

  given tuplePrepend[L <: HList, R <: HList]: Prepend[L, R] with {
    override type Out = Tuple.Concat[L, R]

    override def apply(left: L, right: R): Out = left ++ right
  }
}
