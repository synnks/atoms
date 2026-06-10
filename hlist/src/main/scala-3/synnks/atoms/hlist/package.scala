package synnks.atoms

package object hlist {
  type HList = Tuple
  type HNil  = EmptyTuple
  val HNil: EmptyTuple = EmptyTuple

  type ::[H, T <: HList] = H *: T

  extension [H](head: H) {
    def ::[L <: HList](hlist: L): H *: L = head *: hlist
  }

  extension [L <: HList](hlist: L) {
    def removeElem[E](using remove: ops.hlist.Remove[L, E]): remove.Out = remove(hlist)
  }
}
