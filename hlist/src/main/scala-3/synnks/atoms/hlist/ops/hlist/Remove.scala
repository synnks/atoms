package synnks.atoms.hlist.ops.hlist

import synnks.atoms.hlist.*

trait Remove[L <: HList, E] {
  type Out

  def apply(hlist: L): Out
}

object Remove extends LowPriorityRemove {
  type Aux[L <: HList, E, Out0] = Remove[L, E] { type Out = Out0 }

  given removeHead[H, T <: HList]: Remove[H :: T, H] with {
    override type Out = (H, T)

    override def apply(hlist: H :: T): Out = (hlist.head, hlist.tail)
  }
}

private[hlist] trait LowPriorityRemove {
  given removeTail[H, T <: HList, E, Removed, Rest <: HList](using
    remove: Remove.Aux[T, E, (Removed, Rest)]
  ): Remove[H :: T, E] with {
    override type Out = (Removed, H :: Rest)

    override def apply(hlist: H :: T): Out = {
      val (removed, rest) = remove(hlist.tail)
      (removed, hlist.head *: rest)
    }
  }
}
