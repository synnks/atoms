package synnks.atoms.mapreduce

import shapeless.*

sealed trait ChainedMapReduceFunction[K <: HList, V <: HList] extends Product with Serializable

object ChainedMapReduceFunction {

  case class Last[KH, VH, `VH+1`](mapReduceFn: MapReduceFunction[KH, `VH+1`, VH])
      extends ChainedMapReduceFunction[KH :: HNil, VH :: `VH+1` :: HNil]

  case class Init[KH, KT <: HList, VH, `VH+1`, VT <: HList](
    mapReduceFn: MapReduceFunction[KH, `VH+1`, VH],
    next: ChainedMapReduceFunction[KT, `VH+1` :: VT]
  ) extends ChainedMapReduceFunction[KH :: KT, VH :: `VH+1` :: VT]
}
