package synnks.atoms.mapreduce

import shapeless.*
import synnks.atoms.mapreduce.ops.*

sealed trait ChainedMapReduceFunction[K <: HList, V <: HList] extends Product with Serializable {
  def andThen[KH, VH, `VH+1`](mapReduceFn: MapReduceFunction[KH, `VH+1`, VH])(implicit
    andThen: AndThen[K, V, KH, VH, `VH+1`]
  ): andThen.Out = andThen(this, mapReduceFn)
}

object ChainedMapReduceFunction {

  case class Last[KH, VH, `VH+1`](mapReduceFn: MapReduceFunction[KH, `VH+1`, VH])
      extends ChainedMapReduceFunction[KH :: HNil, VH :: `VH+1` :: HNil]

  case class Init[KH, `KH+1`, KT <: HList, VH, `VH+1`, VT <: HList](
    mapReduceFn: MapReduceFunction[KH, `VH+1`, VH],
    next: ChainedMapReduceFunction[`KH+1` :: KT, `VH+1` :: VT]
  ) extends ChainedMapReduceFunction[KH :: `KH+1` :: KT, VH :: `VH+1` :: VT]
}
