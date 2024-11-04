package synnks.atoms

import shapeless.*

package object mapreduce {

  implicit def toChainedMapReduceFunction[KH, VH, `VH+1`](
    mapReduceFn: MapReduceFunction[KH, `VH+1`, VH]
  ): ChainedMapReduceFunction[KH :: HNil, VH :: `VH+1` :: HNil] =
    ChainedMapReduceFunction.Last(mapReduceFn)
}
