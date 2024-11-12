package synnks.atoms.mapreduce.syntax

import shapeless.*
import synnks.atoms.mapreduce.MapReduceFunction
import synnks.atoms.mapreduce.ops.*

final class MapReduceFunctionOps[K <: HList, IR <: HList, R](val f: MapReduceFunction[K, IR, R]) extends AnyVal {

  def andThen[K2 <: HList, IR2 <: HList, R2](g: MapReduceFunction[K2, IR2, R2])(implicit
    andThen: AndThen[K, IR, R, K2, IR2, R2]
  ): andThen.Out = andThen(f, g)

  def compose[K2 <: HList, IR2 <: HList, R2](g: MapReduceFunction[K2, IR2, R2])(implicit
    andThen: AndThen[K2, IR2, R2, K, IR, R]
  ): andThen.Out = andThen(g, f)
}
