package synnks.atoms.mapreduce.ops

import synnks.atoms.hlist.*
import synnks.atoms.mapreduce.*

private[atoms] object AndThenCompat {
  def apply[
    K1 <: HList,
    IR1 <: HList,
    R1,
    K2H,
    K2T <: HList,
    IR2H,
    IR2T <: HList,
    R2,
    NK <: HList,
    NIR <: HList
  ](
    f: MapReduceFunction[K1, IR1, R1],
    g: MapReduceFunction[K2H :: K2T, IR2H :: IR2T, R2],
    andThen: AndThen.Aux[K1, IR1, R1, K2T, IR2T, IR2H, MapReduceFunction[NK, NIR, IR2H]]
  ): MapReduceFunction[K2H :: NK, IR2H :: NIR, R2] =
    g match {
      case MapReduceFunction.Chain(map, reduce, next) =>
        MapReduceFunction(map, reduce, andThen(f, next))
    }
}
