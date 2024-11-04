package synnks.atoms.mapreduce.ops

import shapeless.*
import synnks.atoms.mapreduce.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create Compose[${K}, ${V}, ${KL}, ${VL-1}, ${VL}] instance.
The `V` type of the MapReduceFunction[..., ${VL-1}, ...] does not match the last element of the `V` HList of ChainedMapReduceFunction[..., ${V}]
""")
sealed trait Compose[K <: HList, V <: HList, KL, `VL-1`, VL] {
  type Out

  def apply(
    chainedMapReduceFn: ChainedMapReduceFunction[K, V],
    last: MapReduceFunction[KL, VL, `VL-1`]
  ): Out
}

object Compose {
  type Aux[K <: HList, V <: HList, KL, `VL-1`, VL, Out0] = Compose[K, V, KL, `VL-1`, VL] {
    type Out = Out0
  }

  implicit def composeLast[KH, `KH+1`, VH, `VH+1`, `VH+2`]
    : Compose.Aux[KH :: HNil, VH :: `VH+1` :: HNil, `KH+1`, `VH+1`, `VH+2`, ChainedMapReduceFunction[
      KH :: `KH+1` :: HNil,
      VH :: `VH+1` :: `VH+2` :: HNil
    ]] = new Compose[KH :: HNil, VH :: `VH+1` :: HNil, `KH+1`, `VH+1`, `VH+2`] {
    override type Out = ChainedMapReduceFunction[KH :: `KH+1` :: HNil, VH :: `VH+1` :: `VH+2` :: HNil]

    def apply(
      chainedMapReduceFn: ChainedMapReduceFunction[KH :: HNil, VH :: `VH+1` :: HNil],
      last: MapReduceFunction[`KH+1`, `VH+2`, `VH+1`]
    ): Out =
      chainedMapReduceFn match {
        case ChainedMapReduceFunction.Last(mapReduceFn) =>
          ChainedMapReduceFunction.Init(mapReduceFn, last)
      }
  }

  implicit def composeInit[
    KH,
    `KH+1`,
    KT <: HList,
    VH,
    `VH+1`,
    VT <: HList,
    KL,
    `VL-1`,
    VL,
    NKT <: HList,
    NVT <: HList
  ](implicit
    compose: Compose.Aux[`KH+1` :: KT, `VH+1` :: VT, KL, `VL-1`, VL, ChainedMapReduceFunction[
      `KH+1` :: NKT,
      `VH+1` :: NVT
    ]]
  ): Compose.Aux[KH :: `KH+1` :: KT, VH :: `VH+1` :: VT, KL, `VL-1`, VL, ChainedMapReduceFunction[
    KH :: `KH+1` :: NKT,
    VH :: `VH+1` :: NVT
  ]] = new Compose[KH :: `KH+1` :: KT, VH :: `VH+1` :: VT, KL, `VL-1`, VL] {
    override type Out = ChainedMapReduceFunction[KH :: `KH+1` :: NKT, VH :: `VH+1` :: NVT]

    override def apply(
      chainedMapReduceFn: ChainedMapReduceFunction[KH :: `KH+1` :: KT, VH :: `VH+1` :: VT],
      last: MapReduceFunction[KL, VL, `VL-1`]
    ): Out =
      chainedMapReduceFn match {
        case ChainedMapReduceFunction.Init(mapReduceFn, next) =>
          ChainedMapReduceFunction.Init(mapReduceFn, compose(next, last))
      }
  }
}
