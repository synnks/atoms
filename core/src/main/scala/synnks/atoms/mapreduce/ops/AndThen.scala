package synnks.atoms.mapreduce.ops

import shapeless.*
import synnks.atoms.mapreduce.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create AndThen[${K}, ${V}, ${KH}, ${VH}, ${VH+1}] instance.
The `V` type of the MapReduceFunction[..., ${VH+1}, ...] does not match the head of the `V` HList of ChainedMapReduceFunction[..., ${V}]
""")
sealed trait AndThen[K <: HList, V <: HList, KH, VH, `VH+1`] {
  type Out

  def apply(
    chainedMapReduceFn: ChainedMapReduceFunction[K, V],
    first: MapReduceFunction[KH, `VH+1`, VH]
  ): Out
}

object AndThen {
  type Aux[K <: HList, V <: HList, KH, VH, `VH+1`, Out0] = AndThen[K, V, KH, VH, `VH+1`] {
    type Out = Out0
  }

  @inline def apply[K <: HList, V <: HList, KH, VH, `VH+1`](implicit
    instance: AndThen[K, V, KH, VH, `VH+1`]
  ): AndThen.Aux[K, V, KH, VH, `VH+1`, instance.Out] = instance

  implicit def andThen[KH, `KH+1`, KT <: HList, VH, `VH+1`, VT <: HList]
    : AndThen.Aux[`KH+1` :: KT, `VH+1` :: VT, KH, VH, `VH+1`, ChainedMapReduceFunction[
      KH :: `KH+1` :: KT,
      VH :: `VH+1` :: VT
    ]] =
    new AndThen[`KH+1` :: KT, `VH+1` :: VT, KH, VH, `VH+1`] {
      override type Out = ChainedMapReduceFunction[KH :: `KH+1` :: KT, VH :: `VH+1` :: VT]

      override def apply(
        chainedMapReduceFn: ChainedMapReduceFunction[`KH+1` :: KT, `VH+1` :: VT],
        first: MapReduceFunction[KH, `VH+1`, VH]
      ): Out =
        ChainedMapReduceFunction.Init(first, chainedMapReduceFn)
    }
}
