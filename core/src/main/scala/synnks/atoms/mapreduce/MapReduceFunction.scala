package synnks.atoms.mapreduce

import cats.Semigroup
import shapeless.*

sealed trait MapReduceFunction[K <: HList, IR <: HList, +R] extends Product with Serializable

object MapReduceFunction {

  import synnks.atoms.mapreduce.syntax.*

  private[atoms] case object Unit extends MapReduceFunction[HNil, HNil, Nothing]

  private[atoms] case class Chain[KH, KT <: HList, IRH, IRT <: HList, R](
    map: (KH, IRH) => R,
    reduce: (R, R) => R,
    next: MapReduceFunction[KT, IRT, IRH]
  ) extends MapReduceFunction[KH :: KT, IRH :: IRT, R]

  def apply[KH, KT <: HList, IRH, IRT <: HList, R](
    map: (KH, IRH) => R,
    reduce: (R, R) => R,
    next: MapReduceFunction[KT, IRT, IRH]
  ): MapReduceFunction[KH :: KT, IRH :: IRT, R] = Chain(map, reduce, next)

  def apply[KH, KT <: HList, IRH, IRT <: HList, R: Semigroup](
    map: (KH, IRH) => R,
    next: MapReduceFunction[KT, IRT, IRH]
  ): MapReduceFunction[KH :: KT, IRH :: IRT, R] = Chain(map, Semigroup[R].combine, next)

  def apply[K, IR, R](
    map: (K, IR) => R,
    reduce: (R, R) => R
  ): MapReduceFunction[K :: HNil, IR :: HNil, R] = Chain(map, reduce, Unit)

  def apply[K, IR, R: Semigroup](
    map: (K, IR) => R
  ): MapReduceFunction[K :: HNil, IR :: HNil, R] = Chain(map, Semigroup[R].combine, Unit)

  implicit def mapReduceFunctionOps[K <: HList, IR <: HList, R](f: MapReduceFunction[K, IR, R]): MapReduceFunctionOps[K, IR, R] =
    new MapReduceFunctionOps(f)
}
