package synnks.atoms.mapreduce

import cats.Semigroup
import shapeless.*
import synnks.atoms.mapreduce.ops.*

sealed trait MapReduceFunction[K <: HList, IR <: HList, R] extends Product with Serializable {

  def andThen[K2 <: HList, IR2 <: HList, R2](g: MapReduceFunction[K2, IR2, R2])(implicit
    andThen: AndThen[K, IR, R, K2, IR2, R2]
  ): andThen.Out = andThen(this, g)
}

object MapReduceFunction {

  private[atoms] case class Unit[R]() extends MapReduceFunction[HNil, HNil, R]

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
  ): MapReduceFunction[K :: HNil, IR :: HNil, R] = Chain(map, reduce, Unit())

  def apply[K, IR, R: Semigroup](
    map: (K, IR) => R
  ): MapReduceFunction[K :: HNil, IR :: HNil, R] = Chain(map, Semigroup[R].combine, Unit())
}
