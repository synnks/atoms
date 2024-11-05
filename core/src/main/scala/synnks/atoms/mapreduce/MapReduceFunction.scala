package synnks.atoms.mapreduce

import cats.Semigroup
import cats.data.NonEmptyMap

final class MapReduceFunction[K, V, R](map: (K, V) => R, reduce: (R, R) => R) {
  def apply(nonEmptyMap: NonEmptyMap[K, V]): R = nonEmptyMap.transform(map).reduceLeft(reduce)
}

object MapReduceFunction {

  def apply[K, V, R: Semigroup](map: (K, V) => R): MapReduceFunction[K, V, R] =
    new MapReduceFunction(map, Semigroup[R].combine)
}
