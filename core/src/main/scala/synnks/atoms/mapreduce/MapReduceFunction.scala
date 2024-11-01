package synnks.atoms.mapreduce

import cats.Semigroup

final class MapReduceFunction[K, V, R](map: (K, V) => R, reduce: (R, R) => R)

object MapReduceFunction {

  def apply[K, V, R: Semigroup](map: (K, V) => R): MapReduceFunction[K, V, R] =
    new MapReduceFunction(map, Semigroup[R].combine)
}
