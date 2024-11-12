package synnks.atoms

import cats.Semigroup
import shapeless.*
import synnks.atoms.mapreduce.syntax.*

package object mapreduce {

  implicit def toMapReduceFunction[KH, IRH, R: Semigroup](
    f: (KH, IRH) => R
  ): MapReduceFunction[KH :: HNil, IRH :: HNil, R] = MapReduceFunction(f)

  implicit def toMapReduceFunctionOps[KH, IRH, R: Semigroup](
    f: (KH, IRH) => R
  ): MapReduceFunctionOps[KH :: HNil, IRH :: HNil, R] = toMapReduceFunction(f)
}
