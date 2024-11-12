package synnks.atoms

import cats.Semigroup
import shapeless.*

package object mapreduce {

  implicit def toMapReduceFunction[KH, IRH, R: Semigroup](
    f: (KH, IRH) => R
  ): MapReduceFunction[KH :: HNil, IRH :: HNil, R] = MapReduceFunction(f)
}
