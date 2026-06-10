package synnks.atoms.ops

import synnks.atoms.*
import synnks.atoms.hlist.*
import synnks.atoms.mapreduce.*

private[atoms] object MapReduceCompat {
  def apply[GH, GT <: HList, K <: HList, V, IRH, IRT <: HList, R](
    groupedAtoms: GroupedAtoms[GH :: GT, K, V],
    f: MapReduceFunction[GH :: GT, IRH :: IRT, R],
    mapReduce: MapReduce[GT, K, V, IRT, IRH]
  ): R =
    f match {
      case MapReduceFunction.Chain(map, reduce, next) =>
        groupedAtoms match {
          case NestedAtoms(atoms) =>
            atoms
              .map(mapReduce(_, next))
              .transform(map)
              .reduceLeft(reduce)
        }
    }
}
