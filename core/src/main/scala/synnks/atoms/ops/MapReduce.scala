package synnks.atoms.ops

import shapeless.*
import synnks.atoms.*
import synnks.atoms.mapreduce.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create MapReduce[${G}, ${K}, ${V}, ${IR}, ${R}] instance.
The type of the last element of ${IR} needs to be Atoms[${K}, ${V}].
""")
sealed trait MapReduce[G <: HList, K <: HList, V, IR <: HList, R] {
  def apply(groupedAtoms: GroupedAtoms[G, K, V], f: MapReduceFunction[G, IR, R]): R
}

object MapReduce {

  @inline def apply[G <: HList, K <: HList, V, IR <: HList, R](implicit
    instance: MapReduce[G, K, V, IR, R]
  ): MapReduce[G, K, V, IR, R] = instance

  implicit def mapReduceBase[K <: HList, V]: MapReduce[HNil, K, V, HNil, Atoms[K, V]] =
    new MapReduce[HNil, K, V, HNil, Atoms[K, V]] {

      override def apply(
        groupedAtoms: GroupedAtoms[HNil, K, V],
        f: MapReduceFunction[HNil, HNil, Atoms[K, V]]
      ): Atoms[K, V] = groupedAtoms.ungroupBy[HNil]
    }

  implicit def mapReduceRecurse[GH, GT <: HList, K <: HList, V, IRH, IRT <: HList, R](implicit
    mapReduce: MapReduce[GT, K, V, IRT, IRH]
  ): MapReduce[GH :: GT, K, V, IRH :: IRT, R] =
    new MapReduce[GH :: GT, K, V, IRH :: IRT, R] {

      override def apply(
        groupedAtoms: GroupedAtoms[GH :: GT, K, V],
        f: MapReduceFunction[GH :: GT, IRH :: IRT, R]
      ): R = f match {
        case f: MapReduceFunction.Chain[GH, GT, IRH, IRT, R] =>
          groupedAtoms match {
            case NestedAtoms(atoms) =>
              atoms
                .map(mapReduce(_, f.next))
                .transform(f.map)
                .reduceLeft(f.reduce)
          }
      }
    }

  final class PartiallyApplied[G <: HList, K <: HList, V, R](val groupedAtoms: GroupedAtoms[G, K, V]) extends AnyVal {
    def apply[IR <: HList](f: MapReduceFunction[G, IR, R])(implicit mapReduce: MapReduce[G, K, V, IR, R]): R =
      mapReduce(groupedAtoms, f)
  }
}
