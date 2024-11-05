package synnks.atoms.ops

import shapeless.*
import synnks.atoms.*
import synnks.atoms.mapreduce.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create MapReduce[${L}, ${G}, ${K}, ${V}] instance.
The type of the last element of ${L} needs to be Atoms[shapeless.HNil, ${V}].
""")
sealed trait MapReduce[L <: HList, G <: HList, K <: HList, V] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V], chainedMapReduceFn: ChainedMapReduceFunction[G, L]): Out
}

object MapReduce {
  type Aux[L <: HList, G <: HList, K <: HList, V, Out0] = MapReduce[L, G, K, V] { type Out = Out0 }

  @inline def apply[L <: HList, G <: HList, K <: HList, V](implicit
    instance: MapReduce[L, G, K, V]
  ): MapReduce.Aux[L, G, K, V, instance.Out] = instance

  implicit def mapReduceBase[GH, K <: HList, V, R]: MapReduce.Aux[R :: Atoms[K, V] :: HNil, GH :: HNil, K, V, R] =
    new MapReduce[R :: Atoms[K, V] :: HNil, GH :: HNil, K, V] {
      override type Out = R

      override def apply(
        groupedAtoms: GroupedAtoms[GH :: HNil, K, V],
        chainedMapReduceFn: ChainedMapReduceFunction[GH :: HNil, R :: Atoms[K, V] :: HNil]
      ): R =
        chainedMapReduceFn match {
          case ChainedMapReduceFunction.Last(mapReduceFn) =>
            groupedAtoms match {
              case NestedAtoms(atoms) => mapReduceFn(atoms.map(_.ungroupBy[HNil]))
            }
        }
    }

  implicit def mapReduceRecurse[GH, `GH+1`, GT <: HList, K <: HList, V, RH, `RH+1`, RT <: HList](implicit
    mapReduce: MapReduce.Aux[`RH+1` :: RT, `GH+1` :: GT, K, V, `RH+1`]
  ): MapReduce.Aux[RH :: `RH+1` :: RT, GH :: `GH+1` :: GT, K, V, RH] =
    new MapReduce[RH :: `RH+1` :: RT, GH :: `GH+1` :: GT, K, V] {
      override type Out = RH

      override def apply(
        groupedAtoms: GroupedAtoms[GH :: `GH+1` :: GT, K, V],
        chainedMapReduceFn: ChainedMapReduceFunction[GH :: `GH+1` :: GT, RH :: `RH+1` :: RT]
      ): RH = chainedMapReduceFn match {
        case ChainedMapReduceFunction.Init(mapReduceFn, next) =>
          groupedAtoms match {
            case NestedAtoms(atoms) => mapReduceFn(atoms.map(mapReduce(_, next)))
          }
      }
    }
}
