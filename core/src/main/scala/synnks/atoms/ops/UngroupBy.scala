package synnks.atoms.ops

import shapeless.*
import synnks.atoms.*

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Cannot create UngroupBy[${L}, ${G}, ${K}, ${V}] instance.\n${L} contains elements that do not exist in ${G}."
)
sealed trait UngroupBy[L <: HList, G <: HList, K <: HList, V] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V]): Out
}

object UngroupBy {
  type Aux[L <: HList, G <: HList, K <: HList, V, Out0] = UngroupBy[L, G, K, V] { type Out = Out0 }

  @inline def apply[L <: HList, G <: HList, K <: HList, V](implicit
    instance: UngroupBy[L, G, K, V]
  ): UngroupBy.Aux[L, G, K, V, instance.Out] = instance

  implicit def ungroupByHNil[K <: HList, V]: UngroupBy.Aux[HNil, HNil, K, V, Atoms[K, V]] =
    new UngroupBy[HNil, HNil, K, V] {
      override type Out = Atoms[K, V]

      override def apply(groupedAtoms: GroupedAtoms[HNil, K, V]): Out = groupedAtoms match {
        case atoms: Atoms[K, V] => atoms
      }
    }

  implicit def ungroupByIdentity[GH, GT <: HList, K <: HList, V]
    : UngroupBy.Aux[HNil, GH :: GT, K, V, GroupedAtoms[GH :: GT, K, V]] =
    new UngroupBy[HNil, GH :: GT, K, V] {
      override type Out = GroupedAtoms[GH :: GT, K, V]

      override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V]): Out = groupedAtoms
    }

  implicit def ungroupByRecurse[LH, LT <: HList, GT <: HList, K <: HList, V](implicit
    ungroupBy: UngroupBy[LT, GT, LH :: K, V]
  ): UngroupBy.Aux[LH :: LT, LH :: GT, K, V, ungroupBy.Out] = new UngroupBy[LH :: LT, LH :: GT, K, V] {
    override type Out = ungroupBy.Out

    override def apply(groupedAtoms: GroupedAtoms[LH :: GT, K, V]): Out = groupedAtoms match {
      case NestedAtoms(groupedAtoms) =>
        ungroupBy {
          groupedAtoms.transform { (lh, groupedAtoms) =>
            groupedAtoms.mapKeys(lh :: _)
          }.reduce
        }
    }
  }
}
