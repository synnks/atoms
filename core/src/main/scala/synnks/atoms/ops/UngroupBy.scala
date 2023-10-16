package synnks.atoms.ops

import shapeless.*
import synnks.atoms.*

sealed trait UngroupBy[L <: HList, G <: HList, K <: HList, V] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V]): Out
}

object UngroupBy {
  type Aux[L <: HList, G <: HList, K <: HList, V, Out0] = UngroupBy[L, G, K, V] { type Out = Out0 }

  @inline def apply[L <: HList, G <: HList, K <: HList, V](implicit
    instance: UngroupBy[L, G, K, V]
  ): UngroupBy.Aux[L, G, K, V, instance.Out] = instance

  implicit def ungroupByIdentity[G <: HList, K <: HList, V]: UngroupBy.Aux[HNil, G, K, V, GroupedAtoms[G, K, V]] =
    new UngroupBy[HNil, G, K, V] {
      override type Out = GroupedAtoms[G, K, V]

      override def apply(groupedAtoms: GroupedAtoms[G, K, V]): Out = groupedAtoms
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
