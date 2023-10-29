package synnks.atoms.ops

import shapeless.*
import synnks.atoms.*

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Cannot create Lookup[${L}, ${G}, ${K}, ${V}] instance.\n${L} contains elements that do not exist in ${G}, or do not appear in the same order."
)
sealed trait Lookup[L <: HList, G <: HList, K <: HList, V] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V], lookupKeys: L): Out
}

object Lookup {
  type Aux[L <: HList, G <: HList, K <: HList, V, Out0] = Lookup[L, G, K, V] { type Out = Out0 }

  @inline def apply[L <: HList, G <: HList, K <: HList, V](implicit
    instance: Lookup[L, G, K, V]
  ): Lookup.Aux[L, G, K, V, instance.Out] = instance

  implicit def lookupHNil[G <: HList, K <: HList, V](implicit
    ungroupBy: UngroupBy[G, G, K, V]
  ): Lookup.Aux[HNil, G, K, V, Option[ungroupBy.Out]] =
    new Lookup[HNil, G, K, V] {
      override type Out = Option[ungroupBy.Out]

      override def apply(groupedAtoms: GroupedAtoms[G, K, V], lookupKeys: HNil): Out =
        Some(groupedAtoms.ungroupBy[G](ungroupBy))
    }

  implicit def lookupSameHead[LH, LT <: HList, GT <: HList, K <: HList, V, AtomsRes](implicit
    lookup: Lookup.Aux[LT, GT, K, V, Option[AtomsRes]]
  ): Lookup.Aux[LH :: LT, LH :: GT, K, V, Option[AtomsRes]] = new Lookup[LH :: LT, LH :: GT, K, V] {
    override type Out = Option[AtomsRes]

    override def apply(groupedAtoms: GroupedAtoms[LH :: GT, K, V], lookupKeys: LH :: LT): Out = groupedAtoms match {
      case NestedAtoms(groupedAtoms) => groupedAtoms.lookup(lookupKeys.head).flatMap(_.lookup(lookupKeys.tail))
    }
  }

  implicit def lookupDifferentHead[LH, LT <: HList, GH, GT <: HList, K <: HList, V](implicit
    ungroupBy: UngroupBy.Aux[GH :: HNil, GH :: GT, K, V, GroupedAtoms[GT, GH :: K, V]],
    lookup: Lookup[LH :: LT, GT, GH :: K, V]
  ): Lookup.Aux[LH :: LT, GH :: GT, K, V, lookup.Out] = new Lookup[LH :: LT, GH :: GT, K, V] {
    override type Out = lookup.Out

    override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V], lookupKeys: LH :: LT): Out =
      groupedAtoms.ungroupBy[GH :: HNil].lookup(lookupKeys)
  }
}
