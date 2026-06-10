package synnks.atoms.ops

import cats.Semigroup
import synnks.atoms.*
import synnks.atoms.hlist.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create UngroupBy[${L}, ${G}, ${K}, ${V}] instance.
${L} contains elements that do not exist in ${G}, or do not appear in the same order.
""")
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

  implicit def ungroupBySameHead[LH, LT <: HList, GT <: HList, K <: HList, V, NG <: HList, NK <: HList, Out0](implicit
    ungroupBy: UngroupBy[LT, GT, K, V] { type Out <: GroupedAtoms[NG, NK, V] },
    mapAtoms: MapAtoms.Aux[NG, NK, V, LH :: NK, V, Out0],
    semigroup: Semigroup[Out0]
  ): UngroupBy.Aux[LH :: LT, LH :: GT, K, V, Out0] = new UngroupBy[LH :: LT, LH :: GT, K, V] {
    override type Out = Out0

    override def apply(groupedAtoms: GroupedAtoms[LH :: GT, K, V]): Out = groupedAtoms match {
      case NestedAtoms(groupedAtoms) =>
        groupedAtoms.transform { (lh, groupedAtoms) =>
          mapAtoms(ungroupBy(groupedAtoms), _.mapKeys(lh :: _))
        }.reduce
    }
  }

  implicit def ungroupByDifferentHead[LH, LT <: HList, GH, GT <: HList, K <: HList, V, NG <: HList, NK <: HList](
    implicit ungroupBy: UngroupBy[LH :: LT, GT, K, V] { type Out <: GroupedAtoms[NG, NK, V] }
  ): UngroupBy.Aux[LH :: LT, GH :: GT, K, V, GroupedAtoms[GH :: NG, NK, V]] = new UngroupBy[LH :: LT, GH :: GT, K, V] {
    override type Out = GroupedAtoms[GH :: NG, NK, V]

    override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V]): Out = groupedAtoms match {
      case NestedAtoms(groupedAtoms) => NestedAtoms(groupedAtoms.map(ungroupBy.apply))
    }
  }
}
