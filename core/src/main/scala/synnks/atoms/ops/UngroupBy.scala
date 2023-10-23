package synnks.atoms.ops

import shapeless.*
import synnks.atoms.*

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Cannot create UngroupBy[${L}, ${G}, ${K}, ${V}] instance.\n${L} contains elements that do not exist in ${G}, or do not appear in the same order."
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

  /*
  FIXME:
  Ungrouping returns the keys in the reverse order that they are grouped in.
  This is due to the order of the operations.
  Given a GroupedAtoms[A :: B :: C :: HNil, K <: HList, V], ungroupBy[A :: B :: C :: HNil] generates the following stack:

  ungroupBySameHead[A, B :: C :: HNil, B :: C :: HNil, K, V](
    ungroupBySameHead[B, C :: HNil, C :: HNil, A :: K, V](
      ungroupBySameHead[C, HNil, HNil, B :: A :: K, V](
        ungroupByHNil[C :: B :: A :: K, V]
      )
    )
  )
   */
  implicit def ungroupBySameHead[LH, LT <: HList, GT <: HList, K <: HList, V](implicit
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

  // TODO: Possible to merge together?
  implicit def ungroupByInitDifferentHead[LH, LT <: HList, GH, GT <: HList, K <: HList, V, NG <: HList, NK <: HList](
    implicit ungroupBy: UngroupBy.Aux[LH :: LT, GT, K, V, GroupedAtoms[NG, NK, V]]
  ): UngroupBy.Aux[LH :: LT, GH :: GT, K, V, GroupedAtoms[GH :: NG, NK, V]] = new UngroupBy[LH :: LT, GH :: GT, K, V] {
    override type Out = GroupedAtoms[GH :: NG, NK, V]

    override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V]): Out = groupedAtoms match {
      case NestedAtoms(groupedAtoms) => NestedAtoms(groupedAtoms.map(ungroupBy.apply))
    }
  }

  implicit def ungroupByLastDifferentHead[UH, UT <: HList, GH, GT <: HList, K <: HList, V, NK <: HList](implicit
    ungroupBy: UngroupBy.Aux[UH :: UT, GT, K, V, Atoms[NK, V]]
  ): UngroupBy.Aux[UH :: UT, GH :: GT, K, V, GroupedAtoms[GH :: HNil, NK, V]] =
    new UngroupBy[UH :: UT, GH :: GT, K, V] {
      override type Out = GroupedAtoms[GH :: HNil, NK, V]

      override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V]): Out = groupedAtoms match {
        case NestedAtoms(groupedAtoms) => NestedAtoms(groupedAtoms.map(ungroupBy.apply))
      }
    }
}
