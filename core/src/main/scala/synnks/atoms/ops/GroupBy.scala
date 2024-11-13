package synnks.atoms.ops

import cats.Order
import shapeless.*
import shapeless.ops.hlist.Remove
import synnks.atoms.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create GroupBy[${L}, ${G}, ${K}, ${V}] instance.
${L} contains elements that do not exist in ${K}.
""")
sealed trait GroupBy[L <: HList, G <: HList, K <: HList, V] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V]): Out
}

object GroupBy {
  type Aux[L <: HList, G <: HList, K <: HList, V, Out0] = GroupBy[L, G, K, V] { type Out = Out0 }

  @inline def apply[L <: HList, G <: HList, K <: HList, V](implicit
    instance: GroupBy[L, G, K, V]
  ): GroupBy.Aux[L, G, K, V, instance.Out] = instance

  implicit def groupByIdentity[G <: HList, K <: HList, V]: GroupBy.Aux[HNil, G, K, V, GroupedAtoms[G, K, V]] =
    new GroupBy[HNil, G, K, V] {
      override type Out = GroupedAtoms[G, K, V]

      override def apply(groupedAtoms: GroupedAtoms[G, K, V]): Out = groupedAtoms
    }

  implicit def groupByRecurse[LH, LT <: HList, K <: HList, V, RemK <: HList, NK <: HList](implicit
    order: Order[LH],
    remove: Remove.Aux[K, LH, (LH, RemK)],
    groupBy: GroupBy.Aux[LT, HNil, RemK, V, GroupedAtoms[LT, NK, V]]
  ): GroupBy.Aux[LH :: LT, HNil, K, V, GroupedAtoms[LH :: LT, NK, V]] =
    new GroupBy[LH :: LT, HNil, K, V] {
      override type Out = GroupedAtoms[LH :: LT, NK, V]

      override def apply(groupedAtoms: GroupedAtoms[HNil, K, V]): Out = groupedAtoms match {
        case Atoms(values) =>
          NestedAtoms {
            values
              .groupByNem(_.keys.removeElem[LH]._1)
              .map(values => groupBy(Atoms(values).mapKeys(_.removeElem[LH]._2)))
          }
      }
    }

  implicit def groupByForward[LH, LT <: HList, GH, GT <: HList, K <: HList, V, NG <: HList, NK <: HList](implicit
    groupBy: GroupBy.Aux[LH :: LT, GT, K, V, GroupedAtoms[NG, NK, V]]
  ): GroupBy.Aux[LH :: LT, GH :: GT, K, V, GroupedAtoms[GH :: NG, NK, V]] = new GroupBy[LH :: LT, GH :: GT, K, V] {
    override type Out = GroupedAtoms[GH :: NG, NK, V]

    override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V]): Out = groupedAtoms match {
      case NestedAtoms(atoms) => NestedAtoms(atoms.map(groupBy.apply))
    }
  }
}
