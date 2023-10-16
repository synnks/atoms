package synnks.atoms.ops

import cats.Order
import shapeless.*
import shapeless.ops.hlist.Remove
import synnks.atoms.{ Atoms, GroupedAtoms, NestedAtoms }

sealed trait GroupBy[L <: HList, K <: HList, V] {
  type Out

  def apply(atoms: Atoms[K, V]): Out
}

object GroupBy {
  type Aux[L <: HList, K <: HList, V, Out0] = GroupBy[L, K, V] { type Out = Out0 }

  @inline def apply[L <: HList, K <: HList, V](implicit
    instance: GroupBy[L, K, V]
  ): GroupBy.Aux[L, K, V, instance.Out] = instance

  implicit def groupByBase[K <: HList, V]: GroupBy.Aux[HNil, K, V, GroupedAtoms[HNil, K, V]] = new GroupBy[HNil, K, V] {
    override type Out = GroupedAtoms[HNil, K, V]

    override def apply(atoms: Atoms[K, V]): Out = atoms
  }

  implicit def groupByRecurse[LH, LT <: HList, K <: HList, V, RemK <: HList, NK <: HList](implicit
    order: Order[LH],
    remove: Remove.Aux[K, LH, (LH, RemK)],
    groupBy: GroupBy.Aux[LT, RemK, V, GroupedAtoms[LT, NK, V]]
  ): GroupBy.Aux[LH :: LT, K, V, GroupedAtoms[LH :: LT, NK, V]] = new GroupBy[LH :: LT, K, V] {
    override type Out = GroupedAtoms[LH :: LT, NK, V]

    override def apply(atoms: Atoms[K, V]): Out =
      NestedAtoms {
        atoms.values
          .groupByNem(_.keys.removeElem[LH]._1)
          .map(values => groupBy(Atoms(values).mapKeys(_.removeElem[LH]._2)))
      }
  }
}
