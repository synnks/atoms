package synnks.atoms

import cats.{ Reducible, Semigroup }
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.syntax.all.*
import shapeless.*
import synnks.atoms.ops.GroupBy

sealed trait GroupedAtoms[G <: HList, K <: HList, V] {

  def ++(other: GroupedAtoms[G, K, V]): GroupedAtoms[G, K, V]

  def map[NK <: HList, NV](f: Atom[K, V] => Atom[NK, NV]): GroupedAtoms[G, NK, NV]

  def mapKeys[NK <: HList](f: K => NK): GroupedAtoms[G, NK, V] = map(_.mapKeys(f))

  def groupBy[L <: HList](implicit groupBy: GroupBy[L, G, K, V]): groupBy.Out = groupBy(this)
}

object GroupedAtoms {

  implicit def semigroup[G <: HList, K <: HList, V]: Semigroup[GroupedAtoms[G, K, V]] = _ ++ _
}

final case class Atoms[K <: HList, V](values: NonEmptyList[Atom[K, V]]) extends GroupedAtoms[HNil, K, V] {

  override def ++(other: GroupedAtoms[HNil, K, V]): Atoms[K, V] = other match {
    case Atoms(values) => Atoms(this.values ::: values)
  }

  override def map[NK <: HList, NV](f: Atom[K, V] => Atom[NK, NV]): Atoms[NK, NV] = Atoms(values.map(f))

  override def mapKeys[NK <: HList](f: K => NK): Atoms[NK, V] = Atoms(values.map(_.mapKeys(f)))
}

object Atoms {

  def apply[C[_]: Reducible, K <: HList, V](values: C[(K, V)]): Atoms[K, V] =
    Atoms(values.toNonEmptyList.map(Atom.apply[K, V].tupled))
}

final private[atoms] case class NestedAtoms[GH, GT <: HList, K <: HList, V](
  groupedAtoms: NonEmptyMap[GH, GroupedAtoms[GT, K, V]]
) extends GroupedAtoms[GH :: GT, K, V] {

  override def ++(other: GroupedAtoms[GH :: GT, K, V]): GroupedAtoms[GH :: GT, K, V] = other match {
    case NestedAtoms(groupedAtoms) => NestedAtoms(this.groupedAtoms |+| groupedAtoms)
  }

  override def map[NK <: HList, NV](f: Atom[K, V] => Atom[NK, NV]): GroupedAtoms[GH :: GT, NK, NV] =
    NestedAtoms(groupedAtoms.map(_.map(f)))
}
