package synnks.atoms

import cats.Reducible
import cats.data.NonEmptyList
import cats.syntax.all.*
import shapeless.*
import synnks.atoms.ops.GroupBy

final case class Atoms[K <: HList, V](values: NonEmptyList[Atom[K, V]]) {

  def map[NK <: HList, NV](f: Atom[K, V] => Atom[NK, NV]): Atoms[NK, NV] = Atoms(values.map(f))

  def mapKeys[NK <: HList](f: K => NK): Atoms[NK, V] = Atoms(values.map(_.mapKeys(f)))

  def groupBy[L <: HList](implicit groupBy: GroupBy[L, K, V]): groupBy.Out = groupBy(this)
}

object Atoms {

  def apply[C[_]: Reducible, K <: HList, V](values: C[(K, V)]): Atoms[K, V] =
    Atoms(values.toNonEmptyList.map { case (keys, value) => Atom(keys, value) })
}
