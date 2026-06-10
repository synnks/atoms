package synnks.atoms.ops

import synnks.atoms.*
import synnks.atoms.hlist.*

trait MapKeys[A, K <: HList, NK <: HList, V] {
  type Out

  def apply(a: A)(f: K => NK): Out
}

object MapKeys {
  type Aux[A, K <: HList, NK <: HList, V, Out0] = MapKeys[A, K, NK, V] {
    type Out = Out0
  }

  @inline def apply[A, K <: HList, NK <: HList, V](implicit
    instance: MapKeys[A, K, NK, V]
  ): MapKeys.Aux[A, K, NK, V, instance.Out] = instance

  implicit def atomsMapKeys[K <: HList, NK <: HList, V]: MapKeys.Aux[Atoms[K, V], K, NK, V, Atoms[NK, V]] =
    new MapKeys[Atoms[K, V], K, NK, V] {
      override type Out = Atoms[NK, V]

      override def apply(a: Atoms[K, V])(f: K => NK): Out = a.mapKeys(f)
    }

  implicit def groupedAtomsMapKeys[G <: HList, K <: HList, NK <: HList, V]
    : MapKeys.Aux[GroupedAtoms[G, K, V], K, NK, V, GroupedAtoms[G, NK, V]] =
    new MapKeys[GroupedAtoms[G, K, V], K, NK, V] {
      override type Out = GroupedAtoms[G, NK, V]

      override def apply(a: GroupedAtoms[G, K, V])(f: K => NK): Out = a.mapKeys(f)
    }

  implicit def atomsPrependMapKeys[LH, K <: HList, V]: MapKeys.Aux[Atoms[K, V], K, LH :: K, V, Atoms[LH :: K, V]] =
    atomsMapKeys[K, LH :: K, V]

  implicit def groupedAtomsPrependMapKeys[LH, G <: HList, K <: HList, V]
    : MapKeys.Aux[GroupedAtoms[G, K, V], K, LH :: K, V, GroupedAtoms[G, LH :: K, V]] =
    groupedAtomsMapKeys[G, K, LH :: K, V]
}
