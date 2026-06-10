package synnks.atoms.ops

import synnks.atoms.*
import synnks.atoms.hlist.*

trait MapKeys[A, V] {
  type K <: HList
  type NK <: HList
  type Out

  def apply(a: A)(f: K => NK): Out
}

object MapKeys extends LowPriorityMapKeys {
  type Aux[A, V, K0 <: HList, NK0 <: HList, Out0] = MapKeys[A, V] {
    type K   = K0
    type NK  = NK0
    type Out = Out0
  }

  implicit def atomsPrependMapKeys[LH, K <: HList, V]: MapKeys.Aux[Atoms[K, V], V, K, LH :: K, Atoms[LH :: K, V]] =
    atomsMapKeys[K, LH :: K, V]

  implicit def groupedAtomsPrependMapKeys[LH, G <: HList, K <: HList, V]
    : MapKeys.Aux[GroupedAtoms[G, K, V], V, K, LH :: K, GroupedAtoms[G, LH :: K, V]] =
    groupedAtomsMapKeys[G, K, LH :: K, V]
}

private[ops] trait LowPriorityMapKeys {
  implicit def atomsMapKeys[K0 <: HList, NK0 <: HList, V]: MapKeys.Aux[Atoms[K0, V], V, K0, NK0, Atoms[NK0, V]] =
    new MapKeys[Atoms[K0, V], V] {
      override type K   = K0
      override type NK  = NK0
      override type Out = Atoms[NK0, V]

      override def apply(a: Atoms[K, V])(f: K => NK): Out = a.mapKeys(f)
    }

  implicit def groupedAtomsMapKeys[G <: HList, K0 <: HList, NK0 <: HList, V]
    : MapKeys.Aux[GroupedAtoms[G, K0, V], V, K0, NK0, GroupedAtoms[G, NK0, V]] =
    new MapKeys[GroupedAtoms[G, K0, V], V] {
      override type K   = K0
      override type NK  = NK0
      override type Out = GroupedAtoms[G, NK0, V]

      override def apply(a: GroupedAtoms[G, K, V])(f: K => NK): Out = a.mapKeys(f)
    }
}
