package synnks.atoms.ops

import synnks.atoms.*
import synnks.atoms.hlist.*

trait MapKeys[L <: HList, G <: HList, K <: HList, V] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V], f: K => L): Out
}

object MapKeys {
  type Aux[L <: HList, G <: HList, K <: HList, V, Out0] = MapKeys[L, G, K, V] {
    type Out = Out0
  }

  @inline def apply[L <: HList, G <: HList, K <: HList, V](implicit
    instance: MapKeys[L, G, K, V]
  ): MapKeys.Aux[L, G, K, V, instance.Out] = instance

  implicit def mapKeysHNil[L <: HList, K <: HList, V]: MapKeys.Aux[L, HNil, K, V, Atoms[L, V]] =
    new MapKeys[L, HNil, K, V] {
      override type Out = Atoms[L, V]

      override def apply(groupedAtoms: GroupedAtoms[HNil, K, V], f: K => L): Out = groupedAtoms match {
        case atoms: Atoms[K, V] => atoms.mapKeys(f)
      }
    }

  implicit def mapKeysHCons[L <: HList, GH, GT <: HList, K <: HList, V]
    : MapKeys.Aux[L, GH :: GT, K, V, GroupedAtoms[GH :: GT, L, V]] =
    new MapKeys[L, GH :: GT, K, V] {
      override type Out = GroupedAtoms[GH :: GT, L, V]

      override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V], f: K => L): Out = groupedAtoms.mapKeys(f)
    }

  implicit def mapKeysPrependHNil[LH, K <: HList, V]: MapKeys.Aux[LH :: K, HNil, K, V, Atoms[LH :: K, V]] =
    mapKeysHNil[LH :: K, K, V]

  implicit def mapKeysPrependHCons[LH, GH, GT <: HList, K <: HList, V]
    : MapKeys.Aux[LH :: K, GH :: GT, K, V, GroupedAtoms[GH :: GT, LH :: K, V]] =
    mapKeysHCons[LH :: K, GH, GT, K, V]
}
