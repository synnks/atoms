package synnks.atoms.ops

import synnks.atoms.*
import synnks.atoms.hlist.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create MapAtoms[${G}, ${K}, ${V}, ${NK}, ${NV}] instance.
""")
sealed trait MapAtoms[G <: HList, K <: HList, V, NK <: HList, NV] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V], f: Atom[K, V] => Atom[NK, NV]): Out
}

object MapAtoms {
  type Aux[G <: HList, K <: HList, V, NK <: HList, NV, Out0] = MapAtoms[G, K, V, NK, NV] {
    type Out = Out0
  }

  @inline def apply[G <: HList, K <: HList, V, NK <: HList, NV](implicit
    instance: MapAtoms[G, K, V, NK, NV]
  ): MapAtoms.Aux[G, K, V, NK, NV, instance.Out] = instance

  implicit def mapAtomsHNil[K <: HList, V, NK <: HList, NV]: MapAtoms.Aux[HNil, K, V, NK, NV, Atoms[NK, NV]] =
    new MapAtoms[HNil, K, V, NK, NV] {
      override type Out = Atoms[NK, NV]

      override def apply(groupedAtoms: GroupedAtoms[HNil, K, V], f: Atom[K, V] => Atom[NK, NV]): Out =
        groupedAtoms match {
          case atoms: Atoms[K, V] => atoms.mapAtoms(f)
        }
    }

  implicit def mapAtomsHCons[GH, GT <: HList, K <: HList, V, NK <: HList, NV]
    : MapAtoms.Aux[GH :: GT, K, V, NK, NV, GroupedAtoms[GH :: GT, NK, NV]] =
    new MapAtoms[GH :: GT, K, V, NK, NV] {
      override type Out = GroupedAtoms[GH :: GT, NK, NV]

      override def apply(groupedAtoms: GroupedAtoms[GH :: GT, K, V], f: Atom[K, V] => Atom[NK, NV]): Out =
        groupedAtoms.mapAtoms(f)
    }
}
