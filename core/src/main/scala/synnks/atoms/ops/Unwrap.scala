package synnks.atoms.ops

import cats.data.{ NonEmptyList, NonEmptyMap }
import shapeless.*
import synnks.atoms.*

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Cannot create Unwrap[${G}, ${K}, ${V}] instance.\n"
)
sealed trait Unwrap[G <: HList, K <: HList, V] {
  type Out

  def apply(groupedAtoms: GroupedAtoms[G, K, V]): Out
}

object Unwrap {
  type Aux[G <: HList, K <: HList, V, Out0] = Unwrap[G, K, V] { type Out = Out0 }

  @inline def apply[G <: HList, K <: HList, V](implicit
    instance: Unwrap[G, K, V]
  ): Unwrap.Aux[G, K, V, instance.Out] = instance

  implicit def unwrapHNil[K <: HList, V]: Unwrap.Aux[HNil, K, V, NonEmptyList[Atom[K, V]]] = new Unwrap[HNil, K, V] {
    override type Out = NonEmptyList[Atom[K, V]]

    override def apply(atoms: GroupedAtoms[HNil, K, V]): Out = atoms match {
      case atoms: Atoms[K, V] => atoms.values
    }
  }

  implicit def unwrapRecurse[GH, GT <: HList, K <: HList, V](implicit
    unwrap: Unwrap[GT, K, V]
  ): Unwrap.Aux[GH :: GT, K, V, NonEmptyMap[GH, unwrap.Out]] = new Unwrap[GH :: GT, K, V] {
    override type Out = NonEmptyMap[GH, unwrap.Out]

    override def apply(atoms: GroupedAtoms[GH :: GT, K, V]): Out = atoms match {
      case NestedAtoms(atoms) => atoms.map(unwrap.apply)
    }
  }
}
