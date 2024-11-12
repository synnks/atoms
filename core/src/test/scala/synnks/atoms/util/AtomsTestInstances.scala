package synnks.atoms.util

import cats.data.NonEmptyList
import cats.{ Eq, Order }
import cats.syntax.all.*
import org.scalacheck.{ Arbitrary, Cogen }
import shapeless.*
import synnks.atoms.{ Atom, Atoms, GroupedAtoms, NestedAtoms }

trait AtomsTestInstances {

  implicit def atomOrder[K <: HList: Order, V: Order]: Order[Atom[K, V]] = Order.whenEqual(
    Order.by(_.keys),
    Order.by(_.value)
  )

  implicit def anyOrderAtomsEq[K <: HList, V](implicit order: Order[Atom[K, V]]): Eq[Atoms[K, V]] =
    Eq.by(_.values.sorted)

  implicit def hNilGroupedAtomsEq[K <: HList, V](implicit eq: Eq[Atoms[K, V]]): Eq[GroupedAtoms[HNil, K, V]] =
    Eq.by { case atoms: Atoms[K, V] => atoms }

  implicit def hConsGroupedAtomsEq[GH, GT <: HList, K <: HList, V](implicit
    eq: Eq[GroupedAtoms[GT, K, V]]
  ): Eq[GroupedAtoms[GH :: GT, K, V]] = (x, y) =>
    (x, y) match {
      case (NestedAtoms(x), NestedAtoms(y)) => x === y
    }

  implicit def atomArbitrary[K <: HList: Arbitrary, V: Arbitrary]: Arbitrary[Atom[K, V]] = Arbitrary {
    for {
      keys  <- Arbitrary.arbitrary[K]
      value <- Arbitrary.arbitrary[V]
    } yield Atom(keys, value)
  }

  implicit def atomsArbitrary[K <: HList, V](implicit A: Arbitrary[Atom[K, V]]): Arbitrary[Atoms[K, V]] = Arbitrary {
    for {
      head <- Arbitrary.arbitrary[Atom[K, V]]
      tail <- Arbitrary.arbitrary[List[Atom[K, V]]]
    } yield Atoms(NonEmptyList(head, tail))
  }

  implicit def atomCogen[K <: HList: Cogen, V: Cogen]: Cogen[Atom[K, V]] = Cogen { (seed, atom) =>
    Cogen[(K, V)].perturb(seed, (atom.keys, atom.value))
  }

  implicit def atomsCogen[K <: HList: Cogen, V: Cogen]: Cogen[Atoms[K, V]] = Cogen.it(_.values.iterator)
}
