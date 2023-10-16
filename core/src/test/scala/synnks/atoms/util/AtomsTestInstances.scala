package synnks.atoms.util

import cats.data.NonEmptyList
import cats.{ Eq, Order }
import org.scalacheck.Arbitrary
import shapeless.*
import synnks.atoms.{ Atom, Atoms }

trait AtomsTestInstances {

  implicit def atomOrder[K <: HList: Order, V: Order]: Order[Atom[K, V]] = Order.whenEqual(
    Order.by(_.keys),
    Order.by(_.value)
  )

  implicit def anyOrderAtomsEq[K <: HList: Order, V: Order]: Eq[Atoms[K, V]] = Order.by(_.values)

  implicit def atomArbitrary[K <: HList, V](implicit
    kArbitrary: Arbitrary[K],
    vArbitrary: Arbitrary[V]
  ): Arbitrary[Atom[K, V]] = Arbitrary(
    for {
      keys  <- kArbitrary.arbitrary
      value <- vArbitrary.arbitrary
    } yield Atom(keys, value)
  )

  implicit def atomsArbitrary[K <: HList, V](implicit A: Arbitrary[Atom[K, V]]): Arbitrary[Atoms[K, V]] = Arbitrary(
    for {
      head <- A.arbitrary
      tail <- implicitly[Arbitrary[List[Atom[K, V]]]].arbitrary
    } yield Atoms(NonEmptyList(head, tail))
  )
}
