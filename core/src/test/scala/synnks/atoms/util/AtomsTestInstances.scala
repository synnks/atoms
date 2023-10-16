package synnks.atoms.util

import cats.{ Eq, Order }
import shapeless.HList
import synnks.atoms.{ Atom, Atoms }

trait AtomsTestInstances {

  implicit def atomOrder[K <: HList: Order, V: Order]: Order[Atom[K, V]] = Order.whenEqual(
    Order.by(_.keys),
    Order.by(_.value)
  )

  implicit def anyOrderAtomsEq[K <: HList: Order, V: Order]: Eq[Atoms[K, V]] = Order.by(_.values)
}
