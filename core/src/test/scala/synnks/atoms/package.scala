package synnks

import cats.{ Eq, Order }
import shapeless.*

package object atoms {

  implicit def hNilOrder: Order[HNil] = Order.allEqual

  implicit def hConsOrder[H: Order, T <: HList: Order]: Order[H :: T] =
    Order.whenEqual(Order.by(_.head), Order.by(_.tail))

  implicit def atomOrder[K <: HList: Order, V: Order]: Order[Atom[K, V]] = Order.whenEqual(
    Order.by(_.keys),
    Order.by(_.value)
  )

  implicit def anyOrderAtomsEq[K <: HList: Order, V: Order]: Eq[Atoms[K, V]] = Order.by(_.values)
}
