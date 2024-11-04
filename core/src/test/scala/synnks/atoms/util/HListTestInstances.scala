package synnks.atoms.util

import cats.Order
import org.scalacheck.{ Arbitrary, Gen }
import shapeless.*

trait HListTestInstances {

  implicit val hNilOrder: Order[HNil] = Order.allEqual

  implicit def hConsOrder[H: Order, T <: HList: Order]: Order[H :: T] =
    Order.whenEqual(Order.by(_.head), Order.by(_.tail))

  implicit val hNilArbitrary: Arbitrary[HNil] = Arbitrary(Gen.const(HNil))

  implicit def hConsArbitrary[H: Arbitrary, T <: HList: Arbitrary]: Arbitrary[H :: T] = Arbitrary {
    for {
      h <- Arbitrary.arbitrary[H]
      t <- Arbitrary.arbitrary[T]
    } yield h :: t
  }

}
