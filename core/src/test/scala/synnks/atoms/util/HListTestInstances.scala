package synnks.atoms.util

import cats.Order
import org.scalacheck.{Arbitrary, Gen}
import shapeless.*

trait HListTestInstances {

  implicit val hNilOrder: Order[HNil] = Order.allEqual

  implicit def hConsOrder[H: Order, T <: HList : Order]: Order[H :: T] =
    Order.whenEqual(Order.by(_.head), Order.by(_.tail))

  implicit val hNilArbitrary: Arbitrary[HNil] = Arbitrary(Gen.const(HNil))

  implicit def hConsArbitrary[H, T <: HList](implicit hArbitrary: Arbitrary[H], tArbitrary: Arbitrary[T]): Arbitrary[H :: T] = Arbitrary(for {
    h <- hArbitrary.arbitrary
    t <- tArbitrary.arbitrary
  } yield h :: t)

}
