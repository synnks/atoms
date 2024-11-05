package synnks.atoms.util

import cats.Order
import org.scalacheck.{ Arbitrary, Cogen, Gen }
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

  implicit val hNilCogen: Cogen[HNil] = Cogen(_ => 0L)

  implicit def hConsCogen[H: Cogen, T <: HList: Cogen]: Cogen[H :: T] = Cogen { (seed, hList) =>
    Cogen[(H, T)].perturb(seed, (hList.head, hList.tail))
  }
}
