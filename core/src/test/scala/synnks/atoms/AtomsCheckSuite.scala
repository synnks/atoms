package synnks.atoms

import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import synnks.atoms.util.{ AtomsTestInstances, HListTestInstances }

trait AtomsCheckSuite extends ScalaCheckSuite with HListTestInstances with AtomsTestInstances {

  implicit def nonEmptyListArbitrary[A](implicit aArbitrary: Arbitrary[A]): Arbitrary[NonEmptyList[A]] = Arbitrary(
    for {
      head <- aArbitrary.arbitrary
      tail <- implicitly[Arbitrary[List[A]]].arbitrary
    } yield NonEmptyList(head, tail)
  )
}
