package synnks.atoms.ops

import cats.data.{ NonEmptyList, NonEmptyMap }
import org.scalacheck.Prop.*
import synnks.atoms.*
import shapeless.*

class UnwrapTests extends AtomsSuite {

  test("unwrap Atoms") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val result = atoms.unwrap

      assertTypedEquals[NonEmptyList[Atom[Int :: String :: HNil, Double]]](
        result,
        atoms.values
      )
    }
  }

  test("unwrap GroupedAtoms") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      val result       = groupedAtoms.unwrap

      val expected = atoms.values
        .groupMapNem(_.keys.select[Int])(_.mapKeys(_.removeElem[Int]._2))
        .map(_.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2)))

      assertTypedEquals[NonEmptyMap[Int, NonEmptyMap[String, NonEmptyList[Atom[HNil, Double]]]]](
        result,
        expected
      )
    }
  }
}
