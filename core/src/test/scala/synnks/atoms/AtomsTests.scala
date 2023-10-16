package synnks.atoms

import cats.data.NonEmptyList
import cats.syntax.all.*
import org.scalacheck.Prop.*
import shapeless.*

class AtomsTests extends AtomsCheckSuite {

  test("groupBy HNil") {
    forAll { (values: NonEmptyList[(Int :: String :: HNil, Double)]) =>
      val atoms = Atoms(values)

      assert(atoms.groupBy[HNil] === atoms)
    }
  }

  test("groupBy 1 level head of HList") {
    forAll { (values: NonEmptyList[(Int :: String :: HNil, Double)]) =>
      val atoms    = Atoms(values)
      val expected = atoms.values.groupMapNem(_.keys.head)(_.mapKeys(_.tail)).map(Atoms(_))

      assert(atoms.groupBy[Int :: HNil] === expected)
    }
  }

  test("groupBy 1 level mid element of HList") {
    forAll { (values: NonEmptyList[(Int :: String :: HNil, Double)]) =>
      val atoms    = Atoms(values)
      val expected = atoms.values.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2)).map(Atoms(_))

      assert(atoms.groupBy[String :: HNil] === expected)
    }
  }

  test("groupBy all levels") {
    forAll { (values: NonEmptyList[(Int :: String :: Double :: HNil, Boolean)]) =>
      val atoms    = Atoms(values)
      val expected = atoms.values
        .groupMapNem(_.keys.select[Int])(_.mapKeys(_.removeElem[Int]._2))
        .map(
          _.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
            .map(_.groupMapNem(_.keys.select[Double])(_.mapKeys(_.removeElem[Double]._2)).map(Atoms(_)))
        )

      assert(atoms.groupBy[Int :: String :: Double :: HNil] === expected)
    }
  }

  test("groupBy skip levels") {
    forAll { (values: NonEmptyList[(Int :: String :: Double :: HNil, Boolean)]) =>
      val atoms    = Atoms(values)
      val expected = atoms.values
        .groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
        .map(Atoms(_))

      assert(atoms.groupBy[String :: HNil] === expected)
    }
  }
}
