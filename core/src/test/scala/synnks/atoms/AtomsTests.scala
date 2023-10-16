package synnks.atoms

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.laws.discipline.arbitrary.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import shapeless.*

class AtomsTests extends ScalaCheckSuite {

  test("groupBy HNil") {
    forAll { (nel: NonEmptyList[(Int, String, Double)]) =>
      val atoms = Atoms(nel.map { case (int, string, double) => (int :: string :: HNil, double) })

      assert(atoms.groupBy[HNil] === atoms)
    }
  }

  test("groupBy 1 level head of HList") {
    forAll { (nel: NonEmptyList[(Int, String, Double)]) =>
      val atoms    = Atoms(nel.map { case (int, string, double) => (int :: string :: HNil, double) })
      val expected = atoms.values.groupMapNem(_.keys.head)(_.mapKeys(_.tail)).map(Atoms(_))

      assert(atoms.groupBy[Int :: HNil] === expected)
    }
  }

  test("groupBy 1 level mid element of HList") {
    forAll { (nel: NonEmptyList[(Int, String, Double)]) =>
      val atoms    = Atoms(nel.map { case (int, string, double) => (int :: string :: HNil, double) })
      val expected = atoms.values.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2)).map(Atoms(_))

      assert(atoms.groupBy[String :: HNil] === expected)
    }
  }

  test("groupBy all levels") {
    forAll { (nel: NonEmptyList[(Int, String, Double, Boolean)]) =>
      val atoms    = Atoms(nel.map { case (int, string, double, boolean) => (int :: string :: double :: HNil, boolean) })
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
    forAll { (nel: NonEmptyList[(Int, String, Double, Boolean)]) =>
      val atoms    = Atoms(nel.map { case (int, string, double, boolean) => (int :: string :: double :: HNil, boolean) })
      val expected = atoms.values
        .groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
        .map(Atoms(_))

      assert(atoms.groupBy[String :: HNil] === expected)
    }
  }
}
