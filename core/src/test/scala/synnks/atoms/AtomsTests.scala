package synnks.atoms

import cats.Semigroup
import cats.syntax.all.*
import org.scalacheck.Prop.*
import shapeless.*
import shapeless.ops.hlist.Reverse

class AtomsTests extends AtomsCheckSuite {

  test("map") {
    def mapFunction[K <: HList, V: Semigroup](atom: Atom[K, V])(implicit reverse: Reverse[K]): Atom[reverse.Out, V] =
      atom.map { case (keys, value) =>
        (keys.reverse, value |+| value)
      }
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = Atoms(atoms.values.map(mapFunction(_)))

      assert(atoms.map(mapFunction(_)) === expected)
    }
  }

  test("mapKeys") {
    def mapKeysFunction[K <: HList](keys: K)(implicit reverse: Reverse[K]): reverse.Out = keys.reverse

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = Atoms(atoms.values.map(_.mapKeys(mapKeysFunction(_))))

      assert(atoms.mapKeys(mapKeysFunction(_)) === expected)
    }
  }

  test("groupBy HNil") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      assert(atoms.groupBy[HNil] === atoms)
    }
  }

  test("groupBy 1 level head of HList") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = atoms.values.groupMapNem(_.keys.head)(_.mapKeys(_.tail)).map(Atoms(_))

      assert(atoms.groupBy[Int :: HNil] === expected)
    }
  }

  test("groupBy 1 level mid element of HList") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = atoms.values
        .groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
        .map(Atoms(_))

      assert(atoms.groupBy[String :: HNil] === expected)
    }
  }

  test("groupBy all levels") {
    forAll { (atoms: Atoms[Int :: String :: Double :: HNil, Boolean]) =>
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
    forAll { (atoms: Atoms[Int :: String :: Double :: HNil, Boolean]) =>
      val expected = atoms.values
        .groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
        .map(Atoms(_))

      assert(atoms.groupBy[String :: HNil] === expected)
    }
  }
}
