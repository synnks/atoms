package synnks.atoms

import cats.syntax.all.*
import org.scalacheck.Prop.*
import synnks.atoms.hlist.*

class AtomsTests extends AtomsSuite {

  test("map") {
    def mapFunction(atom: Atom[Int :: String :: HNil, Double]): Atom[String :: Int :: HNil, Double] =
      atom.map { case (keys, value) =>
        (keys.tail.head :: keys.head :: HNil, value |+| value)
      }

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = Atoms(atoms.values.map(mapFunction))

      assertTypedEquals[Atoms[String :: Int :: HNil, Double]](
        atoms.map(mapFunction),
        expected
      )
    }
  }

  test("mapKeys") {
    def mapKeysFunction(keys: Int :: String :: HNil): String :: Int :: HNil = keys.tail.head :: keys.head :: HNil

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = Atoms(atoms.values.map(_.mapKeys(mapKeysFunction)))

      assertTypedEquals[Atoms[String :: Int :: HNil, Double]](
        atoms.mapKeys(mapKeysFunction),
        expected
      )
    }
  }
}
