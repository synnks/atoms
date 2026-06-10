package synnks.atoms.ops

import cats.syntax.all.*
import org.scalacheck.Prop.*
import synnks.atoms.*
import synnks.atoms.hlist.*

class MapAtomsTests extends AtomsSuite {

  test("map Atoms[K, V]") {
    def mapFunction(atom: Atom[Int :: String :: HNil, Double]): Atom[String :: Int :: HNil, Double] =
      atom.map { case (keys, value) =>
        (keys.tail.head :: keys.head :: HNil, value |+| value)
      }

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = atoms.mapAtoms(mapFunction)

      assertTypedEquals[Atoms[String :: Int :: HNil, Double]](
        atoms.map(mapFunction),
        expected
      )
    }
  }

  test("map GroupedAtoms[G, K, V]") {
    def mapFunction(atom: Atom[String :: Boolean :: HNil, Double]): Atom[Boolean :: String :: HNil, String] =
      atom.map { case (keys, value) =>
        (keys.tail.head :: keys.head :: HNil, value.toString)
      }

    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: HNil]
      val expected     = groupedAtoms.mapAtoms(mapFunction)

      assertTypedEquals[GroupedAtoms[Int :: HNil, Boolean :: String :: HNil, String]](
        groupedAtoms.map(mapFunction),
        expected
      )
    }
  }
}
