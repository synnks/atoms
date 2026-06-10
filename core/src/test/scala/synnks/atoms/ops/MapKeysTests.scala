package synnks.atoms.ops

import org.scalacheck.Prop.*
import synnks.atoms.*
import synnks.atoms.hlist.*

class MapKeysTests extends AtomsSuite {

  test("mapKeys Atoms[K, V]") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = atoms.map(_.mapKeys(keys => keys.tail.head :: keys.head :: HNil))

      assertTypedEquals[Atoms[String :: Int :: HNil, Double]](
        atoms.mapKeys(keys => keys.tail.head :: keys.head :: HNil),
        expected
      )
    }
  }

  test("mapKeys GroupedAtoms[G, K, V]") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: HNil]
      val expected     = groupedAtoms.map(_.mapKeys(keys => keys.tail.head :: keys.head :: HNil))

      assertTypedEquals[GroupedAtoms[Int :: HNil, Boolean :: String :: HNil, Double]](
        groupedAtoms.mapKeys(keys => keys.tail.head :: keys.head :: HNil),
        expected
      )
    }
  }
}
