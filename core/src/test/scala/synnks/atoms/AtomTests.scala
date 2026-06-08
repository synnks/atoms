package synnks.atoms

import cats.syntax.all.*
import org.scalacheck.Prop.*
import synnks.atoms.hlist.*

class AtomTests extends AtomsSuite {

  test("map") {
    def mapFunction(keys: Int :: String :: HNil, value: Double): (String :: Int :: HNil, Double) =
      (keys.tail.head :: keys.head :: HNil, value |+| value)

    forAll { (atom: Atom[Int :: String :: HNil, Double]) =>
      val expected = {
        val (keys, value) = mapFunction(atom.keys, atom.value)
        Atom(keys, value)
      }

      assertTypedEquals[Atom[String :: Int :: HNil, Double]](atom.map(mapFunction), expected)
    }
  }

  test("mapKeys") {
    def mapKeysFunction(keys: Int :: String :: HNil): String :: Int :: HNil = keys.tail.head :: keys.head :: HNil

    forAll { (atom: Atom[Int :: String :: HNil, Double]) =>
      val expected = Atom(mapKeysFunction(atom.keys), atom.value)

      assertTypedEquals[Atom[String :: Int :: HNil, Double]](atom.mapKeys(mapKeysFunction), expected)
    }
  }
}
