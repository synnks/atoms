package synnks.atoms

import cats.Semigroup
import cats.syntax.all.*
import org.scalacheck.Prop.*
import shapeless.*
import shapeless.ops.hlist.Reverse

class AtomTests extends AtomsCheckSuite {

  test("map") {
    def mapFunction[K <: HList, V: Semigroup](keys: K, value: V)(implicit reverse: Reverse[K]): (reverse.Out, V) =
      (keys.reverse, value |+| value)

    forAll { (atom: Atom[Int :: String :: HNil, Double]) =>
      val expected = {
        val (keys, value) = mapFunction(atom.keys, atom.value)
        Atom(keys, value)
      }

      assertTypedEquals[Atom[String :: Int :: HNil, Double]](atom.map(mapFunction(_, _)), expected)
    }
  }

  test("mapKeys") {
    def mapKeysFunction[K <: HList](keys: K)(implicit reverse: Reverse[K]): reverse.Out = keys.reverse

    forAll { (atom: Atom[Int :: String :: HNil, Double]) =>
      val expected = Atom(mapKeysFunction(atom.keys), atom.value)

      assertTypedEquals[Atom[String :: Int :: HNil, Double]](atom.mapKeys(mapKeysFunction(_)), expected)
    }
  }
}
