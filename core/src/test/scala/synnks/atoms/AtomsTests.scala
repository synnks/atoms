package synnks.atoms

import cats.Semigroup
import cats.syntax.all.*
import org.scalacheck.Prop.*
import shapeless.*
import shapeless.ops.hlist.Reverse

class AtomsTests extends AtomsSuite {

  test("map") {
    def mapFunction[K <: HList, V: Semigroup](atom: Atom[K, V])(implicit reverse: Reverse[K]): Atom[reverse.Out, V] =
      atom.map { case (keys, value) =>
        (keys.reverse, value |+| value)
      }

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = Atoms(atoms.values.map(mapFunction(_)))

      assertTypedEquals[Atoms[String :: Int :: HNil, Double]](
        atoms.map(mapFunction(_)),
        expected
      )
    }
  }

  test("mapKeys") {
    def mapKeysFunction[K <: HList](keys: K)(implicit reverse: Reverse[K]): reverse.Out = keys.reverse

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = Atoms(atoms.values.map(_.mapKeys(mapKeysFunction(_))))

      assertTypedEquals[Atoms[String :: Int :: HNil, Double]](
        atoms.mapKeys(mapKeysFunction(_)),
        expected
      )
    }
  }
}
