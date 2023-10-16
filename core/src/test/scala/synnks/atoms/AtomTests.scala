package synnks.atoms

import org.scalacheck.Prop.*
import shapeless.*

class AtomTests extends AtomsCheckSuite {

  test("map") {
    def mapFunction[T <: HList](keys: Int :: T, value: String): (Int :: T, String) =
      ((keys.head + 1) :: keys.tail, value + value)

    forAll { (int: Int, string: String) =>
      val atom     = Atom(int :: HNil, string)
      val expected = Atom.apply[Int :: HNil, String].tupled(mapFunction(atom.keys, atom.value))

      assert(atom.map(mapFunction) == expected)
    }
  }

  test("mapKeys") {
    def mapFunction[T <: HList](keys: Int :: T): Int :: T = (keys.head + 1) :: keys.tail

    forAll { (int: Int, string: String) =>
      val atom     = Atom(int :: HNil, string)
      val expected = Atom(mapFunction(atom.keys), atom.value)

      assert(atom.mapKeys(mapFunction) == expected)
    }
  }
}
