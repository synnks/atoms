package synnks.atoms.ops

import org.scalacheck.Prop.*
import synnks.atoms.*
import synnks.atoms.hlist.*

class UngroupByTests extends AtomsSuite {

  test("ungroupBy element outside of G compilation error") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      assertCompileErrorsContain(
        compileErrors("groupedAtoms.ungroupBy[Boolean :: HNil]"),
        "Cannot create UngroupBy[",
        "contains elements that do not exist in",
        "or do not appear in the same order",
        "groupedAtoms.ungroupBy[Boolean :: HNil]"
      )
    }
  }

  test("ungroupBy elements of G out of order compilation error") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      assertCompileErrorsContain(
        compileErrors("groupedAtoms.ungroupBy[String :: Int :: HNil]"),
        "Cannot create UngroupBy[",
        "contains elements that do not exist in",
        "or do not appear in the same order",
        "groupedAtoms.ungroupBy[String :: Int :: HNil]"
      )
    }
  }

  test("ungroupBy[HNil] on Atoms idempotency") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>

      assertTypedEquals[Atoms[Int :: String :: HNil, Double]](
        atoms.ungroupBy[HNil],
        atoms
      )
    }
  }

  test("ungroupBy[HNil] on GroupedAtoms[HNil, K, V] downcast to Atoms[K, V]") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms: GroupedAtoms[HNil, Int :: String :: HNil, Double]

      assertTypedEquals[Atoms[Int :: String :: HNil, Double]](
        groupedAtoms.ungroupBy[HNil],
        atoms
      )
    }
  }

  test("ungroupBy[HNil] on GroupedAtoms[GH :: GT, K, V] idempotency") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

      assertTypedEquals[GroupedAtoms[Int :: String :: HNil, HNil, Double]](
        groupedAtoms.ungroupBy[HNil],
        groupedAtoms
      )
    }
  }

  test("ungroupBy head of G") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      val expected     = atoms.groupBy[String :: HNil]

      assertTypedEquals[GroupedAtoms[String :: HNil, Int :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: HNil],
        expected
      )
    }
  }

  test("ungroupBy[G]") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]

      assertTypedEquals[Atoms[Int :: String :: Boolean :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: String :: Boolean :: HNil],
        atoms
      )
    }
  }

  test("ungroupBy non consecutive elements of G") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]
      val expected     = atoms.groupBy[String :: HNil]

      assertTypedEquals[GroupedAtoms[String :: HNil, Int :: Boolean :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: Boolean :: HNil],
        expected
      )
    }
  }

  test("ungroupBy complements groupBy") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      assertTypedEquals[Atoms[Int :: String :: Boolean :: HNil, Double]](
        atoms.groupBy[Boolean :: HNil].ungroupBy[Boolean :: HNil],
        atoms
      )
    }
  }

  test("ungroupBy associativity") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]
      val expected     = groupedAtoms.ungroupBy[Int :: String :: Boolean :: HNil]

      assertTypedEquals[Atoms[Int :: String :: Boolean :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: HNil].ungroupBy[String :: HNil].ungroupBy[Boolean :: HNil],
        expected
      )
      assertTypedEquals[Atoms[Int :: String :: Boolean :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: HNil].ungroupBy[String :: Boolean :: HNil],
        expected
      )
      assertTypedEquals[Atoms[Int :: String :: Boolean :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: String :: HNil].ungroupBy[Boolean :: HNil],
        expected
      )
    }
  }
}
