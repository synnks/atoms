package synnks.atoms.ops

import org.scalacheck.Prop.*
import shapeless.*
import synnks.atoms.*

class GroupByTests extends AtomsSuite {

  test("groupBy element outside of K compilation error") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      assertNoDiff(
        compileErrors("atoms.groupBy[Boolean :: HNil]"),
        s"""|error:
            |Cannot create GroupBy[Boolean :: shapeless.HNil, shapeless.HNil, Int :: String :: shapeless.HNil, Double] instance.
            |Boolean :: shapeless.HNil contains elements that do not exist in Int :: String :: shapeless.HNil.
            |atoms.groupBy[Boolean :: HNil]
            |             ^
            |""".stripMargin
      )
    }
  }

  test("groupBy[HNil] on Atoms[K, V] upcast to GroupedAtoms[HNil, K, V]") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = atoms: GroupedAtoms[HNil, Int :: String :: HNil, Double]

      assertTypedEquals[GroupedAtoms[HNil, Int :: String :: HNil, Double]](
        atoms.groupBy[HNil],
        expected
      )
    }
  }

  test("groupBy[HNil] on GroupedAtoms idempotency") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = atoms: GroupedAtoms[HNil, Int :: String :: HNil, Double]

      assertTypedEquals[GroupedAtoms[HNil, Int :: String :: HNil, Double]](
        expected.groupBy[HNil],
        expected
      )
    }
  }

  test("groupBy head of K") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = NestedAtoms(
        atoms.values
          .groupMapNem(_.keys.head)(_.mapKeys(_.tail))
          .map(Atoms(_))
      ): GroupedAtoms[Int :: HNil, String :: HNil, Double]

      assertTypedEquals[GroupedAtoms[Int :: HNil, String :: HNil, Double]](
        atoms.groupBy[Int :: HNil],
        expected
      )
    }
  }

  test("groupBy element of tail of K") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = NestedAtoms(
        atoms.values
          .groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
          .map(Atoms(_))
      ): GroupedAtoms[String :: HNil, Int :: HNil, Double]

      assertTypedEquals[GroupedAtoms[String :: HNil, Int :: HNil, Double]](
        atoms.groupBy[String :: HNil],
        expected
      )
    }
  }

  test("groupBy[K]") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val expected = NestedAtoms(
        atoms.values
          .groupMapNem(_.keys.select[Int])(_.mapKeys(_.removeElem[Int]._2))
          .map(
            _.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
              .map(_.groupMapNem(_.keys.select[Boolean])(_.mapKeys(_.removeElem[Boolean]._2)).map(Atoms(_)))
              .map(NestedAtoms(_))
          )
          .map(NestedAtoms(_))
      ): GroupedAtoms[Int :: String :: Boolean :: HNil, HNil, Double]

      assertTypedEquals[GroupedAtoms[Int :: String :: Boolean :: HNil, HNil, Double]](
        atoms.groupBy[Int :: String :: Boolean :: HNil],
        expected
      )
    }
  }

  test("groupBy non consecutive elements of K") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val expected = NestedAtoms(
        atoms.values
          .groupMapNem(_.keys.select[Int])(_.mapKeys(_.removeElem[Int]._2))
          .map(_.groupMapNem(_.keys.select[Boolean])(_.mapKeys(_.removeElem[Boolean]._2)).map(Atoms(_)))
          .map(NestedAtoms(_))
      ): GroupedAtoms[Int :: Boolean :: HNil, String :: HNil, Double]

      assertTypedEquals[GroupedAtoms[Int :: Boolean :: HNil, String :: HNil, Double]](
        atoms.groupBy[Int :: Boolean :: HNil],
        expected
      )
    }
  }

  test("groupBy associativity") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val expected = atoms.groupBy[Int :: String :: Boolean :: HNil]

      assertTypedEquals[GroupedAtoms[Int :: String :: Boolean :: HNil, HNil, Double]](
        atoms.groupBy[Int :: HNil].groupBy[String :: HNil].groupBy[Boolean :: HNil],
        expected
      )
      assertTypedEquals[GroupedAtoms[Int :: String :: Boolean :: HNil, HNil, Double]](
        atoms.groupBy[Int :: HNil].groupBy[String :: Boolean :: HNil],
        expected
      )
      assertTypedEquals[GroupedAtoms[Int :: String :: Boolean :: HNil, HNil, Double]](
        atoms.groupBy[Int :: String :: HNil].groupBy[Boolean :: HNil],
        expected
      )
    }
  }
}
