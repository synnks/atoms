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

  test("groupBy HNil") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expected = atoms: GroupedAtoms[HNil, Int :: String :: HNil, Double]

      assertTypedEquals[GroupedAtoms[HNil, Int :: String :: HNil, Double]](
        atoms.groupBy[HNil],
        expected
      )
    }
  }

  test("groupBy 1 level head of HList") {
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

  test("groupBy 1 level mid element of HList") {
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

  test("groupBy all levels") {
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

  test("groupBy skip levels") {
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

  // FIXME GroupedAtoms[G, K, V].ungroupBy[G] should return Atoms[K, V] instead of GroupedAtoms[HNil, K, V]
  test("ungroupBy HNil Atoms".ignore) {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>

      assertEquals(
        atoms.ungroupBy[HNil],
        atoms
      )
    }
  }

  test("ungroupBy HNil GroupedAtoms") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

      assertTypedEquals[GroupedAtoms[Int :: String :: HNil, HNil, Double]](
        groupedAtoms.ungroupBy[HNil],
        groupedAtoms
      )
    }
  }

  test("ungroupBy 1 level head of HList") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      val expected     = atoms.groupBy[String :: HNil]

      assertTypedEquals[GroupedAtoms[String :: HNil, Int :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: HNil],
        expected
      )
    }
  }

  // FIXME GroupedAtoms[G, K, V].ungroupBy[G] should return Atoms[K, V] instead of GroupedAtoms[HNil, K, V]
  test("ungroupBy all levels".ignore) {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]

      assertTypedEquals(
        groupedAtoms.ungroupBy[Int :: String :: Boolean :: HNil].mapKeys(_.reverse),
        atoms
      )
    }
  }

  // FIXME GroupedAtoms[G, K, V].ungroupBy[G] should return Atoms[K, V] instead of GroupedAtoms[HNil, K, V]
  test("ungroupBy complements groupBy".ignore) {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      assertTypedEquals(
        atoms.groupBy[Int :: HNil].ungroupBy[Int :: HNil],
        atoms
      )
    }
  }

  // FIXME GroupedAtoms[G, K, V].ungroupBy[G] should return Atoms[K, V] instead of GroupedAtoms[HNil, K, V]
  test("ungroupBy associativity".ignore) {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]
      val expected     = groupedAtoms.ungroupBy[Int :: String :: Boolean :: HNil]

      assertEquals(
        groupedAtoms.ungroupBy[Int :: HNil].ungroupBy[String :: HNil].ungroupBy[Boolean :: HNil],
        expected
      )
      assertEquals(
        groupedAtoms.ungroupBy[Int :: HNil].ungroupBy[String :: Boolean :: HNil],
        expected
      )
      assertEquals(
        groupedAtoms.ungroupBy[Int :: String :: HNil].ungroupBy[Boolean :: HNil],
        expected
      )
    }
  }
}
