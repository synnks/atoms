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

  test("groupBy skip elements of K") {
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

  test("ungroupBy element outside of G compilation error") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      assertNoDiff(
        compileErrors("groupedAtoms.ungroupBy[Boolean :: HNil]"),
        s"""|error:
            |Cannot create UngroupBy[Boolean :: shapeless.HNil, Int :: String :: shapeless.HNil, shapeless.HNil, Double] instance.
            |Boolean :: shapeless.HNil contains elements that do not exist in Int :: String :: shapeless.HNil, or do not appear in the same order.
            |groupedAtoms.ungroupBy[Boolean :: HNil]
            |                      ^
            |""".stripMargin
      )
    }
  }

  test("ungroupBy element of G out of order compilation error") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      assertNoDiff(
        compileErrors("groupedAtoms.ungroupBy[String :: Int :: HNil]"),
        s"""|error:
            |Cannot create UngroupBy[String :: Int :: shapeless.HNil, Int :: String :: shapeless.HNil, shapeless.HNil, Double] instance.
            |String :: Int :: shapeless.HNil contains elements that do not exist in Int :: String :: shapeless.HNil, or do not appear in the same order.
            |groupedAtoms.ungroupBy[String :: Int :: HNil]
            |                      ^
            |""".stripMargin
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
        // FIXME ungroupBy returns keys in reverse order
        groupedAtoms.ungroupBy[Int :: String :: Boolean :: HNil].mapKeys(_.reverse),
        atoms
      )
    }
  }

  test("ungroupBy skip elements of G") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]
      val expected = atoms.groupBy[String :: HNil]

      assertTypedEquals[GroupedAtoms[String :: HNil, Int :: Boolean :: HNil, Double]](
        // FIXME ungroupBy returns keys in reverse order
        groupedAtoms.ungroupBy[Int :: Boolean :: HNil].mapKeys(_.reverse),
        expected
      )
    }
  }

  test("ungroupBy complements groupBy") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      assertTypedEquals[Atoms[Int :: String :: Boolean :: HNil, Double]](
        atoms.groupBy[Int :: HNil].ungroupBy[Int :: HNil],
        atoms
      )
    }
  }

  test("ungroupBy associativity") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]
      val expected     = groupedAtoms.ungroupBy[Int :: String :: Boolean :: HNil]

      // FIXME ungroupBy returns keys in reverse order
      assertTypedEquals[Atoms[Boolean :: String :: Int :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: HNil].ungroupBy[String :: HNil].ungroupBy[Boolean :: HNil],
        expected
      )
      assertTypedEquals[Atoms[Boolean :: String :: Int :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: HNil].ungroupBy[String :: Boolean :: HNil],
        expected
      )
      assertTypedEquals[Atoms[Boolean :: String :: Int :: HNil, Double]](
        groupedAtoms.ungroupBy[Int :: String :: HNil].ungroupBy[Boolean :: HNil],
        expected
      )
    }
  }
}
