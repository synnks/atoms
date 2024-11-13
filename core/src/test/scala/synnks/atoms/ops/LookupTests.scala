package synnks.atoms.ops

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop.*
import shapeless.*
import synnks.atoms.*

class LookupTests extends AtomsSuite {

  test("lookup element outside of G compilation error") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      assertNoDiff(
        compileErrors("groupedAtoms.lookup(true :: HNil)"),
        s"""|error:
            |
            |Cannot create Lookup[Boolean :: shapeless.HNil, Int :: String :: shapeless.HNil, shapeless.HNil, Double] instance.
            |Boolean :: shapeless.HNil contains elements that do not exist in Int :: String :: shapeless.HNil, or do not appear in the same order.
            |
            |groupedAtoms.lookup(true :: HNil)
            |                   ^
            |""".stripMargin
      )
    }
  }

  test("lookup elements of G out of order compilation error") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      assertNoDiff(
        compileErrors("groupedAtoms.lookup(\"Hello\" :: 1 :: HNil)"),
        s"""|error:
            |
            |Cannot create Lookup[String :: Int :: shapeless.HNil, Int :: String :: shapeless.HNil, shapeless.HNil, Double] instance.
            |String :: Int :: shapeless.HNil contains elements that do not exist in Int :: String :: shapeless.HNil, or do not appear in the same order.
            |
            |groupedAtoms.lookup("Hello" :: 1 :: HNil)
            |                   ^
            |""".stripMargin
      )
    }
  }

  test("lookup(HNil) on Atoms identity") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val lookupResult = atoms.lookup(HNil: HNil)

      assertTypedEquals[Option[Atoms[Int :: String :: HNil, Double]]](
        lookupResult,
        Some(atoms)
      )
    }
  }

  test("lookup(HNil) on GroupedAtoms[G, K, V] ungroup by G") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]
      val lookupResult = groupedAtoms.lookup(HNil: HNil)

      assertTypedEquals[Option[Atoms[Int :: String :: HNil, Double]]](
        lookupResult,
        Some(atoms)
      )
    }
  }

  test("lookup hits using head of G") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expectedMap  = atoms.values
        .groupMapNem(_.keys.select[Int])(_.mapKeys(_.removeElem[Int]._2))
        .map(_.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2)))
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

      forAll(Gen.oneOf(atoms.values.toList)) { randomAtom =>
        val lookupKey    = randomAtom.keys.select[Int]
        val lookupResult = groupedAtoms.lookup(lookupKey :: HNil)

        val expectedLookupResult = expectedMap
          .lookup(lookupKey)
          .map(_.transform((string, atoms) => Atoms(atoms).mapKeys(string :: _)).reduce)

        assertTypedEquals[Option[Atoms[String :: HNil, Double]]](
          lookupResult,
          expectedLookupResult
        )
      }
    }
  }

  test("lookup misses using head of G") {
    implicit val positiveIntArbitrary: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

      forAll(Gen.negNum[Int]) { negativeInt =>
        val lookupKeys   = negativeInt :: HNil
        val lookupResult = groupedAtoms.lookup(lookupKeys)

        assertTypedEquals[Option[Atoms[String :: HNil, Double]]](
          lookupResult,
          None
        )
      }
    }
  }

  test("lookup hits using non-head element of G") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val expectedMap  = atoms.values
        .groupMapNem(_.keys.select[Int])(_.mapKeys(_.removeElem[Int]._2))
        .map(_.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2)))
      val groupedAtoms = atoms.groupBy[String :: Int :: HNil]

      forAll(Gen.oneOf(atoms.values.toList)) { randomAtom =>
        val lookupKey    = randomAtom.keys.select[Int]
        val lookupResult = groupedAtoms.lookup(lookupKey :: HNil)

        val expectedLookupResult = expectedMap
          .lookup(lookupKey)
          .map(_.transform((string, atoms) => Atoms(atoms).mapKeys(string :: _)).reduce)

        assertTypedEquals[Option[Atoms[String :: HNil, Double]]](
          lookupResult,
          expectedLookupResult
        )
      }
    }
  }

  test("lookup misses using non-head element of G") {
    implicit val positiveIntArbitrary: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[String :: Int :: HNil]

      forAll(Gen.negNum[Int]) { negativeInt =>
        val lookupKeys   = negativeInt :: HNil
        val lookupResult = groupedAtoms.lookup(lookupKeys)

        assertTypedEquals[Option[Atoms[String :: HNil, Double]]](
          lookupResult,
          None
        )
      }
    }
  }

  test("lookup hits using all types of G") {
    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val expectedMap  = atoms.values
        .groupMapNem(_.keys.select[Int])(_.mapKeys(_.removeElem[Int]._2))
        .map(
          _.groupMapNem(_.keys.select[String])(_.mapKeys(_.removeElem[String]._2))
            .map(_.groupMapNem(_.keys.select[Boolean])(_.mapKeys(_.removeElem[Boolean]._2)))
        )
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]

      forAll(Gen.oneOf(atoms.values.toList)) { randomAtom =>
        val lookupKeys   = randomAtom.keys
        val lookupResult = groupedAtoms.lookup(lookupKeys)

        val expectedLookupResult = for {
          r1 <- expectedMap.lookup(lookupKeys.select[Int])
          r2 <- r1.lookup(lookupKeys.select[String])
          r3 <- r2.lookup(lookupKeys.select[Boolean])
        } yield Atoms(r3)

        assertTypedEquals[Option[Atoms[HNil, Double]]](
          lookupResult,
          expectedLookupResult
        )
      }
    }
  }

  test("lookup misses using all types of G") {
    implicit val positiveIntArbitrary: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])

    forAll { (atoms: Atoms[Int :: String :: Boolean :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: Boolean :: HNil]

      forAll(Gen.oneOf(atoms.values.toList), Gen.negNum[Int]) { (randomAtom, negativeInt) =>
        val lookupKeys   = randomAtom.keys.updatedElem(negativeInt)
        val lookupResult = groupedAtoms.lookup(lookupKeys)

        assertTypedEquals[Option[Atoms[HNil, Double]]](
          lookupResult,
          None
        )
      }
    }
  }
}
