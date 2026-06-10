package synnks.atoms.mapreduce.ops

import org.scalacheck.Prop.*
import synnks.atoms.*
import synnks.atoms.hlist.*
import synnks.atoms.mapreduce.*

class AndThenTests extends AtomsSuite {

  test("andThen between incompatible functions compilation error") {
    forAll {
      (
        f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean],
        g: MapReduceFunction[Double :: HNil, Long :: HNil, Unit]
      ) =>
        assertCompileErrorsContain(
          compileErrors("f andThen g"),
          "Cannot create AndThen[",
          "The last element of the `IR2` type of the second MapReduceFunction",
          "does not match the type of the result from the first MapReduceFunction",
          "f andThen g"
        )
    }
  }

  test("f andThen identity") {
    forAll { (f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean], g: MapReduceFunction[HNil, HNil, ?]) =>
      assertTypedEquals[MapReduceFunction[Int :: HNil, String :: HNil, Boolean]](f andThen g, f)
    }
  }

  test("f andThen g (1 element)") {
    forAll {
      (
        f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean],
        g: MapReduceFunction[Double :: HNil, Boolean :: HNil, Long]
      ) =>
        val expected = g match {
          case MapReduceFunction.Chain(map, reduce, next) => MapReduceFunction(map, reduce, f andThen next)
        }
        assertTypedEquals[MapReduceFunction[Double :: Int :: HNil, Boolean :: String :: HNil, Long]](
          f andThen g,
          expected
        )
    }
  }

  test("f andThen g (multiple elements)") {
    forAll {
      (
        f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean],
        g: MapReduceFunction[Short :: Float :: Double :: HNil, Char :: Byte :: Boolean :: HNil, Long]
      ) =>
        val expected = g match {
          case MapReduceFunction.Chain(map1, reduce1, next1) =>
            next1 match {
              case MapReduceFunction.Chain(map2, reduce2, next2) =>
                next2 match {
                  case MapReduceFunction.Chain(map3, reduce3, next3) =>
                    MapReduceFunction(
                      map1,
                      reduce1,
                      MapReduceFunction(
                        map2,
                        reduce2,
                        MapReduceFunction(
                          map3,
                          reduce3,
                          f andThen next3
                        )
                      )
                    )
                }
            }
        }
        assertTypedEquals[
          MapReduceFunction[Short :: Float :: Double :: Int :: HNil, Char :: Byte :: Boolean :: String :: HNil, Long]
        ](
          f andThen g,
          expected
        )
    }
  }

  test("andThen associativity") {
    forAll {
      (
        f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean],
        g: MapReduceFunction[Double :: HNil, Boolean :: HNil, Long],
        h: MapReduceFunction[Float :: HNil, Long :: HNil, Byte]
      ) =>
        assertTypedEquals[MapReduceFunction[Float :: Double :: Int :: HNil, Long :: Boolean :: String :: HNil, Byte]](
          (f andThen g) andThen h,
          f andThen (g andThen h)
        )
    }
  }

  test("compose between incompatible functions compilation error") {
    forAll {
      (
        f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean],
        g: MapReduceFunction[Double :: HNil, Long :: HNil, Unit]
      ) =>
        assertCompileErrorsContain(
          compileErrors("g compose f"),
          "Cannot create AndThen[",
          "The last element of the `IR2` type of the second MapReduceFunction",
          "does not match the type of the result from the first MapReduceFunction",
          "g compose f"
        )
    }
  }

  test("compose complements andThen") {
    forAll {
      (
        f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean],
        g: MapReduceFunction[Double :: HNil, Boolean :: HNil, Long]
      ) =>
        assertTypedEquals[MapReduceFunction[Double :: Int :: HNil, Boolean :: String :: HNil, Long]](
          f andThen g,
          g compose f
        )
    }
  }

  test("compose associativity") {
    forAll {
      (
        f: MapReduceFunction[Int :: HNil, String :: HNil, Boolean],
        g: MapReduceFunction[Double :: HNil, Boolean :: HNil, Long],
        h: MapReduceFunction[Float :: HNil, Long :: HNil, Byte]
      ) =>
        assertTypedEquals[MapReduceFunction[Float :: Double :: Int :: HNil, Long :: Boolean :: String :: HNil, Byte]](
          (h compose g) compose f,
          h compose (g compose f)
        )
    }
  }
}
