package synnks.atoms.mapreduce.ops

import org.scalacheck.Prop.*
import shapeless.*
import synnks.atoms.*
import synnks.atoms.mapreduce.*
import synnks.atoms.util.MapReduceFunctionTestInstances

class AndThenTests extends AtomsSuite with MapReduceFunctionTestInstances {

  test("andThen between incompatible functions compilation error") {
    forAll {
      (
        chainedMapReduceFn: ChainedMapReduceFunction[String :: HNil, Boolean :: Int :: HNil],
        mapReduceFn: MapReduceFunction[String, String, String]
      ) =>
        assertNoDiff(
          compileErrors("chainedMapReduceFn.andThen(mapReduceFn)"),
          s"""
             |error:
             |
             |Cannot create AndThen[String :: shapeless.HNil, Boolean :: Int :: shapeless.HNil, String, String, String] instance.
             |The `V` type of the MapReduceFunction[..., String, ...] does not match the head of the `V` HList of ChainedMapReduceFunction[..., Boolean :: Int :: shapeless.HNil]
             |
             |chainedMapReduceFn.andThen(mapReduceFn)
             |                          ^
             |""".stripMargin
        )
    }
  }

  test("andThen between compatible functions") {
    forAll {
      (
        chainedMapReduceFn: ChainedMapReduceFunction[String :: HNil, Boolean :: Int :: HNil],
        mapReduceFn: MapReduceFunction[String, Boolean, String]
      ) =>
        val expected =
          ChainedMapReduceFunction.Init(mapReduceFn, chainedMapReduceFn)

        assertTypedEquals[ChainedMapReduceFunction[String :: String :: HNil, String :: Boolean :: Int :: HNil]](
          chainedMapReduceFn andThen mapReduceFn,
          expected
        )
    }
  }
}
