package synnks.atoms.mapreduce.ops

import org.scalacheck.Prop.*
import shapeless.*
import synnks.atoms.AtomsSuite
import synnks.atoms.mapreduce.*

class ComposeTests extends AtomsSuite {

  test("compose between incompatible functions compilation error") {
    forAll {
      (
        chainedMapReduceFn: ChainedMapReduceFunction[String :: HNil, Boolean :: Int :: HNil],
        mapReduceFn: MapReduceFunction[String, String, String]
      ) =>
        assertNoDiff(
          compileErrors("chainedMapReduceFn.compose(mapReduceFn)"),
          s"""
             |error:
             |
             |Cannot create Compose[String :: shapeless.HNil, Boolean :: Int :: shapeless.HNil, String, String, String] instance.
             |The `V` type of the MapReduceFunction[..., String, ...] does not match the last element of the `V` HList of ChainedMapReduceFunction[..., Boolean :: Int :: shapeless.HNil]
             |
             |chainedMapReduceFn.compose(mapReduceFn)
             |                          ^
             |""".stripMargin
        )
    }
  }

  test("compose between compatible functions") {
    forAll {
      (
        chainedMapReduceFn: ChainedMapReduceFunction[String :: HNil, Boolean :: Int :: HNil],
        mapReduceFn: MapReduceFunction[String, String, Int]
      ) =>
        val expected = chainedMapReduceFn match {
          case ChainedMapReduceFunction.Last(first) => ChainedMapReduceFunction.Init(first, mapReduceFn)
        }

        assertTypedEquals[ChainedMapReduceFunction[String :: String :: HNil, Boolean :: Int :: String :: HNil]](
          chainedMapReduceFn compose mapReduceFn,
          expected
        )
    }
  }
}
