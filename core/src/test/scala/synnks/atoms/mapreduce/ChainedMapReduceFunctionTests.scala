package synnks.atoms.mapreduce

import org.scalacheck.Prop.*
import synnks.atoms.AtomsSuite

class ChainedMapReduceFunctionTests extends AtomsSuite {

  test("andThen and compose associativity") {
    forAll {
      (
        fn1: MapReduceFunction[Int, String, Boolean],
        fn2: MapReduceFunction[Long, Boolean, Float],
        fn3: MapReduceFunction[Double, Float, Byte]
      ) =>
        assertTypedEquals(
          fn1 andThen fn2 andThen fn3,
          fn3 compose fn2 compose fn1
        )
    }
  }
}
