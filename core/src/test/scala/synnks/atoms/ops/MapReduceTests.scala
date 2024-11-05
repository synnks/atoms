package synnks.atoms.ops

import org.scalacheck.Prop.*
import shapeless.*
import synnks.atoms.*
import synnks.atoms.mapreduce.*

class MapReduceTests extends AtomsSuite {

  test("mapReduce with invalid function compilation error") {
    forAll {
      (
        atoms: Atoms[Int :: String :: HNil, Double],
        fn: ChainedMapReduceFunction[Int :: String :: HNil, Unit :: Unit :: Unit :: HNil]
      ) =>
        val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

        assertNoDiff(
          compileErrors("groupedAtoms.mapReduce(fn)"),
          s"""|error:
              |
              |Cannot create MapReduce[Unit :: Unit :: Unit :: shapeless.HNil, Int :: String :: shapeless.HNil, shapeless.HNil, Double] instance.
              |The type of the last element of Unit :: Unit :: Unit :: shapeless.HNil needs to be Atoms[shapeless.HNil, Double].
              |
              |groupedAtoms.mapReduce(fn)
              |                      ^
              |""".stripMargin
        )
    }
  }

  test("mapReduce") {
    def prependKey[K <: HList, V, A](head: A, atoms: Atoms[K, V]): Atoms[A :: K, V] = atoms.mapKeys(head :: _)

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

      val result = groupedAtoms.mapReduce {
        MapReduceFunction(prependKey[HNil, Double, String]) andThen
          MapReduceFunction(prependKey[String :: HNil, Double, Int])
      }

      assertTypedEquals[Atoms[Int :: String :: HNil, Double]](result, atoms)
    }
  }

}
