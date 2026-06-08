package synnks.atoms.ops

import org.scalacheck.Prop.*
import synnks.atoms.*
import synnks.atoms.hlist.*
import synnks.atoms.mapreduce.*

class MapReduceTests extends AtomsSuite {

  test("mapReduce with invalid function compilation error") {
    forAll {
      (
        atoms: Atoms[Int :: String :: HNil, Double],
        f: MapReduceFunction[Int :: String :: HNil, Unit :: Unit :: HNil, Unit]
      ) =>
        val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

        assertNoDiff(
          compileErrors("groupedAtoms.mapReduce(f)"),
          s"""|error:
              |
              |Cannot create MapReduce[Int :: String :: shapeless.HNil, shapeless.HNil, Double, Unit :: Unit :: synnks.atoms.hlist.HNil, Unit] instance.
              |The type of the last element of Unit :: Unit :: synnks.atoms.hlist.HNil needs to be Atoms[shapeless.HNil, Double].
              |
              |groupedAtoms.mapReduce(f)
              |                      ^
              |""".stripMargin
        )
    }
  }

  test("mapReduce Atoms[K, V]") {
    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val result = atoms.mapReduce(MapReduceFunction.unit)

      assertTypedEquals[Atoms[Int :: String :: HNil, Double]](result, atoms)
    }
  }

  test("mapReduce GroupedAtoms[G, K, V]") {
    def prependKey[K <: HList, V, A](head: A, atoms: Atoms[K, V]): Atoms[A :: K, V] = atoms.mapKeys(head :: _)

    forAll { (atoms: Atoms[Int :: String :: HNil, Double]) =>
      val groupedAtoms = atoms.groupBy[Int :: String :: HNil]

      val result = groupedAtoms.mapReduce {
        prependKey[HNil, Double, String] _ andThen prependKey[String :: HNil, Double, Int] _
      }

      assertTypedEquals[Atoms[Int :: String :: HNil, Double]](result, atoms)
    }
  }
}
