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

        assertCompileErrorsContain(
          compileErrors("groupedAtoms.mapReduce(f)"),
          "Cannot create MapReduce[",
          "The type of the last element of",
          "needs to be Atoms[",
          "groupedAtoms.mapReduce(f)"
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

      val prependString: MapReduceFunction[String :: HNil, Atoms[HNil, Double] :: HNil, Atoms[String :: HNil, Double]] =
        MapReduceFunction[String, Atoms[HNil, Double], Atoms[String :: HNil, Double]](prependKey[HNil, Double, String])
      val prependInt
        : MapReduceFunction[Int :: HNil, Atoms[String :: HNil, Double] :: HNil, Atoms[Int :: String :: HNil, Double]]  =
        MapReduceFunction[Int, Atoms[String :: HNil, Double], Atoms[Int :: String :: HNil, Double]](
          prependKey[String :: HNil, Double, Int]
        )
      val result                                                                                                       = groupedAtoms.mapReduce(prependString.andThen(prependInt))

      assertTypedEquals[Atoms[Int :: String :: HNil, Double]](result, atoms)
    }
  }
}
