package synnks.atoms.util

import cats.Eq
import org.scalacheck.{ Arbitrary, Cogen, Gen }
import shapeless.*
import synnks.atoms.mapreduce.*

trait MapReduceFunctionTestInstances {

  implicit def mapReduceFunctionEq[K <: HList, IR <: HList, R]: Eq[MapReduceFunction[K, IR, R]] =
    Eq.fromUniversalEquals

  implicit def mapReduceFunctionUnitArbitrary[R]: Arbitrary[MapReduceFunction[HNil, HNil, R]] =
    Arbitrary(Gen.const(MapReduceFunction.Unit()))

  implicit def mapReduceFunctionArbitrary[KH: Cogen, KT <: HList, IRH: Cogen, IRT <: HList, R: Cogen: Arbitrary](
    implicit A: Arbitrary[MapReduceFunction[KT, IRT, IRH]]
  ): Arbitrary[MapReduceFunction[KH :: KT, IRH :: IRT, R]] = Arbitrary(
    for {
      map    <- Arbitrary.arbitrary[(KH, IRH) => R]
      reduce <- Arbitrary.arbitrary[(R, R) => R]
      next   <- Arbitrary.arbitrary[MapReduceFunction[KT, IRT, IRH]]
    } yield MapReduceFunction(map, reduce, next)
  )
}
