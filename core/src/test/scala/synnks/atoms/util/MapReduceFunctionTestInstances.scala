package synnks.atoms.util

import cats.Eq
import org.scalacheck.{ Arbitrary, Cogen }
import shapeless.*
import synnks.atoms.mapreduce.*

trait MapReduceFunctionTestInstances {

  implicit def chainedMapReduceFunctionEq[K <: HList, V <: HList]: Eq[ChainedMapReduceFunction[K, V]] =
    Eq.fromUniversalEquals

  implicit def mapReduceFunctionArbitrary[K: Cogen, V: Cogen, R: Cogen: Arbitrary]
    : Arbitrary[MapReduceFunction[K, V, R]] = Arbitrary(
    for {
      map    <- Arbitrary.arbitrary[(K, V) => R]
      reduce <- Arbitrary.arbitrary[(R, R) => R]
    } yield new MapReduceFunction(map, reduce)
  )

  implicit def toChainedMapReduceFunctionArbitrary[K, V, R](implicit
    arbitrary: Arbitrary[MapReduceFunction[K, V, R]]
  ): Arbitrary[ChainedMapReduceFunction[K :: HNil, R :: V :: HNil]] =
    Arbitrary(arbitrary.arbitrary.map(toChainedMapReduceFunction))

  implicit def chainedMapReduceFunctionArbitrary[KH, `KH+1`, VH, `VH+1`, KT <: HList, VT <: HList](implicit
    mapReduceFnArbitrary: Arbitrary[MapReduceFunction[KH, `VH+1`, VH]],
    chainedMapReduceFnArbitrary: Arbitrary[ChainedMapReduceFunction[`KH+1` :: KT, `VH+1` :: VT]]
  ): Arbitrary[ChainedMapReduceFunction[KH :: `KH+1` :: KT, VH :: `VH+1` :: VT]] =
    Arbitrary(
      for {
        mapReduceFn <- mapReduceFnArbitrary.arbitrary
        next        <- chainedMapReduceFnArbitrary.arbitrary
      } yield ChainedMapReduceFunction.Init(mapReduceFn, next)
    )
}
