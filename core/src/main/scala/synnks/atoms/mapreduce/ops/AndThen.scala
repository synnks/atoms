package synnks.atoms.mapreduce.ops

import shapeless.*
import synnks.atoms.mapreduce.*

import scala.annotation.implicitNotFound

@implicitNotFound("""
Cannot create AndThen[${K1}, ${IR1}, ${R1}, ${K2}, ${IR2}, ${R2}] instance.
The last element of the `IR2` type of the second MapReduceFunction[..., ${IR2}, ...] does not match the type of the result from the first MapReduceFunction[..., ..., ${R1}].
""")
sealed trait AndThen[K1 <: HList, IR1 <: HList, R1, K2 <: HList, IR2 <: HList, R2] {
  type Out

  def apply(f: MapReduceFunction[K1, IR1, R1], g: MapReduceFunction[K2, IR2, R2]): Out
}

object AndThen {
  type Aux[K1 <: HList, IR1 <: HList, R1, K2 <: HList, IR2 <: HList, R2, Out0] = AndThen[K1, IR1, R1, K2, IR2, R2] {
    type Out = Out0
  }

  @inline def apply[K1 <: HList, IR1 <: HList, R1, K2 <: HList, IR2 <: HList, R2](implicit
    instance: AndThen[K1, IR1, R1, K2, IR2, R2]
  ): AndThen.Aux[K1, IR1, R1, K2, IR2, R2, instance.Out] = instance

  implicit def andThenIdentity[K1 <: HList, IR1 <: HList, R1, R2]
    : AndThen.Aux[K1, IR1, R1, HNil, HNil, R2, MapReduceFunction[K1, IR1, R1]] =
    new AndThen[K1, IR1, R1, HNil, HNil, R2] {
      override type Out = MapReduceFunction[K1, IR1, R1]

      override def apply(f: MapReduceFunction[K1, IR1, R1], g: MapReduceFunction[HNil, HNil, R2]): Out = f
    }

  implicit def andThenRecurse[
    K1 <: HList,
    IR1 <: HList,
    R1,
    K2H,
    K2T <: HList,
    IR2H,
    IR2T <: HList,
    R2,
    NK <: HList,
    NIR <: HList
  ](implicit
    andThen: AndThen.Aux[K1, IR1, R1, K2T, IR2T, IR2H, MapReduceFunction[NK, NIR, IR2H]]
  ): AndThen.Aux[K1, IR1, R1, K2H :: K2T, IR2H :: IR2T, R2, MapReduceFunction[K2H :: NK, IR2H :: NIR, R2]] =
    new AndThen[K1, IR1, R1, K2H :: K2T, IR2H :: IR2T, R2] {
      override type Out = MapReduceFunction[K2H :: NK, IR2H :: NIR, R2]

      override def apply(
        f: MapReduceFunction[K1, IR1, R1],
        g: MapReduceFunction[K2H :: K2T, IR2H :: IR2T, R2]
      ): Out = g match {
        case g: MapReduceFunction.Chain[K2H, K2T, IR2H, IR2T, R2] =>
          MapReduceFunction(g.map, g.reduce, andThen(f, g.next))
      }
    }
}
