package synnks.atoms.hlist.ops

package object hlist {
  type Prepend[L <: shapeless.HList, R <: shapeless.HList] = shapeless.ops.hlist.Prepend[L, R]
  val Prepend: shapeless.ops.hlist.Prepend.type = shapeless.ops.hlist.Prepend

  type Remove[L <: shapeless.HList, E] = shapeless.ops.hlist.Remove[L, E]
  val Remove: shapeless.ops.hlist.Remove.type = shapeless.ops.hlist.Remove
}
