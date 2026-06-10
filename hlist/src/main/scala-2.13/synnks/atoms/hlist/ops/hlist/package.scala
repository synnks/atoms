package synnks.atoms.hlist.ops

package object hlist {
  type Remove[L <: shapeless.HList, E] = shapeless.ops.hlist.Remove[L, E]
  val Remove: shapeless.ops.hlist.Remove.type = shapeless.ops.hlist.Remove
}
