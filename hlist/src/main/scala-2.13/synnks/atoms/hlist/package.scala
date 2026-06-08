package synnks.atoms

package object hlist {
  type HList = shapeless.HList
  type HNil  = shapeless.HNil
  val HNil: shapeless.HNil = shapeless.HNil

  type ::[H, T <: HList] = shapeless.::[H, T]
  val :: : shapeless.::.type = shapeless.::

  implicit final class HListOps[L <: HList](private val hlist: L) extends AnyVal {
    def :+[E, Out <: HList](element: E)(implicit prepend: shapeless.ops.hlist.Prepend.Aux[L, E :: HNil, Out]): Out =
      new shapeless.syntax.HListOps(hlist).:+(element)

    def removeElem[E](implicit remove: shapeless.ops.hlist.Remove[L, E]): remove.Out =
      new shapeless.syntax.HListOps(hlist).removeElem[E]
  }
}
