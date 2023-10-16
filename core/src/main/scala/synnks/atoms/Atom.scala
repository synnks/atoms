package synnks.atoms

import shapeless.*

final case class Atom[K <: HList, V](keys: K, value: V) {

  def map[NK <: HList, NV](f: (K, V) => (NK, NV)): Atom[NK, NV] =
    (Atom[NK, NV] _).tupled(f(keys, value))

  def mapKeys[NK <: HList](f: K => NK): Atom[NK, V] =
    map((keys, value) => (f(keys), value))
}
