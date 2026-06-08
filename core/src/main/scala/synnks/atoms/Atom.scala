package synnks.atoms

import synnks.atoms.hlist.*

final case class Atom[K <: HList, V](keys: K, value: V) {

  def map[NK <: HList, NV](f: (K, V) => (NK, NV)): Atom[NK, NV] = {
    val (newKeys, newValue) = f(keys, value)
    Atom(newKeys, newValue)
  }

  def mapKeys[NK <: HList](f: K => NK): Atom[NK, V] =
    map((keys, value) => (f(keys), value))
}
