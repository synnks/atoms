# Atoms

```scala
libraryDependencies += "com.synnks" %% "atoms" % "<version>"
```

Type-safe data cube for Scala.

`Atoms` is a small library for working with non-empty collections of keyed facts.

It lets you group, regroup, look up, unwrap, and aggregate heterogeneous data while preserving the shape of your dimensions at the type level.

Many data processing tasks start with flat facts:

```scala
(Country, Year, Product) -> Revenue
```

Then you need to reshape them:

```scala
Country -> Year -> Product -> Revenue
Year -> Country -> Product -> Revenue
Country -> Product -> Revenue
```

With ordinary maps, these transformations are easy to get subtly wrong. Dimension order is implicit, lookup keys can be mixed up, and regrouping logic tends to become ad hoc.

`Atoms` makes the dimensions part of the type.

## Example

```scala
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{ Order, Semigroup }
import synnks.atoms.*
import synnks.atoms.hlist.*
import synnks.atoms.mapreduce.*

final case class Country(value: String)
object Country {
  implicit val order: Order[Country] = Order.by(_.value)
}
final case class Year(value: Int)
object Year    {
  implicit val order: Order[Year] = Order.by(_.value)
}
final case class Product(value: String)
final case class Revenue(value: BigDecimal)
object Revenue {
  implicit val semigroup: Semigroup[Revenue] = Semigroup[BigDecimal].imap(Revenue.apply)(_.value)
}

// Start with flat facts
val atoms: Atoms[Country :: Year :: Product :: HNil, Revenue] =
  Atoms(
    NonEmptyList.of(
      (Country("DE") :: Year(2025) :: Product("A") :: HNil, Revenue(100)),
      (Country("DE") :: Year(2025) :: Product("B") :: HNil, Revenue(50)),
      (Country("FR") :: Year(2025) :: Product("A") :: HNil, Revenue(80))
    )
  )
  
// Group by country and year - dimensions move into the grouping type
val byCountryAndYear: GroupedAtoms[Country :: Year :: HNil, Product :: HNil, Revenue] =
  atoms.groupBy[Country :: Year :: HNil]

// Look up a specific slice
val germany: Option[Atoms[Product :: Year :: HNil, Revenue]] =
  byCountryAndYear.lookup(Country("DE") :: HNil)

val germany2025: Option[Atoms[Product :: HNil, Revenue]] =
  byCountryAndYear.lookup(Country("DE") :: Year(2025) :: HNil)

// Reshape: ungroup country, keeping only year as grouping
val byYear: GroupedAtoms[Year :: HNil, Product :: Country :: HNil, Revenue] =
  byCountryAndYear.ungroupBy[Country :: HNil]

// Aggregate: fold grouped data with a typed map-reduce pipeline
def sumByYear(year: Year, atoms: Atoms[Product :: HNil, Revenue]): Revenue =
  atoms.values.map(_.value).reduce

def sumByCountry(country: Country, perCountry: Revenue): Revenue =
  perCountry

val total: Revenue = byCountryAndYear.mapReduce(sumByYear andThen sumByCountry)
```

At the type level, `GroupedAtoms[G, K, V]` tracks both the grouping hierarchy (`G`) and the remaining fact dimensions (`K`). Each operation changes that shape explicitly, so invalid dimensional transformations are rejected before runtime.

## Intended Use Cases

ETL pipelines, data imports, validation, reporting, typed aggregation, business-rule-heavy batch processing, small analytical data cubes.

Especially helpful when dimensional correctness matters more than raw throughput.

## License

[Apache 2.0](LICENSE)
