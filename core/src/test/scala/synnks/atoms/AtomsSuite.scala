package synnks.atoms

import cats.Eq
import cats.syntax.all.*
import munit.{ Location, ScalaCheckSuite }
import org.scalacheck.Test
import synnks.atoms.util.*

trait AtomsSuite
    extends ScalaCheckSuite
    with HListTestInstances
    with AtomsTestInstances
    with MapReduceFunctionTestInstances {

  override protected def scalaCheckTestParameters: Test.Parameters = Test.Parameters.defaultVerbose

  protected def assertTypedEquals[A: Eq](actual: A, expected: A): Unit =
    assertEquals(actual, expected)(Location.generate, _ === _)
}
