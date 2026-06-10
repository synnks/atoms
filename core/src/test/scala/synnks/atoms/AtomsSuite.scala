package synnks.atoms

import cats.Eq
import cats.syntax.all.*
import munit.diff.DiffOptions
import munit.{ Location, ScalaCheckSuite }
import org.scalacheck.Test
import synnks.atoms.util.*

import scala.annotation.nowarn

trait AtomsSuite
    extends ScalaCheckSuite
    with HListTestInstances
    with AtomsTestInstances
    with MapReduceFunctionTestInstances {

  override protected def scalaCheckTestParameters: Test.Parameters = Test.Parameters.defaultVerbose

  @nowarn("msg=Implicit parameters should be provided with a `using` clause")
  protected def assertTypedEquals[A: Eq](actual: A, expected: A): Unit =
    assertEquals(actual, expected)(Location.generate, _ === _, DiffOptions.default)

  protected def assertCompileErrorsContain(actual: String, expected: String*): Unit =
    expected.foreach { message =>
      assert(
        actual.contains(message),
        s"Expected compile errors to contain:\n$message\n\nActual compile errors:\n$actual"
      )
    }
}
