package synnks.atoms

import cats.Eq
import cats.syntax.all.*
import munit.{ Compare, Location, ScalaCheckSuite }
import org.scalacheck.Test
import synnks.atoms.util.*

trait AtomsSuite
    extends ScalaCheckSuite
    with HListTestInstances
    with AtomsTestInstances
    with MapReduceFunctionTestInstances {

  override protected def scalaCheckTestParameters: Test.Parameters = Test.Parameters.defaultVerbose

  protected def assertTypedEquals[A: Eq](actual: A, expected: A)(implicit loc: Location): Unit = {
    implicit val compare: Compare[A, A] = _ === _
    assertEquals(actual, expected)
  }

  protected def assertCompileErrorsContain(actual: String, expected: String*)(implicit loc: Location): Unit =
    expected.foreach { message =>
      assert(
        actual.contains(message),
        s"Expected compile errors to contain:\n$message\n\nActual compile errors:\n$actual"
      )
    }
}
