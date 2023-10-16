package synnks.atoms

import munit.ScalaCheckSuite
import org.scalacheck.Test
import synnks.atoms.util.{ AtomsTestInstances, HListTestInstances }

trait AtomsCheckSuite extends ScalaCheckSuite with HListTestInstances with AtomsTestInstances {

  override protected def scalaCheckTestParameters: Test.Parameters = Test.Parameters.defaultVerbose
}
