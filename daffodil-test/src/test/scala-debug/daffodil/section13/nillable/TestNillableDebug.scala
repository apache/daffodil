package daffodil.section13.nillable

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File
import daffodil.debugger.Debugger.withDebugger
import daffodil.debugger.Debugger

class TestNillableDebug extends JUnitSuite {
  val testDir = "/daffodil/section13/nillable/"
  val aa = testDir + "nillable.tdml"
  lazy val runnerAA = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_litNil4() { runnerAA.runOneTest("litNil4") }
  @Test def test_litNil5() { runnerAA.runOneTest("litNil5") }
  @Test def test_litNil7() { runnerAA.runOneTest("litNil7") }

  val ln = testDir + "literal-value-nils.tdml"
  lazy val runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))

  // This test must be changed to use logicalValue nils.
  // For LiteralValue Nils, representation must be text!!!
  // LogicalValue Nils are not yet implemented.
  @Test def test_binary_01() { runnerLN.runOneTest("binary_01") }

  val testDir_01 = "/daffodil/section06/entities/"
  val entity = testDir_01 + "entities_01.tdml"
  lazy val runnerEntity = new DFDLTestSuite(Misc.getRequiredResource(entity))
  // This test must be changed to use logicalValue nils.
  // For LiteralValue Nils, representation must be text!!!
  // LogicalValue Nils are not yet implemented.
  @Test def test_entity_fail_05() { runnerEntity.runOneTest("entity_fail_05") }
  // This test must be changed to use logicalValue nils.
  // For LiteralValue Nils, representation must be text!!!
  // LogicalValue Nils are not yet implemented.
  @Test def test_entity_fail_06() { runnerEntity.runOneTest("entity_fail_06") }
}
