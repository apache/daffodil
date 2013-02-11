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

  @Test def test_litNil7() { runnerAA.runOneTest("litNil7") }
  @Test def test_empty_infoset() { runnerAA.runOneTest("empty_infoset") }
  
  val ln = testDir + "literal-value-nils.tdml"
  lazy val runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))

  val testDir_01 = "/daffodil/section06/entities/"
  val entity = testDir_01 + "entities_01.tdml"
  lazy val runnerEntity = new DFDLTestSuite(Misc.getRequiredResource(entity))
  @Test def test_entity_success_06() { runnerEntity.runOneTest("entity_success_06") }
}
