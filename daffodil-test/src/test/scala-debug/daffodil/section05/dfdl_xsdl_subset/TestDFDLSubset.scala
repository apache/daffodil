package daffodil.section05.dfdl_xsdl_subset

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

import daffodil.debugger.Debugger

class TestDFDLSubset extends JUnitSuite {
  val testDir = "/daffodil/section05/dfdl_xsdl_subset/"
  val tdml = testDir + "DFDLSubset.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_group_ref() { runner.runOneTest("group_ref") }

  // Fails due to not laying down separator correctly for sequence within sequence
  @Test def test_groupRefGroupRef() { Debugger.withDebugger { runner.runOneTest("groupRefGroupRef") } }

  // Fails, not laying down separator correctly
  @Test def test_sequenceWithinSequence() { Debugger.withDebugger { runner.runOneTest("sequenceWithinSequence") } }

  // Fails due to not laying down separator correctly
  @Test def test_refInitiator3() { Debugger.withDebugger { runner.runOneTest("refInitiator3") } }

  // Running as of 09282012
  @Test def test_groupRef() { Debugger.withDebugger { runner.runOneTest("groupRef") } }
  // Tests that initiator is found when on ElementRef
  @Test def test_refInitiator() { Debugger.withDebugger { runner.runOneTest("refInitiator") } }
  // Tests that initiator is found when on GlobalElmentDecl
  @Test def test_refInitiator2() { Debugger.withDebugger { runner.runOneTest("refInitiator2") } }
  @Test def test_groupRefInheritProps() { Debugger.withDebugger { runner.runOneTest("groupRefInheritProps") } }

}
