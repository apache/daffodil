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
  @Test def test_prefix() { runner.runOneTest("prefix") }
   @Test def test_groupRef() { Debugger.withDebugger{runner.runOneTest("groupRef") }}
  
  }
