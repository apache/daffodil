package daffodil.section07.escapeScheme

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestEscapeScheme extends JUnit3Suite {
  val testDir = "/daffodil/section07/escapeScheme/"
  val tdml = testDir + "escapeScheme.tdml"
  val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  def test_escape_scheme() { runner.runOneTest("escapeSchemeSimple") }
  
  }
