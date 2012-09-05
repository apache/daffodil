package daffodil.section12.lengthKind

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestLengthKindDelimitedDebug extends JUnit3Suite {
  val testDir = "/daffodil/section12/lengthKind/"
  
  val ab = testDir + "AB.tdml"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))
    
  def test_AB004() { runnerAB.runOneTest("AB004") }
  def test_AB005() { runnerAB.runOneTest("AB005") }
  }
