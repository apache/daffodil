package daffodil.section13.nillable


import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestNillable extends JUnit3Suite {
  val testDir = "/daffodil/section13/nillable/"
  val aa = testDir + "nillable.tdml"
  val runnerAA = new DFDLTestSuite(Misc.getRequiredResource(aa))

  def test_litNil1() { runnerAA.runOneTest("litNil1") }
  def test_litNil2() { runnerAA.runOneTest("litNil2") }
  def test_litNil3() { runnerAA.runOneTest("litNil3") }

}
