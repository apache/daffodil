package daffodil.section13.nillable


import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestNillable extends JUnit3Suite {
  val testDir = "src/daffodil/section13/nillable/"
  val aa = testDir + "nillable.tdml"
  val runnerAA = new DFDLTestSuite(new File(aa))

  def test_litNil1() { runnerAA.runOneTest("litNil1") }
  def test_litNil2() { runnerAA.runOneTest("litNil2") }

}