package daffodil.section6.entities

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestClassEntities_01 extends JUnit3Suite {
  
  val testDir_01 = "srcDebug/daffodil/section6/entities/"
  val aa_01 = testDir_01 + "charClassEntities.tdml"
  val runner_01 = new DFDLTestSuite(new File(aa_01))
  
  def test_LineFeed() { runner_01.runOneTest("LineFeed") }
  def test_CarriageReturn() { runner_01.runOneTest("CarriageReturn") }
  def test_LineSeparator() { runner_01.runOneTest("LineSeparator") }
  def test_NextLine() { runner_01.runOneTest("NextLine") }
  
  }
