package daffodil.section06.entities

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestEntities_01 extends JUnit3Suite {
  
  val testDir_01 = "/daffodil/section06/entities/"
  val aa_01 = testDir_01 + "Entities.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(aa_01))
  
  def test_text_entities_6_02() { runner_01.runOneTest("text_entities_6_02") }
  def test_byte_entities_6_06() { runner_01.runOneTest("byte_entities_6_06") }
  def test_byte_entities_6_07() { runner_01.runOneTest("byte_entities_6_07") }
  
  }
