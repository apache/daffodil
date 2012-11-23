package daffodil.section06.entities

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

class TestEntities_01 extends JUnitSuite {

  val testDir_01 = "/daffodil/section06/entities/"
  val aa_01 = testDir_01 + "Entities.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(aa_01))

  //@Test def test_text_entities_6_02() { runner_01.runOneTest("text_entities_6_02") }
  @Test def test_byte_entities_6_06() { runner_01.runOneTest("byte_entities_6_06") }
  @Test def test_byte_entities_6_07() { runner_01.runOneTest("byte_entities_6_07") }

  val tdml = testDir_01 + "charClassEntities.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  @Test def test_HexCodePoint() { runner.runOneTest("HexCodePoint") }

}
