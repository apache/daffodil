package daffodil.section10.representation_properties

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

class TestRepProps2 extends JUnitSuite {
  val testDir_01 = "/daffodil/section10/representation_properties/"
  val tdml1 = testDir_01 + "RepProps.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml1))

  @Test def test_repPropMissing3() { runner.runOneTest("repPropMissing3") }

}
