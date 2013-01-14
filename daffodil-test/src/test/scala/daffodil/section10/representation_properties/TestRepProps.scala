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
import daffodil.debugger.Debugger

class TestRepProps extends JUnitSuite {
  val testDir = "/daffodil/section10/representation_properties/"
  val aa = testDir + "RepProps.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_repPropMissing() { runner.runOneTest("repPropMissing") }
  @Test def test_repPropMissing2() { runner.runOneTest("repPropMissing2") }
//  @Test def test_repPropMissing3() { runner.runOneTest("repPropMissing3") }

}
