package edu.illinois.ncsa.daffodil.section10.representation_properties

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File

class TestRepProps2 extends JUnitSuite {
  val testDir_01 = "/edu.illinois.ncsa.daffodil/section10/representation_properties/"
  val tdml1 = testDir_01 + "RepProps.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml1))

  @Test def test_repPropMissing3() { runner.runOneTest("repPropMissing3") }

}
