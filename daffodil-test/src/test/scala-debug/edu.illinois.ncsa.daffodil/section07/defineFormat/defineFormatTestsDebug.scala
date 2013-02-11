package edu.illinois.ncsa.daffodil.section07.defineFormat

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

class defineFormatTestsDebug extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section07/defineFormat/"
  val tdml = testDir + "defineFormat.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_nameCollision() { runner.runOneTest("nameCollision") }

}
