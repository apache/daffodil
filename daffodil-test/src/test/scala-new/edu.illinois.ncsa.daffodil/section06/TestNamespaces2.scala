package edu.illinois.ncsa.daffodil.section06

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

class TestNamespaces2 extends JUnitSuite {
  val testDir = "edu.illinois.ncsa.daffodil/section06/namespaces/"
  val aa = testDir + "namespaces.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_Lesson2_include_schema() { runner.runOneTest("Lesson2_include_schema") }
  @Test def test_Lesson2_import_schema() { runner.runOneTest("Lesson2_import_schema") }

}
