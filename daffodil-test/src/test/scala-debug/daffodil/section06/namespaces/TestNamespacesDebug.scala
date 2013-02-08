package daffodil.section06.namespaces

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

//class TestNamespacesDebug extends JUnitSuite {
//  val testDir = "/daffodil/section06/namespaces/"
//  val aa = testDir + "namespaces.tdml"
//  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
//
//  // these work. Moved to scala-new
////  @Test def test_Lesson2_include_schema() { runner.runOneTest("Lesson2_include_schema") }
////  @Test def test_Lesson2_import_schema() { runner.runOneTest("Lesson2_import_schema") }
//
//}
