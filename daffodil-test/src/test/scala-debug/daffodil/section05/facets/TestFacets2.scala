package daffodil.section05.facets

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

class TestFacets2 extends JUnitSuite {
  val testDir = "/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_minMaxDoNotMatch() { runner.runOneTest("minMaxDoNotMatch") }
  
  }
