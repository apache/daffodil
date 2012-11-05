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
import daffodil.debugger.Debugger

class TestFacetsNew extends JUnitSuite {
  val testDir = "/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_facet_pattern_01() { runner.runOneTest("facetPattern01") }
  @Test def test_facet_pattern_02() { runner.runOneTest("facetPattern02") }
  @Test def test_facet_pattern_03() { runner.runOneTest("facetPattern03") }
}