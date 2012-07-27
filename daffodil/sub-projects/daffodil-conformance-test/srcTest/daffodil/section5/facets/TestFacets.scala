package daffodil.section5.facets

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestFacets extends JUnit3Suite {
  val testDir = "srcTest/daffodil/section5/facets/"
  val aa = testDir + "Facets.tdml"
  val runner = new DFDLTestSuite(new File(aa))
  
  def test_error_minOccurs() { runner.runOneTest("error_minOccurs") }
  
  }
