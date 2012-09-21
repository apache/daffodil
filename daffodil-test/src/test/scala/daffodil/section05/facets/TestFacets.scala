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

class TestFacets extends JUnitSuite {
  val testDir = "/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
    def test_correctNumCells0() { runner.runOneTest("correctNumCells0") }
    def test_extraCellsParsed() { runner.runOneTest("extraCellsParsed") }
    def test_scalarElements() { runner.runOneTest("scalarElements") }
    def test_lessThanMinBasic() { runner.runOneTest("lessThanMinBasic") }
    def test_moreThanMaxBasic() { runner.runOneTest("moreThanMaxBasic") }
    def test_arrayElements() { runner.runOneTest("arrayElements") }
    def test_upToMaxParsed() { runner.runOneTest("upToMaxParsed") }
    def test_lessThanMinCells() { runner.runOneTest("lessThanMinCells") }
    def test_moreThanMinCells() { runner.runOneTest("moreThanMinCells") }
    def test_lessThanMinCellsAfterLargeRow() { runner.runOneTest("lessThanMinCellsAfterLargeRow") }
    def test_largeNumRows() { runner.runOneTest("largeNumRows") }
    def test_minMaxDoNotMatch() { runner.runOneTest("minMaxDoNotMatch") }

  }
