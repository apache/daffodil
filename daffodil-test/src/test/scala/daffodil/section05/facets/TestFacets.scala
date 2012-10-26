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

class TestFacets extends JUnitSuite {
  val testDir = "/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_correctNumCells0() { runner.runOneTest("correctNumCells0") }
  @Test def test_extraCellsParsed() { runner.runOneTest("extraCellsParsed") }
  @Test def test_scalarElements() { runner.runOneTest("scalarElements") }
  @Test def test_lessThanMinBasic() { runner.runOneTest("lessThanMinBasic") }
  @Test def test_moreThanMaxBasic() { runner.runOneTest("moreThanMaxBasic") }
  @Test def test_arrayElements() { runner.runOneTest("arrayElements") }
  @Test def test_upToMaxParsed() { runner.runOneTest("upToMaxParsed") }
  @Test def test_lessThanMinCells() { runner.runOneTest("lessThanMinCells") }
  @Test def test_moreThanMinCells() { runner.runOneTest("moreThanMinCells") }
  @Test def test_lessThanMinCellsAfterLargeRow() { runner.runOneTest("lessThanMinCellsAfterLargeRow") }
  @Test def test_largeNumRows() { runner.runOneTest("largeNumRows") }
  @Test def test_lessThanMinCellsAfterLargeRow_Neg() { runner.runOneTest("lessThanMinCellsAfterLargeRow_Neg") }
  @Test def test_fixedUnboundedMax() { runner.runOneTest("fixedUnboundedMax") }
  @Test def test_minMaxDoNotMatch() { runner.runOneTest("minMaxDoNotMatch") }

  }
