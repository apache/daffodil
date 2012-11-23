package daffodil.section12.aligned_data

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

class TestAlignedData extends JUnitSuite {
  val testDir_01 = "/daffodil/section12/aligned_data/"
  val tdml1 = testDir_01 + "Aligned_Data.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))

  @Test def test_leadingSkip_1() = {
    runner1.runOneTest("leadingSkip1")
  }
  @Test def test_leadingSkip_2() = { runner1.runOneTest("leadingSkip2") }

}
