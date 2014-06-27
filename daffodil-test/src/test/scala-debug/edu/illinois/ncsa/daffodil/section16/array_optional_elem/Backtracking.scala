package edu.illinois.ncsa.daffodil.section16.array_optional_elem

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

class BacktrackingDebug {
  val testDir = "/edu/illinois/ncsa/daffodil/section16/array_optional_elem/"
  val tdml = testDir + "backtracking.tdml"
  lazy val r = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_backtrack1() = { Debugger.withTracing(false); r.runOneTest("backtrack1") }

}
