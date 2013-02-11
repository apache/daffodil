package daffodil

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.contrib.java.lang.system.ExpectedSystemExit
import org.junit.contrib.java.lang.system.internal.CheckExitCalled
import org.junit.Test
import org.junit.Rule
import daffodil.util.Misc
import java.io.ByteArrayInputStream
import daffodil.tdml.DFDLTestSuite

class TestMultiFileSuppport extends JUnitSuite {

  val testDir = "/test/multiFile/"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(testDir + "multiFile.tdml"))

  @Test def testSimpleIncludeOfFormat() { runner.runOneTest("simpleInclude") }
  @Test def testSimpleImportOfFormat() { runner.runOneTest("simpleImport") }
  @Test def testIncludeNoNamespace() { runner.runOneTest("includeNoNamespace") }
  @Test def testImportWithOverlappingNSPrefixes() { runner.runOneTest("importWithOverlappingNSPrefixes") }

}