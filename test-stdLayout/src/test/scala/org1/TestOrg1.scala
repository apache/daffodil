package org1

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestOrg1 {
  val runner = Runner("org1", "testStdLayout.tdml")
  val runner2 = Runner("org1", "testSchemaFilesUnderSrcTest.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}

class TestOrg1 {

  import TestOrg1._

  @Test def test_outer_01() = { runner.runOneTest("outer_01") }
  @Test def test_schemaFilesUnderSrcTest_01() = { runner2.runOneTest("test_schemaFilesUnderSrcTest_01") }

}
