package org2

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestOrg2 {
  val runner = Runner("org2", "testSchemaFilesUnderSrcTest2.tdml")
  val runner2 = Runner("org2", "testEmbeddedSchema.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner2.reset
  }
}

class TestOrg2 {

  import TestOrg2._

  @Test def test_schemaFilesUnderSrcTest_02() = { runner.runOneTest("test_schemaFilesUnderSrcTest_02") }
  @Test def test_embeddedSchema_01() = { runner2.runOneTest("test_embeddedSchema_01") }

}
