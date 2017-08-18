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

  // DFDL-1832
  // Shows that a DFDL schema file under src/test/resources/org2/xsd is found
  // but it cannot include files from under src/main/resources/org2/xsd.
  @Test def test_schemaFilesUnderSrcTest_02() = { runner.runOneTest("test_schemaFilesUnderSrcTest_02") }

  // DFDL-1832
  // Shows that a TDML file with embedded schema, that embedded schema
  // cannot import a DFDL schema file under src/main/resources/org2/xsd due
  // to the package naming not being resolved properly.
  @Test def test_embeddedSchema_01() = { runner2.runOneTest("test_embeddedSchema_01") }

}
