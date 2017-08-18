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

  // DFDL-1832
  //
  // You get an error like:
  // Schema Definition Error: Error loading schema due to org.xml.sax.SAXParseException;
  // systemId: file:/home/.../test-stdLayout/target/eclipse/classes/org2/xsd/org2/xsd/payload.dfdl.xsd; lineNumber: 33; columnNumber: 38; src-resolve: Cannot resolve the name 'tns:Data' to a(n) 'element declaration' component.
  //
  // If you look at that file path, notice the org2/xsd/org2/xsd is doubled up. I don't know how exactly this happens, but it happens in Xerces code.
  //
  // Back when our projects just had src/main/resources/xsd, these paths would come out with "xsd/xsd" doubled up, and we put in a hack to look for that and remove it.
  //
  // But now that we're using package-style directory names, it's not so easy to just hack it, as whatever package name you would have, it will get doubled.
  //
  // Note: the hack code is in DaffodilXMLLoader in daffodil-lib
  //

  // This tests if schema files under src/main/resources/org1/xsd we can include/import
  // from org2.
  @Test def test_outer_01() = { runner.runOneTest("outer_01") }

  // DFDL-1832
  // Error as with test_outer_01, though
  // this test is checking if DFDL schema files in the src/test/resources
  // directories can be found and reference schema files in
  // the src/main/resources directories.
  @Test def test_schemaFilesUnderSrcTest_01() = { runner2.runOneTest("test_schemaFilesUnderSrcTest_01") }

}
