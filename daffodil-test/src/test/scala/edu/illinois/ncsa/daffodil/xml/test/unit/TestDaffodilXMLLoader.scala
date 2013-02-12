package edu.illinois.ncsa.daffodil.xml.test.unit

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */


import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test
import java.io.File
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.util.LoggingDefaults
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Misc

/**
 * This unit test cannot be in the regular daffodil-core library due
 * to needing to populate a file into a directory that is on the
 * classpath.
 *
 * In daffodil-test there is no problem with us creating such a
 * directory dynamically, and having it be on the classpath of the
 * project. At least under Eclipse anyway.
 *
 * TODO: is the above a security hole? If so, is there a better way
 * to test this? Do we have to make a whole separate project just
 * to test this?
 */
class TestDaffodilXMLLoader extends JUnitSuite {

  /**
   * This test makes sure that a CatalogManager.properties file
   * will be read and catalogs it specifies will be loaded and used.
   *
   * It tests this by validating some data that is invalid with respect
   * to a schema supplied by way of the CatalogManager.properties file.
   *
   * If the error is detected, then it must have found and used the
   * user-specified catalog.
   */
  @Test def testThisTestRig() {

    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)

    val test = new CatalogTestBase {

      override val testCatalog = <catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
                                   <uri name="foobar" uri={ tmpSchemaFileName.getAbsolutePath }/>
                                 </catalog>

      // The '5' for line should cause a validation error because it doesn't match
      // the 'A' enum in the schema.
      val testData =
        <onlyAnAWillDo xmlns="foobar">5</onlyAnAWillDo>

      val testSchema =
        <schema xmlns={ xsd } targetNamespace="foobar" xmlns:xsd={ xsd }>
          <!-- in our internal schema line is an int, in this schema it is a string.
             So if our internal schema is being used, a validation against that will cause an error
             for anything that doesn't obey the syntax of an int. So we want to demonstrate
             that we get that error.
          -->
          <xsd:element name="onlyAnAWillDo">
            <xsd:simpleType>
              <xsd:restriction base="xsd:string">
                <xsd:enumeration value="A"/>
              </xsd:restriction>
            </xsd:simpleType>
          </xsd:element>
        </schema>

      override def runTest(): Boolean = {
        if (!super.runTest()) {
          System.err.println("test skipped.")
          return false
        }
        // but when it has to resolve our dafint namespace, we don't want it to take the 
        // one we created here, rather we want it to take our internal one.
        val excs = exceptionList

        println(excs)
        //
        // Now, our internal dafint schema requires the line attribute to be a 
        // xs:int, so using 'A' above means we should get an error on the line attribute.
        //
        // these checks pass if we got an error complaining about the 'line'
        //
        assertTrue(excs.length >= 1)
        // check that we got an error message complaining about the element.
        val hasError = excs.exists { _.getMessage().contains("onlyAnAWillDo") }
        assertTrue(hasError)
        true
      }
    }

    test.runTest
  }

  /**
   * This test insures that a user catalog doesn't get priority over
   * our internal catalog by defining a catalog and schema that uses
   * one of our namespaces.
   *
   * Our dafint.xsd defines testElement and the line and col attributes
   * must be type xs:int. The data here has dafint:line with value 'A',
   * so if the built-in schema is used, it will find this error. So the
   * test passes if the error is detected.
   */
  @Test def testThatUserCatalogCannotOverrideDaffodilInternalCatalog1() {

    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)

    val test = new CatalogTestBase {

      // The 'A' for line should cause a validation error if it is using our internal schema
      // We want an error, becasue we want it using our internal catalog, not the user supplied one.
      val testData =
        <dafint:testElement xmlns={ ns } xmlns:dafint={ ns } dafint:line="A" dafint:col="6">5</dafint:testElement>

      val testSchema =
        <schema xmlns={ xsd } targetNamespace={ ns } xmlns:dafint={ ns } xmlns:xsd={ xsd }>
          <!-- in our internal schema line is an int, in this schema it is a string.
             So if our internal schema is being used, a validation against that will cause an error
             for anything that doesn't obey the syntax of an int. So we want to demonstrate
             that we get that error.
          -->
          <attribute name="line" type="xsd:string"/>
          <attribute name="col" type="xsd:string"/>
          <element name="testElement">
            <complexType>
              <simpleContent>
                <extension base="xsd:integer">
                  <attribute ref="dafint:line"/>
                  <attribute ref="dafint:col"/>
                </extension>
              </simpleContent>
            </complexType>
          </element>
        </schema>

      override def runTest(): Boolean = {
        if (!super.runTest()) {
          System.err.println("test skipped.")
          return false
        }
        // but when it has to resolve our dafint namespace, we don't want it to take the 
        // one we created here, rather we want it to take our internal one.
        val excs = exceptionList

        println(excs)
        //
        // Now, our internal dafint schema requires the line attribute to be a 
        // xs:int, so using 'A' above means we should get an error on the line attribute.
        //
        // these checks pass if we got an error complaining about the 'line'
        //
        assertTrue(excs.length >= 1)
        val hasLineError = excs.exists { _.getMessage().contains("line") }
        assertTrue(hasLineError)
        true
      }
    }

    test.runTest
  }

  /**
   * This test is similar to the above, but makes sure that our internal
   * dafint.xsd is being used because the data would be invalid and cause
   * invalid error if the user-supplied schema was being used.
   *
   * Test passes if there is no invalid data error.
   */
  @Test def testThatUserCatalogCannotOverrideDaffodilInternalCatalog2() {

    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)

    val test = new CatalogTestBase {
      val testData =
        // The '5' for line NOT cause a validation error because it is an INT
        // however, this is a weak test because 5 is also a perfectly good string.
        // so we override the schema too below to use an enumeration to insist on 
        // the string being 'A' not compatible with an int.
        <e xmlns={ ns } xmlns:dafint={ ns } dafint:line="5" dafint:col="6">5</e>

      // So if this schema is being used, the load will cause an error.
      // We don't want that error.
      val testSchema =
        <schema xmlns={ xsd } targetNamespace={ ns } xmlns:dafint={ ns } xmlns:xsd={ xsd }>
          <!-- in our internal schema this is an int, in this schema it is a string 
             that is NOT an int enforced by the enumeration facet.
             So if our internal schema is being used, a validation against that will be ok.
             But if THIS schema is being used, a validation against our data will fail.
          -->
          <attribute name="line">
            <simpleType>
              <restriction base="xsd:string">
                <enumeration value="A"/>
              </restriction>
            </simpleType>
          </attribute>
          <element name="e">
            <complexType>
              <simpleContent>
                <extension base="xsd:integer">
                  <attribute ref="dafint:line"/>
                </extension>
              </simpleContent>
            </complexType>
          </element>
        </schema>

      override def runTest(): Boolean = {
        if (!super.runTest()) {
          System.err.println("test skipped.")
          return false
        }
        // but when it has to resolve our dafint namespace, we don't want it to take the 
        // one we created here, rather we want it to take our internal one.
        val excs = exceptionList

        println(excs)
        //
        // Now, our internal dafint schema requires the line attribute to be a 
        // xs:int, so using '5' above will be ok. 
        //
        // these checks pass only if we did NOT get an error.
        assertTrue(excs.length == 0)
        true
      }
    }

    test.runTest
  }

}

abstract class CatalogTestBase {

  val cd = new File(".")
  val cdpath = cd.getAbsolutePath()
  System.err.println("cd = " + cdpath)
  val testCPDirPath =
    if (cdpath.endsWith("daffodil-test/.")) "testData_OnClassPath"
    else if (cdpath.endsWith("daffodil/.")) "daffodil-test/testData_OnClassPath"
    else {
      System.err.println("Can't run catalog tests because we are not in a directory where they are runnable.")
      ""
    }
  val testCPDir = new File(testCPDirPath)
  val tmpSchemaFileName = new File(testCPDir.getAbsolutePath() + "/sch.xsd")
  val tmpDataFileName = new File(testCPDir.getAbsolutePath() + "/data.xml")
  val tmpCatalogFileName = new File(testCPDir.getAbsolutePath() + "/testCatalog.xml")
  val tmpCatalogManagerPropFile = new File(testCPDir.getAbsolutePath() + "/CatalogManager.properties")

  // Take the # off of the verbosity line if you want more output
  // to see what's happening with catalog/uri resolution
  val CMPropsContent =
    """#verbosity=4
relative-catalogs=true
catalogs=testData_OnClassPath/testCatalog.xml
"""

  // This is one of our internal namespaces. Let's make a schema that uses it
  // and see whose gets loaded, this one, or our internal one.

  val ns = XMLUtils.DAFFODIL_INTERNAL_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val ex = XMLUtils.EXAMPLE_NAMESPACE

  def testSchema: scala.xml.Node

  def testData: scala.xml.Node

  val testCatalog = <catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
                      <uri name={ ns } uri={ tmpSchemaFileName.getAbsolutePath }/>
                    </catalog>

  var exceptionList: List[Exception] = Nil

  /**
   *  returns false if the test can't be executed because we can't create the file
   *  and it should be bypassed without failing.
   */
  def runTest(): Boolean = {

    if (testCPDirPath == "") return false
    // user will have their own catalogs, we need to still use ours
    // even if they have the same URI/URNs as namespaces in their
    // catalogs.

    // classpath will have the subdirectory of this project named
    // test_OnClasspath on the classpath.
    // Create it if it doesn't exist

    if (testCPDir.exists() && testCPDir.isDirectory() & testCPDir.canWrite()) {
      // we're good to go
    } else {
      try {
        System.err.println("Creating test directory " + testCPDir.getAbsolutePath())
        testCPDir.mkdir()
        testCPDir.deleteOnExit()
      } catch {
        case e: SecurityException => {
          System.err.println("Required test directory does not exist, and cannot be created.")
          return false
        }
      }
    }

    tmpSchemaFileName.deleteOnExit()
    tmpDataFileName.deleteOnExit()
    tmpCatalogFileName.deleteOnExit()
    tmpCatalogManagerPropFile.deleteOnExit()

    using(new java.io.FileWriter(tmpSchemaFileName)) {
      fw =>
        fw.write(testSchema.toString())
    }
    using(new java.io.FileWriter(tmpCatalogManagerPropFile)) {
      fw =>
        fw.write(CMPropsContent)
    }
    using(new java.io.FileWriter(tmpCatalogFileName)) {
      fw =>
        fw.write(testCatalog.toString())
    }
    using(new java.io.FileWriter(tmpDataFileName)) {
      fw =>
        fw.write(testData.toString())
    }

    // At this point, these files should be on the classpath.
    // Let's make sure by trying to retrieve one as a resource.
    Misc.getResourceOption("/CatalogManager.properties") match {
      case (Some(res), path) => // ok. It exists.
      case (None, path) => {
        System.err.println("Unable to get required resource: " + path + " from the classpath.")
        return false
      }
    }

    val loader = new DaffodilXMLLoader(new org.xml.sax.ErrorHandler {

      def warning(exception: SAXParseException) = {
        exceptionList = exception :: exceptionList
        System.err.println("Warning " + exception.getMessage())
      }

      def error(exception: SAXParseException) = {
        exceptionList = exception :: exceptionList
        System.err.println("Error: " + exception.getMessage())
      }
      def fatalError(exception: SAXParseException) = {
        exceptionList = exception :: exceptionList
        System.err.println("Fatal: " + exception.getMessage())
      }
    })

    val elem = loader.loadFile(tmpDataFileName) // that should validate it.
    // println(elem)
    true // returned to indicate that things worked
  }
}
