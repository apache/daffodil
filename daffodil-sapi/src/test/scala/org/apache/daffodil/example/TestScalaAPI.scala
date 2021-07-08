/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.example

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.file.Paths
import javax.xml.XMLConstants

import org.apache.commons.io.FileUtils

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

import org.xml.sax.XMLReader

import org.apache.daffodil.exceptions.Abort
import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.DaffodilParseXMLReader
import org.apache.daffodil.sapi.DaffodilUnhandledSAXException
import org.apache.daffodil.sapi.DaffodilUnparseErrorSAXException
import org.apache.daffodil.sapi.DataProcessor
import org.apache.daffodil.sapi.ExternalVariableException
import org.apache.daffodil.sapi.InvalidUsageException
import org.apache.daffodil.sapi.ParseResult
import org.apache.daffodil.sapi.SAXErrorHandlerForSAPITest
import org.apache.daffodil.sapi.ValidationMode
import org.apache.daffodil.sapi.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.sapi.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.sapi.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream


object TestScalaAPI {
  /**
   * Best practices for XML loading are to turn off anything that could lead to
   * insecurity.
   *
   * This is probably unnecessary in the case of these tests, but as these tests
   * are also used to illustrate API usage, this exemplifies best practice.
   */
  def setSecureDefaults(xmlReader: XMLReader): Unit = {
    xmlReader.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    // since we're not really sure what they mean by secure processing
    // we make doubly sure by setting these ourselves also.
    xmlReader.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    xmlReader.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    xmlReader.setFeature("http://xml.org/sax/features/external-general-entities", false)
  }
}

class TestScalaAPI {

  import TestScalaAPI._

  lazy val SAX_NAMESPACES_FEATURE = "http://xml.org/sax/features/namespaces"
  lazy val SAX_NAMESPACE_PREFIXES_FEATURE = "http://xml.org/sax/features/namespace-prefixes"

  private def getResource(resPath: String): File = {
    val f = try {
      new File(this.getClass().getResource(resPath).toURI())
    } catch {
      case _: Throwable => null
    }
    f
  }

  /**
   * This is a test-only helper function used to serialize and deserialize a
   * DataProcessor to ensure all SAPI classes that need to extend
   * Serializable do so appropriately.
   *
   * All of the SAPI tests create a DataProcessor. To test that we correctly
   * made all the necessary changes to make the SAPI DataProcessor
   * serializable, it is important to serialize and deserialize that
   * DataProcessor before use in the tests. This function acts as a helper
   * function to accomplish that task.
   *
   * So this functions accepts a DataProcessor, serializes and deserializes
   * that DataProcessor in memory, and then returns the result. The test
   * should then use that resulting DataProcessor for the rest of the test.
   * This function is only used for testing purposes.
   *
   * Note that this function contains an ObjectInputStream for
   * deserialization, but one that is extended to override the resolveClass
   * function. This override is necessary to work around a bug when running
   * tests in SBT that causes an incorrect class loader to be used. Normal
   * users of the Scala API should not need this and can serialize/deserialize
   * as one would normally do with a standard Object{Input,Output}Stream.
   */
  private def reserializeDataProcessor(dp: DataProcessor): DataProcessor = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(dp)
    oos.close()

    val bais = new ByteArrayInputStream(baos.toByteArray())
    val ois = new ObjectInputStream(bais) {
      /**
       * This override is here because of a bug in sbt where the wrong class loader is being
       * used when deserializing an object.
       * For more information, see https://github.com/sbt/sbt/issues/163
       */
      override protected def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
        try {
          Class.forName(desc.getName, false, getClass.getClassLoader)
        } catch {
          case e: ClassNotFoundException => super.resolveClass(desc);
        }
      }
    }

    ois.readObject().asInstanceOf[DataProcessor]
  }

  @Test
  def testScalaAPI1(): Unit = {
    val debugger = new DebuggerRunnerForSAPITest()

    val c = Daffodil.compiler()
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
      .withDebuggerRunner(debugger)
      .withDebugging(true)
      .withValidationMode(ValidationMode.Off)

    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)

    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
      .contains("----------------------------------------------------------------- 1\n"))
    assertTrue(debugger.getCommand().equals("trace"))

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertEquals("42", bos.toString());
  }

  // This is a duplicate of test testScalaAPI1 that serializes the parser
  // before executing the test.
  @Test
  def testScalaAPI1_A(): Unit = {
    val debugger = new DebuggerRunnerForSAPITest()

    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")

    // Serialize the parser to memory, then deserialize for parsing.
    val os = new ByteArrayOutputStream()
    val output = Channels.newChannel(os)
    dp.save(output)

    val is = new ByteArrayInputStream(os.toByteArray())
    val savedParser = Channels.newChannel(is)
    val compiler = Daffodil.compiler()
    val parser = compiler.reload(savedParser)
    .withDebuggerRunner(debugger)
    .withDebugging(true)
    .withValidationMode(ValidationMode.Off)
    val file = getResource("/test/sapi/myData.dat")
    // This test uses a byte array here, just so as to be sure to exercise
    // the constructor for creating an InputSourceDataInputStream from a byte array
    // and byte buffer.
    val ba = FileUtils.readFileToByteArray(file)
    val bb = ByteBuffer.wrap(ba)
    val input = new InputSourceDataInputStream(bb)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = parser.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)

    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
      .contains("----------------------------------------------------------------- 1\n"))
    assertTrue(debugger.getCommand().equals("trace"))

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertEquals("42", bos.toString());
  }

  @Test
  def testScalaAPI2(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myDataBroken.dat")
    // This test uses a byte array here, just so as to be sure to exercise
    // the constructor for creating an InputSourceDataInputStream from a byte array
    // and byte buffer.
    val ba = FileUtils.readFileToByteArray(file)
    val input = new InputSourceDataInputStream(ba)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)

    // TODO: Need scala API for status enum
    //assertFalse(outputter.getStatus == Status.Done)
    assertTrue(res.isError())
    val diags = res.getDiagnostics
    assertEquals(1, diags.size)
    val d = diags(0)
    assertTrue(d.getMessage().contains("int"))
    assertTrue(d.getMessage().contains("Not an int"))
    assertTrue(d.getDataLocations.toString().contains("10"))
    val locs = d.getLocationsInSchemaFiles
    assertEquals(1, locs.size)
    val loc = locs(0)
    assertTrue(loc.toString().contains("mySchema1.dfdl.xsd")) // reports the element ref, not element decl.
  }

  /**
   * Verify that we can detect when the parse did not consume all the data.
   *
   * @throws IOException
   */
  @Test
  def testScalaAPI3(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema3.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
      .withDistinguishedRootNode("e3", null)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myData16.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertEquals(2, res.location().bytePos1b())
    assertEquals(9, res.location().bitPos1b())

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertEquals("9", bos.toString());
  }

  // This is a duplicate of test testScalaAPI3 that serializes the parser
  // before executing the test.
  @Test
  def testScalaAPI3_A(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema3.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
      .withDistinguishedRootNode("e3", null)
    val dp = pf.onPath("/")

    // Serialize the parser to memory, then deserialize for parsing.
    val os = new ByteArrayOutputStream()
    val output = Channels.newChannel(os)
    dp.save(output)

    val is = new ByteArrayInputStream(os.toByteArray())
    val savedParser = Channels.newChannel(is)
    val compiler = Daffodil.compiler()
    val parser = compiler.reload(savedParser)

    val file = getResource("/test/sapi/myData16.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = parser.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertEquals(2, res.location().bytePos1b())
    assertEquals(9, res.location().bitPos1b())

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertEquals("9", bos.toString());
  }

  @Test
  def testScalaAPI4b(): Unit = {
    val c = Daffodil.compiler()

    val schemaFileName = getResource("/test/sapi/mySchema3.dfdl.xsd")
    val pf = c.compileFile(schemaFileName, Some("e4"), None)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myData2.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertEquals(5, res.location().bytePos1b())
    assertEquals(33, res.location().bitPos1b())

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertEquals("data", bos.toString());
  }

  @Test
  def testScalaAPI5(): Unit = {
    val c = Daffodil.compiler()

    val schemaFileName = getResource("/test/sapi/mySchema3.dfdl.xsd")
    // element
    val pf = c.compileFile(schemaFileName, Some("e4"), None) // e4 is a 4-byte long string
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myData3.dat"); // contains 5
    // bytes
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertEquals(5, res.location().bytePos1b())
    assertEquals(33, res.location().bitPos1b())

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertEquals("data", bos.toString());
  }

  /**
   * *
   * Verify that the compiler throws a FileNotFound exception when fed a list
   * of schema files that do not exist.
   *
   * @throws IOException
   */
  @Test
  def testScalaAPI6(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = new java.io.File("/test/sapi/notHere1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    assertTrue(pf.isError())
    val diags = pf.getDiagnostics
    val found1 = diags.exists{ _.getMessage().contains("notHere1") }

    assertTrue(found1)
  }

  /**
   * Tests a user submitted case where the XML appears to be serializing odd
   * xml entities into the output.
   *
   * @throws IOException
   */
  @Test
  def testScalaAPI7(): Unit = {
    // TODO: This is due to the fact that we are doing several conversions
    // back and forth between Scala.xml.Node and JDOM. And the conversions
    // both use XMLOutputter to format the result (which escapes the
    // entities).
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/TopLevel.dfdl.xsd")
    val pf = c.compileFile(schemaFile, Some("TopLevel"), None)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError()
    assertFalse(err2)
    assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"))
  }

  /**
   * This test is nearly identical to testScalaAPI7. The only difference is
   * that this test uses double newline as a terminator for the first element
   * in the sequence rather than double newline as a separator for the
   * sequence
   *
   * @throws IOException
   */
  @Test
  def testScalaAPI8(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/TopLevel.dfdl.xsd")
    val pf = c.compileFile(schemaFile, Some("TopLevel2"), None)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"))
  }

  /**
   * Verify that calling result() on the ParseResult mutiple times does not
   * error.
   */
  @Test
  def testScalaAPI9(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/TopLevel.dfdl.xsd")
    val pf = c.compileFile(schemaFile, Some("TopLevel2"), None)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)

    val node1 = outputter.getResult

    val bos1 = new java.io.ByteArrayOutputStream()
    val wbc1 = java.nio.channels.Channels.newChannel(bos1)
    val inputter1 = new ScalaXMLInfosetInputter(node1)
    val res2 = dp.unparse(inputter1, wbc1)
    val err2 = res2.isError();
    assertFalse(err2);
    assertTrue(bos1.toString().contains("Return-Path: <bob@smith.com>"))

    val node2 = outputter.getResult

    val bos2 = new java.io.ByteArrayOutputStream()
    val wbc2 = java.nio.channels.Channels.newChannel(bos2)
    val inputter2 = new ScalaXMLInfosetInputter(node2)
    val res3 = dp.unparse(inputter2, wbc2)
    val err3 = res3.isError();
    assertFalse(err3);
    assertTrue(bos2.toString().contains("Return-Path: <bob@smith.com>"))
  }

  /**
   * Verify that hidden elements do not appear in the resulting infoset
   */
  @Test
  def testScalaAPI10(): Unit = {

    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema4.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myData4.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    val node = outputter.getResult
    val hidden = node \\ "hiddenElement"
    assertTrue(hidden.isEmpty)
  }

  /**
   * Verify that nested elements do not appear as duplicates
   */
  @Test
  def testScalaAPI11(): Unit = {

    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema5.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myData5.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    val rootNode = outputter.getResult
    val elementGroup = rootNode \ "elementGroup"
    assertTrue(!elementGroup.isEmpty)
    val groupE2 = elementGroup \ "e2"
    assertTrue(!groupE2.isEmpty)
    val groupE3 = elementGroup \ "e3"
    assertTrue(!groupE3.isEmpty)
    val rootE2 = rootNode \ "e2"
    assertTrue(rootE2.isEmpty)
    val rootE3 = rootNode \ "e3"
    assertTrue(rootE3.isEmpty)
  }

  @Test
  def testScalaAPI12(): Unit = {
    val debugger = new DebuggerRunnerForSAPITest()

    val c = Daffodil.compiler()


    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
      .withDebuggerRunner(debugger)
      .withDebugging(true)
      .withValidationMode(ValidationMode.Off)

    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)

    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
      .contains("----------------------------------------------------------------- 1\n"))
  }

  @Test
  def testScalaAPI13(): Unit = {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via Compiler.
    val debugger = new DebuggerRunnerForSAPITest()
    val c = Daffodil.compiler()

    val extVarsFile = getResource("/test/sapi/external_vars_1.xml")
    val schemaFile = getResource("/test/sapi/mySchemaWithVars.dfdl.xsd")
    val pf = c.compileFile(schemaFile)

    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
      .withExternalVariables(extVarsFile)
      .withDebuggerRunner(debugger)
      .withDebugging(true)
      .withValidationMode(ValidationMode.Off)

    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    val node = outputter.getResult
    val var1Node = node \ "var1Value"
    assertTrue(var1Node.size == 1)
    val var1NodeValue = var1Node.text
    assertTrue(var1NodeValue == "externallySet")
  }

  @Test
  def testScalaAPI14(): Unit = {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via DataProcessor.
    val debugger = new DebuggerRunnerForSAPITest()

    val c = Daffodil.compiler()

    val extVarFile = getResource("/test/sapi/external_vars_1.xml")
    val schemaFile = getResource("/test/sapi/mySchemaWithVars.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
      .withExternalVariables(extVarFile)
      .withDebuggerRunner(debugger)
      .withDebugging(true)
      .withValidationMode(ValidationMode.Off)

    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    val rootNode = outputter.getResult
    val var1ValueNode = rootNode \ "var1Value"
    assertTrue(var1ValueNode.size == 1)
    val var1ValueText = var1ValueNode.text
    assertTrue(var1ValueText == "externallySet")

    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
      .contains("----------------------------------------------------------------- 1\n"))
  }

  // This is a duplicate of test testScalaAPI1 that serializes the parser
  // before executing the test.
  // Demonstrates that setting validation to Full for a saved parser fails.
  //
  @Test
  def testScalaAPI1_A_FullFails(): Unit = {
    val debugger = new DebuggerRunnerForSAPITest()

    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
      .withDebuggerRunner(debugger)
      .withDebugging(true)
    // Serialize the parser to memory, then deserialize for parsing.
    val os = new ByteArrayOutputStream()
    val output = Channels.newChannel(os)
    dp.save(output)

    val is = new ByteArrayInputStream(os.toByteArray())
    val input = Channels.newChannel(is)
    val compiler = Daffodil.compiler()
    val parser = compiler.reload(input)

    try {
      parser.withValidationMode(ValidationMode.Full)
      fail()
    } catch { case e: InvalidUsageException => assertEquals("'Full' validation not allowed when using a restored parser.", e.getMessage()) }
  }

  @Test
  def testScalaAPI15(): Unit = {
    val debugger = new DebuggerRunnerForSAPITest()

    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
      .withDebuggerRunner(debugger)
      .withDebugging(true)

    val file = getResource("/test/sapi/myInfosetBroken.xml")
    val xml = scala.xml.XML.loadFile(file)
    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)

    val inputter = new ScalaXMLInfosetInputter(xml)
    val res = dp.unparse(inputter, wbc)
    val err = res.isError()
    assertTrue(err)

    val diags = res.getDiagnostics
    assertEquals(1, diags.size)
    val d = diags(0);
    assertTrue(d.getMessage().contains("wrong"))
    assertTrue(d.getMessage().contains("e2"))
  }

  @Test
  def testScalaAPI16(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
      .withValidationMode(ValidationMode.Limited)
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    assertTrue(res.isError())
    assertFalse(res.isProcessingError())
    assertTrue(res.isValidationError())

    val diags = res.getDiagnostics
    assertEquals(1, diags.size)
    val d = diags(0)
    assertTrue(d.getMessage().contains("maxInclusive"))
    assertTrue(d.getMessage().contains("e2"))
    assertTrue(d.getMessage().contains("20"))
  }

  @Test
  def testScalaAPI17(): Unit = {
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
      .withValidationMode(ValidationMode.Full)
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    assertTrue(res.isError())
    assertFalse(res.isProcessingError())
    assertTrue(res.isValidationError())
    val actualLength = res.location.bytePos1b - 1
    assertEquals(file.length, actualLength)

    val diags = res.getDiagnostics
    assertEquals(3, diags.size)
    val d0 = diags(0)
    val d1 = diags(1)
    val d2 = diags(2)
    assertTrue(d0.getMessage().contains("42"))
    assertTrue(d0.getMessage().contains("e2"))
    assertTrue(d0.getMessage().contains("not valid"))

    assertTrue(d1.getMessage().contains("42"))
    assertTrue(d1.getMessage().contains("maxInclusive"))
    assertTrue(d1.getMessage().contains("20"))

    assertTrue(d2.getMessage().contains("maxInclusive"))
    assertTrue(d2.getMessage().contains("e2"))
    assertTrue(d2.getMessage().contains("20"))
  }

  @Test
  def testScalaAPI18(): Unit = {
    // Demonstrate that we can use the API to continue a parse where we left off
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema3.dfdl.xsd")
    val pf = c.compileFile(schemaFile, Some("e4"), None)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myData2.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)

    val outputter = new ScalaXMLInfosetOutputter()
    var res: ParseResult = null
    var err: Boolean = false

    res = dp.parse(input, outputter)
    err = res.isError()
    assertFalse(err)
    assertEquals(5, res.location().bytePos1b())
    assertEquals("data", outputter.getResult.text)

    outputter.reset()
    res = dp.parse(input, outputter)
    err = res.isError()
    assertFalse(err)
    assertEquals(9, res.location().bytePos1b())
    assertEquals("left", outputter.getResult.text)

    outputter.reset()
    res = dp.parse(input, outputter)
    err = res.isError()
    assertFalse(err)
    assertFalse(input.hasData())
    assertEquals(13, res.location().bytePos1b())
    assertEquals("over", outputter.getResult.text)
  }

  @Test
  def testScalaAPI19(): Unit = {
    // Demonstrate that we cannot use the API to continue a parse with an invalid InputSource
    // ie. after a runtime SDE. This test needs to be run with an input file larger than 256MB
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/ambig_elt.dfdl.xsd")
    val pf = c.compileFile(schemaFile, Some("root"), None)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)

    val file = getResource("/test/sapi/myData19.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)

    val outputter = new ScalaXMLInfosetOutputter()
    var res: ParseResult = null
    var err: Boolean = false

    // First attempt at parsing should fail due to attempting to backtrack too far
    res = dp.parse(input, outputter)
    err = res.isError()
    assertTrue(err)

    outputter.reset()
    try {
      res = dp.parse(input, outputter)
    } catch {
      case e: Abort => {
        assertTrue(e.getMessage().contains("Usage error"))
        assertTrue(e.getMessage().contains("invalid input source"))
      }
    }
  }

  @Test
  def testScalaAPI20(): Unit = {
    // Test SAX parsing/unparsing
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
    val parseXMLReader = dp.newXMLReaderInstance()

    val file = getResource("/test/sapi/myData.dat")
    val fisDP = new java.io.FileInputStream(file)
    val fisSAX = new java.io.FileInputStream(file)
    val inputSAX = new InputSourceDataInputStream(fisSAX)
    val inputDP = new InputSourceDataInputStream(fisDP)
    val bosDP = new ByteArrayOutputStream()
    val outputter = new XMLTextInfosetOutputter(bosDP, pretty = true)
    dp.parse(inputDP, outputter)
    val infosetDPString = bosDP.toString()

    val outputContentHandler = new org.jdom2.input.sax.SAXHandler()
    val errorHandler = new SAXErrorHandlerForSAPITest()
    // since SAXHandler uses a blank prefix when the below isn't set to true, it introduces
    // a the no-prefixed xmlns mapping
    parseXMLReader.setFeature(SAX_NAMESPACE_PREFIXES_FEATURE, true)
    parseXMLReader.setContentHandler(outputContentHandler)
    parseXMLReader.setErrorHandler(errorHandler)
    parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBDIRECTORY,
      Paths.get(System.getProperty("java.io.tmpdir")))
    parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBPREFIX, "daffodil-sapi-")
    parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBSUFFIX, ".sax.blob")
    parseXMLReader.parse(inputSAX)
    val resSAX = parseXMLReader.getProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    val err = errorHandler.isError
    val diags = errorHandler.getDiagnostics
    val infosetSAX = outputContentHandler.getDocument
    val pretty = org.jdom2.output.Format.getPrettyFormat.setLineSeparator(System.getProperty("line.separator"))
    val infosetSAXString = new org.jdom2.output.XMLOutputter(pretty).outputString(infosetSAX)

    assertFalse(err)
    assertTrue(diags.isEmpty)
    assertEquals(infosetDPString, infosetSAXString)

    // test unparse
    val unparseBos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(unparseBos)

    // prep for SAX unparse
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    val unparseXMLReader = javax.xml.parsers.SAXParserFactory.newInstance.newSAXParser.getXMLReader
    setSecureDefaults(unparseXMLReader)
    unparseXMLReader.setContentHandler(unparseContentHandler)
    unparseXMLReader.setErrorHandler(errorHandler)
    unparseXMLReader.setFeature(SAX_NAMESPACES_FEATURE, true)
    unparseXMLReader.setFeature(SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val is = new ByteArrayInputStream(infosetSAXString.getBytes)
    // kickstart unparse
    unparseXMLReader.parse(new org.xml.sax.InputSource(is))

    val saxUr = unparseContentHandler.getUnparseResult
    wbc.close()
    
    val saxErr = saxUr.isError()
    assertFalse(saxErr)
    assertTrue(saxUr.getDiagnostics.isEmpty)
    assertEquals("42", unparseBos.toString())
  }

  @Test
  def testScalaAPI21(): Unit = {
    /** Test parse with errors */
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
    val parseXMLReader = dp.newXMLReaderInstance()

    val file = getResource("/test/sapi/myDataBroken.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val contentHandler = new org.jdom2.input.sax.SAXHandler()
    val errorHandler = new SAXErrorHandlerForSAPITest()
    parseXMLReader.setContentHandler(contentHandler)
    parseXMLReader.setErrorHandler(errorHandler)
    parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBDIRECTORY,
      Paths.get(System.getProperty("java.io.tmpdir")))
    parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBPREFIX, "daffodil-sapi-")
    parseXMLReader.setProperty(DaffodilParseXMLReader.DAFFODIL_SAX_URN_BLOBSUFFIX, ".sax.blob")
    parseXMLReader.parse(input)

    assertTrue(errorHandler.isError)
    val diags = errorHandler.getDiagnostics
    assertEquals(1, diags.size)
    val d = diags.head
    assertTrue(d.getMessage().contains("int"))
    assertTrue(d.getMessage().contains("Not an int"))
    assertTrue(d.getDataLocations.toString().contains("10"))
    val locs = d.getLocationsInSchemaFiles
    assertEquals(1, locs.size)
    val loc = locs.head
    assertTrue(loc.toString().contains("mySchema1.dfdl.xsd")) // reports the element ref, not element decl.
  }

  @Test
  def testScalaAPI22(): Unit = {
    // Test SAX unparse with errors
    val debugger = new DebuggerRunnerForSAPITest()

    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp1 = pf.onPath("/")
    val dp = reserializeDataProcessor(dp1)
      .withDebuggerRunner(debugger)
      .withDebugging(true)

    val file = getResource("/test/sapi/myInfosetBroken.xml")
    val xml = scala.xml.XML.loadFile(file)
    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    // prep for SAX
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    val errorHandler = new SAXErrorHandlerForSAPITest()
    val unparseXMLReader = javax.xml.parsers.SAXParserFactory.newInstance.newSAXParser.getXMLReader
    setSecureDefaults(unparseXMLReader)
    unparseXMLReader.setContentHandler(unparseContentHandler)
    unparseXMLReader.setErrorHandler(errorHandler)
    unparseXMLReader.setFeature(SAX_NAMESPACES_FEATURE, true)
    unparseXMLReader.setFeature(SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val is = new ByteArrayInputStream(xml.toString().getBytes)
    // kickstart unparse
    try {
      unparseXMLReader.parse(new org.xml.sax.InputSource(is))
    } catch {
      case _: DaffodilUnparseErrorSAXException => // do nothing; handled below
      case _: DaffodilUnhandledSAXException => // do nothing; we don't expect this in this test
    }

    val res = unparseContentHandler.getUnparseResult
    val err = res.isError()
    assertTrue(err)

    val diags = res.getDiagnostics
    assertEquals(1, diags.size)
    val d = diags.head
    assertTrue(d.getMessage().contains("wrong"))
    assertTrue(d.getMessage().contains("e2"))
  }

  @Test
  def testScalaAPI24(): Unit = {
    // Demonstrates error cases of setting external variables
    val c = Daffodil.compiler()

    val schemaFile = getResource("/test/sapi/mySchemaWithComplexVars1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    var dp = pf.onPath("/")
    dp = reserializeDataProcessor(dp)

    // set var without a namespace, ambiguity error because schema contains
    // two variables with same name but different namespace
    try {
      dp = dp.withExternalVariables(Map("var" -> "10"))
    } catch {
      case e: ExternalVariableException => {
        val msg = e.getMessage()
        assertTrue(msg.contains("var"))
        assertTrue(msg.contains("ambiguity"))
        assertTrue(msg.contains("ex1:var"))
        assertTrue(msg.contains("ex2:var"))
      }
    }

    // variable without namespace does not exist error
    try {
      dp = dp.withExternalVariables(Map("dne" -> "10"))
    } catch {
      case e: ExternalVariableException => {
        val msg = e.getMessage()
        assertTrue(msg.contains("definition not found"))
        assertTrue(msg.contains("dne"))
      }
    }

    // variable with namespace does not exist error
    try {
      dp = dp.withExternalVariables(Map("{http://example.com/1}dne" -> "10"))
    } catch {
      case e: ExternalVariableException => {
        val msg = e.getMessage()
        assertTrue(msg.contains("definition not found"))
        assertTrue(msg.contains("{http://example.com/1}dne"))
      }
    }

    // variable cannot be set externally
    try {
      dp = dp.withExternalVariables(Map("{http://example.com/2}var" -> "10"))
    } catch {
      case e: ExternalVariableException => {
        val msg = e.getMessage()
        assertTrue(msg.contains("ex2:var"))
        assertTrue(msg.contains("cannot be set externally"))
      }
    }

    // variable not valid with regards to type
    try {
      dp = dp.withExternalVariables(Map("{http://example.com/1}var" -> "notAnInt"))
    } catch {
      case e: ExternalVariableException => {
        val msg = e.getMessage()
        assertTrue(msg.contains("ex1:var"))
        assertTrue(msg.contains("is not a valid xs:int"))
        assertTrue(msg.contains("notAnInt"))
      }
    }

    // can change the value of the same variable multiple times
    dp = dp.withExternalVariables(Map("{http://example.com/1}var" -> "100"))
    dp = dp.withExternalVariables(Map("{http://example.com/1}var" -> "200"))

    // can parse with the variable values
    {
        val ba = Array[Byte]()
        val bb = ByteBuffer.wrap(ba)
        val dis = new InputSourceDataInputStream(bb)
        val outputter = new ScalaXMLInfosetOutputter()
        val res = dp.parse(dis, outputter)
        assertFalse(res.isError())
        val docString = outputter.getResult().toString()
        assertTrue(docString.contains("<ex1var>200</ex1var>"))
    }

    // can set an external variable after a parse
    dp = dp.withExternalVariables(Map("{http://example.com/1}var" -> "300"))

    // can parse with the updated variable value
    {
        val ba = Array[Byte]()
        val bb = ByteBuffer.wrap(ba)
        val dis = new InputSourceDataInputStream(bb)
        val outputter = new ScalaXMLInfosetOutputter()
        val res = dp.parse(dis, outputter)
        assertFalse(res.isError())
        val docString = outputter.getResult().toString()
        assertTrue(docString.contains("<ex1var>300</ex1var>"))
    }
  }

}
