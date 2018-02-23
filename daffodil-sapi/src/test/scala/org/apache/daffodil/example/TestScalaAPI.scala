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

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.nio.channels.Channels
import org.junit.Test
import org.apache.daffodil.sapi.Daffodil
import org.apache.daffodil.sapi.ParseResult
import org.apache.daffodil.sapi.logger.ConsoleLogWriter
import org.apache.daffodil.sapi.logger.LogLevel
import org.apache.daffodil.sapi.ValidationMode
import org.apache.daffodil.sapi.InvalidUsageException
import org.apache.daffodil.sapi.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.sapi.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.sapi.io.InputSourceDataInputStream

class TestScalaAPI {

  def getResource(resPath: String): File = {
    val f = try {
      new File(this.getClass().getResource(resPath).toURI())
    } catch {
      case _: Throwable => null
    }
    f
  }

  @Test
  def testScalaAPI1() {
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setDebugger(debugger)
    dp.setDebugging(true)
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertTrue(res.location().isAtEnd())
    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)

    assertTrue(lw.others.size > 0)
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

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)

  }

  // This is a duplicate of test testScalaAPI1 that serializes the parser
  // before executing the test.
  @Test
  def testScalaAPI1_A() {
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
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
    parser.setDebugger(debugger)
    parser.setDebugging(true)
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = parser.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertTrue(res.location().isAtEnd())

    lw.errors.foreach(println)
    lw.warnings.foreach(println)
    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)
    assertTrue(lw.others.size > 0)
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

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)

  }

  @Test
  def testScalaAPI2() {
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Info)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myDataBroken.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
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

    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)
    assertEquals(0, lw.others.size)

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  /**
   * Verify that we can detect when the parse did not consume all the data.
   *
   * @throws IOException
   */
  @Test
  def testScalaAPI3() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema3.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    pf.setDistinguishedRootNode("e3", null)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData16.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertFalse(res.location().isAtEnd())
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
  def testScalaAPI3_A() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema3.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    pf.setDistinguishedRootNode("e3", null)
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
    assertFalse(res.location().isAtEnd())
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
  def testScalaAPI4b() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFileName = getResource("/test/sapi/mySchema3.dfdl.xsd")
    c.setDistinguishedRootNode("e4", null)
    val pf = c.compileFile(schemaFileName)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData2.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertFalse(res.location().isAtEnd())
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
  def testScalaAPI5() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFileName = getResource("/test/sapi/mySchema3.dfdl.xsd")
    c.setDistinguishedRootNode("e4", null); // e4 is a 4-byte long string
    // element
    val pf = c.compileFile(schemaFileName)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData3.dat"); // contains 5
    // bytes
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertTrue(!res.location().isAtEnd())
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
  def testScalaAPI6() {
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = new java.io.File("/test/sapi/notHere1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    assertTrue(pf.isError())
    val diags = pf.getDiagnostics
    var found1 = false
    diags.foreach { d =>
      if (d.getMessage().contains("notHere1")) {
        found1 = true
      }
    }
    assertTrue(found1)

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  /**
   * Tests a user submitted case where the XML appears to be serializing odd
   * xml entities into the output.
   *
   * @throws IOException
   */
  @Test
  def testScalaAPI7() {
    // TODO: This is due to the fact that we are doing several conversions
    // back and forth between Scala.xml.Node and JDOM. And the conversions
    // both use XMLOutputter to format the result (which escapes the
    // entities).
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/TopLevel.dfdl.xsd")
    c.setDistinguishedRootNode("TopLevel", null)
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertTrue(res.location().isAtEnd())

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError()
    assertFalse(err2)
    assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"))

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
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
  def testScalaAPI8() {
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/TopLevel.dfdl.xsd")
    c.setDistinguishedRootNode("TopLevel2", null)
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertTrue(res.location().isAtEnd())

    val bos = new java.io.ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bos)
    val inputter = new ScalaXMLInfosetInputter(outputter.getResult)
    val res2 = dp.unparse(inputter, wbc)
    val err2 = res2.isError();
    assertFalse(err2);
    assertTrue(bos.toString().contains("Return-Path: <bob@smith.com>"))

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  /**
   * Verify that calling result() on the ParseResult mutiple times does not
   * error.
   */
  @Test
  def testScalaAPI9() {
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/TopLevel.dfdl.xsd")
    c.setDistinguishedRootNode("TopLevel2", null)
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
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

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  /**
   * Verify that hidden elements do not appear in the resulting infoset
   */
  @Test
  def testScalaAPI10() {

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema4.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
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
    assertTrue(res.location().isAtEnd())
  }

  /**
   * Verify that nested elements do not appear as duplicates
   */
  @Test
  def testScalaAPI11() {

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema5.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
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
    assertTrue(res.location().isAtEnd())
  }

  @Test
  def testScalaAPI12() {
    val lw2 = new LogWriterForSAPITest2()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw2)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setDebugger(debugger)
    dp.setDebugging(true)
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    val err = res.isError()
    assertFalse(err)
    assertTrue(res.location().isAtEnd())

    lw2.errors.foreach(println)
    lw2.warnings.foreach(println)
    assertEquals(0, lw2.errors.size)
    assertEquals(0, lw2.warnings.size)
    assertTrue(lw2.others.size > 0)
    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
      .contains("----------------------------------------------------------------- 1\n"))

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)

  }

  @Test
  def testScalaAPI13() {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via Compiler.
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val extVarsFile = getResource("/test/sapi/external_vars_1.xml")
    val schemaFile = getResource("/test/sapi/mySchemaWithVars.dfdl.xsd")
    c.setExternalDFDLVariables(extVarsFile)
    val pf = c.compileFile(schemaFile)

    val dp = pf.onPath("/")
    dp.setDebugger(debugger)
    dp.setDebugging(true)
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

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)

  }

  @Test
  def testScalaAPI14() {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via DataProcessor.
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val extVarFile = getResource("/test/sapi/external_vars_1.xml")
    val schemaFile = getResource("/test/sapi/mySchemaWithVars.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setDebugger(debugger)
    dp.setDebugging(true)
    dp.setExternalVariables(extVarFile)

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
    assertTrue(res.location().isAtEnd())

    lw.errors.foreach(println)
    lw.warnings.foreach(println)
    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)
    assertTrue(lw.others.size > 0)
    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
      .contains("----------------------------------------------------------------- 1\n"))

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)

  }

  // This is a duplicate of test testScalaAPI1 that serializes the parser
  // before executing the test.
  // Demonstrates that setting validation to Full for a saved parser fails.
  //
  @Test
  def testScalaAPI1_A_FullFails() {
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setDebugger(debugger)
    dp.setDebugging(true)
    // Serialize the parser to memory, then deserialize for parsing.
    val os = new ByteArrayOutputStream()
    val output = Channels.newChannel(os)
    dp.save(output)

    val is = new ByteArrayInputStream(os.toByteArray())
    val input = Channels.newChannel(is)
    val compiler = Daffodil.compiler()
    val parser = compiler.reload(input)

    try {
      parser.setValidationMode(ValidationMode.Full)
      fail()
    } catch { case e: InvalidUsageException => assertEquals("'Full' validation not allowed when using a restored parser.", e.getMessage()) }

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)

  }

  @Test
  def testScalaAPI15() {
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setDebugger(debugger)
    dp.setDebugging(true)
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
    assertTrue(d.getMessage().contains("e1"))

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  @Test
  def testScalaAPI16() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setValidationMode(ValidationMode.Limited)
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    assertTrue(res.isError())
    assertFalse(res.isProcessingError())
    assertTrue(res.isValidationError())
    assertTrue(res.location().isAtEnd())

    val diags = res.getDiagnostics
    assertEquals(1, diags.size)
    val d = diags(0)
    assertTrue(d.getMessage().contains("maxInclusive"))
    assertTrue(d.getMessage().contains("e2"))
    assertTrue(d.getMessage().contains("20"))
  }

  @Test
  def testScalaAPI17() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setValidationMode(ValidationMode.Full)
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)
    val outputter = new ScalaXMLInfosetOutputter()
    val res = dp.parse(input, outputter)
    assertTrue(res.isError())
    assertFalse(res.isProcessingError())
    assertTrue(res.isValidationError())
    assertTrue(res.location().isAtEnd())

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
  def testScalaAPI18() {
    // Demonstrate that we can use the API to continue a parse where we left off
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema3.dfdl.xsd")
    c.setDistinguishedRootNode("e4", null)
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData2.dat")
    val fis = new java.io.FileInputStream(file)
    val input = new InputSourceDataInputStream(fis)

    val outputter = new ScalaXMLInfosetOutputter()
    var res: ParseResult = null
    var err: Boolean = false

    res = dp.parse(input, outputter)
    err = res.isError()
    assertFalse(err)
    assertFalse(res.location().isAtEnd())
    assertEquals(5, res.location().bytePos1b())
    assertEquals("data", outputter.getResult.text)

    outputter.reset()
    res = dp.parse(input, outputter)
    err = res.isError()
    assertFalse(err)
    assertFalse(res.location().isAtEnd())
    assertEquals(9, res.location().bytePos1b())
    assertEquals("left", outputter.getResult.text)

    outputter.reset()
    res = dp.parse(input, outputter)
    err = res.isError()
    assertFalse(err)
    assertTrue(res.location().isAtEnd())
    assertEquals(13, res.location().bytePos1b())
    assertEquals("over", outputter.getResult.text)
  }

}
