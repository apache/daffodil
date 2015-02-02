/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.example

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.IOException
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import java.util.List

import org.jdom2.output.Format
import org.junit.Test

import edu.illinois.ncsa.daffodil.sapi.Daffodil
import edu.illinois.ncsa.daffodil.sapi.DataProcessor
import edu.illinois.ncsa.daffodil.sapi.Diagnostic
import edu.illinois.ncsa.daffodil.sapi.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.sapi.ParseResult
import edu.illinois.ncsa.daffodil.sapi.ProcessorFactory
import edu.illinois.ncsa.daffodil.sapi.logger.ConsoleLogWriter
import edu.illinois.ncsa.daffodil.sapi.logger.LogLevel

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
    Daffodil.setDebugger(debugger)
    Daffodil.setDebugging(true)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc, 2 << 3)
    val err = res.isError()
    if (!err) {
      val doc = res.result()
      //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
      //xo.setFormat(Format.getPrettyFormat())
      //xo.output(doc, System.out)
    }
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())

    lw.errors.foreach(println)
    lw.warnings.foreach(println)

    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)

    assertTrue(lw.others.size > 0)
    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
        .contains("----------------------------------------------------------------- 1\n"))
    assertTrue(debugger.getCommand().equals("trace"))

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
    Daffodil.setDebugger(null)
    Daffodil.setDebugging(false)

  }

  // This is a duplicate of test testJavaAPI1 that serializes the parser
  // before executing the test.
  @Test
  def testJavaAPI1_A() {
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)
    Daffodil.setDebugger(debugger)
    Daffodil.setDebugging(true)

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
    val input = Channels.newChannel(is)
    val compiler = Daffodil.compiler()
    val parser = compiler.reload(input)

    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = parser.parse(rbc, 2 << 3)
    val err = res.isError()
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
      //xo.setFormat(Format.getPrettyFormat())
      //xo.output(doc, System.out)
    }
    val diags = res.getDiagnostics
    diags.foreach { d => 
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())

    lw.errors.foreach(println)
    lw.warnings.foreach(println)
    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)
    assertTrue(lw.others.size > 0)
    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
        .contains("----------------------------------------------------------------- 1\n"))
    assertTrue(debugger.getCommand().equals("trace"))

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
    Daffodil.setDebugger(null)
    Daffodil.setDebugging(false)
  }

  @Test
  def testJavaAPI2() {
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
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc)
    try {
      val r = res.result()
      fail("did not throw")
    } catch {
      case e: Throwable => assertTrue(e.getMessage().contains("no result"))
    }
    assertTrue(res.isError())
    val diags = res.getDiagnostics
    assertEquals(1, diags.size)
    val d = diags(0)
    System.err.println(d.getMessage())
    assertTrue(d.getMessage().contains("int"))
    assertTrue(d.getMessage().contains("Not an int"))
    assertTrue(d.getDataLocations.toString().contains("10"))
    val locs = d.getLocationsInSchemaFiles
    assertEquals(1, locs.size)
    val loc = locs(0)
    assertTrue(loc.toString().contains("mySchema2.dfdl.xsd"))

    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)
    // assertTrue(lw.infos.size > 0)
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
  def testJavaAPI3() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema3.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    pf.setDistinguishedRootNode("e3", null)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData16.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc, 16 << 3)
    val err = res.isError()
    //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
    //xo.setFormat(Format.getPrettyFormat())
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //xo.output(doc, System.out)
    }
    assertFalse(err)
    assertFalse(res.location().isAtEnd())
    assertEquals(2, res.location().bytePos1b())
    assertEquals(9, res.location().bitPos1b())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())
  }

  // This is a duplicate of test testJavaAPI3 that serializes the parser
  // before executing the test.
  @Test
  def testJavaAPI3_A() {
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
    val input = Channels.newChannel(is)
    val compiler = Daffodil.compiler()
    val parser = compiler.reload(input)

    val file = getResource("/test/sapi/myData16.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = parser.parse(rbc, 16 << 3)
    val err = res.isError()
    //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
    //xo.setFormat(Format.getPrettyFormat())
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //xo.output(doc, System.out)
    }
    assertFalse(err)
    assertFalse(res.location().isAtEnd())
    assertEquals(2, res.location().bytePos1b())
    assertEquals(9, res.location().bitPos1b())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())
  }

  /*
   * @Test def testJavaAPI4() { Compiler c =
   * Daffodil.compiler(); String[] schemaFileNames = new String[1]
   * schemaFileNames[0] = getResource("/test/sapi/mySchema3.dfdl.xsd")
   * ProcessorFactory pf = c.compileFile(schemaFileNames)
   * pf.setDistinguishedRootNode("e4", null); DataProcessor dp =
   * pf.onPath("/"); java.io.File file = new
   * java.io.File(getResource("/test/sapi/myData2.dat"))
   * java.io.FileInputStream fis = new java.io.FileInputStream(file)
   * java.nio.channels.ReadableByteChannel rbc = java.nio.channels.Channels
   * .newChannel(fis); ParseResult res = dp.parse(rbc, 64 << 3); boolean err =
   * res.isError(); org.jdom2.output.XMLOutputter xo = new
   * org.jdom2.output.XMLOutputter(); xo.setFormat(Format.getPrettyFormat())
   * java.util.List<Diagnostic> diags = res.getDiagnostics; for (Diagnostic
   * d : diags) { System.err.println(d.getMessage()); } if (!err) {
   * org.jdom2.Document doc = res.result(); xo.output(doc, System.out); }
   * assertFalse(err); assertFalse(res.location().isAtEnd()); assertEquals(4,
   * res.location().bytePos()); assertEquals(32, res.location().bitPos())
   * System.err.println("bitPos = " + res.location().bitPos())
   * System.err.println("bytePos = " + res.location().bytePos()); }
   */

  @Test
  def testJavaAPI4b() {
    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFileName = getResource("/test/sapi/mySchema3.dfdl.xsd")
    c.setDistinguishedRootNode("e4", null)
    val pf = c.compileFile(schemaFileName)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData2.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc, 64 << 3)
    val err = res.isError()
    //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
    //xo.setFormat(Format.getPrettyFormat())
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //xo.output(doc, System.out)
    }
    assertFalse(err)
    assertFalse(res.location().isAtEnd())
    assertEquals(5, res.location().bytePos1b())
    assertEquals(33, res.location().bitPos1b())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())
  }

  @Test
  def testJavaAPI5() {
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
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc, 4 << 3)
    val err = res.isError()
    //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
    //xo.setFormat(Format.getPrettyFormat())
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //xo.output(doc, System.out)
    }
    assertFalse(err)
    assertTrue("Assertion failed: End of data not reached.", res.location()
        .isAtEnd())
    assertEquals(5, res.location().bytePos1b())
    assertEquals(33, res.location().bitPos1b())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())
  }

  /***
   * Verify that the compiler throws a FileNotFound exception when fed a list
   * of schema files that do not exist.
   * 
   * @throws IOException
   */
  @Test
  def testJavaAPI6() {
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
  def testJavaAPI7() {
    // TODO: This is due to the fact that we are doing several conversions
    // back and forth between Scala.xml.Node and JDOM. And the conversions
    // both use XMLOutputter to format the result (which escapes the
    // entities).
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/TopLevel.xsd")
    c.setDistinguishedRootNode("TopLevel", null)
    val pf = c.compileFile(schemaFile)
    if (pf.isError()) {
      val diags = pf.getDiagnostics
      diags.foreach { d =>
        System.err.println(d.getMessage())
      }
    }
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc)
    val err = res.isError()
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
      // xo.setFormat(Format.getPrettyFormat())
      //xo.setFormat(Format.getRawFormat().setTextMode(
      //    Format.TextMode.PRESERVE))
      //xo.output(doc, System.out)
    }
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())

    // assertEquals(0, lw.errors.size)
    // assertEquals(0, lw.warnings.size)
    // assertTrue(lw.infos.size > 0)
    // assertTrue(lw.others.size > 0)

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  /**
   * This test is nearly identical to testJavaAPI7. The only difference is
   * that this test uses double newline as a terminator for the first element
   * in the sequence rather than double newline as a separator for the
   * sequence
   * 
   * @throws IOException
   */
  @Test
  def testJavaAPI8() {
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/TopLevel.xsd")
    c.setDistinguishedRootNode("TopLevel2", null)
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc)
    val err = res.isError()
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
      // xo.setFormat(Format.getPrettyFormat())
      //xo.setFormat(Format.getRawFormat())
      //xo.output(doc, System.out)
    }
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())

    // assertEquals(0, lw.errors.size)
    // assertEquals(0, lw.warnings.size)
    // assertTrue(lw.infos.size > 0)
    // assertTrue(lw.others.size > 0)

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  /**
   * Verify that calling result() on the ParseResult mutiple times does not
   * error.
   */
  @Test
  def testJavaAPI9() {
    val lw = new LogWriterForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/TopLevel.xsd")
    c.setDistinguishedRootNode("TopLevel2", null)
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/01very_simple.txt")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc)
    val err = res.isError()
    if (!err) {
      //org.jdom2.Document doc = res.result()
      // org.jdom2.Document doc2 = res.result()
      // org.jdom2.Document doc3 = res.result()
      //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
      //xo.setFormat(Format.getRawFormat())
      //xo.output(doc, System.out)
    }
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())

    // reset the global logging state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
  }

  /**
   * Verify that hidden elements do not appear in the resulting infoset
   */
  @Test
  def testJavaAPI10() {

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema4.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData4.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc)
    val err = res.isError()
    if (!err) {
      val node = res.result()
      val hidden = node \\ "hiddenElement"
      assertTrue(hidden.isEmpty)
    }
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())
  }

  /**
   * Verify that nested elements do not appear as duplicates
   */
  @Test
  def testJavaAPI11() {

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val schemaFile = getResource("/test/sapi/mySchema5.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData5.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc)
    val err = res.isError()
    if (!err) {
      val rootNode = res.result()
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
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())
  }

  @Test
  def testJavaAPI12() {
    val lw2 = new LogWriterForSAPITest2()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw2)
    Daffodil.setLoggingLevel(LogLevel.Debug)
    Daffodil.setDebugger(debugger)
    Daffodil.setDebugging(true)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)

    val schemaFile = getResource("/test/sapi/mySchema1.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc, 2 << 3)
    val err = res.isError()
    if (!err) {
      //org.jdom2.Document doc = res.result()
      //org.jdom2.output.XMLOutputter xo = new org.jdom2.output.XMLOutputter()
      //xo.setFormat(Format.getPrettyFormat())
      //xo.output(doc, System.out)
    }
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())

    lw2.errors.foreach(println)
    lw2.warnings.foreach(println)
    assertEquals(0, lw2.errors.size)
    assertEquals(0, lw2.warnings.size)
    // assertTrue(lw2.infos.size > 0)
    assertTrue(lw2.others.size > 0)
    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
        .contains("----------------------------------------------------------------- 1\n"))

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
    Daffodil.setDebugger(null)
    Daffodil.setDebugging(false)
  }

  @Test
  def testJavaAPI13() {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via Compiler.
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)
    Daffodil.setDebugger(debugger)
    Daffodil.setDebugging(true)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val extVarsFile = getResource("/test/sapi/external_vars_1.xml")
    val schemaFile = getResource("/test/sapi/mySchemaWithVars.dfdl.xsd")
    c.setExternalDFDLVariables(extVarsFile)
    val pf = c.compileFile(schemaFile)

    val dp = pf.onPath("/")

    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc, 2 << 3)
    val err = res.isError()
    if (!err) {
      val node = res.result()
      val var1Node = node \ "var1Value"
      assertTrue(var1Node.size == 1)
      val var1NodeValue = var1Node.text
      assertTrue(var1NodeValue == "externallySet")
    }

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
    Daffodil.setDebugger(null)
    Daffodil.setDebugging(false)
  }

  @Test
  def testJavaAPI14() {
    // Demonstrates here that we can set external variables
    // after compilation but before parsing via DataProcessor.
    val lw = new LogWriterForSAPITest()
    val debugger = new DebuggerRunnerForSAPITest()

    Daffodil.setLogWriter(lw)
    Daffodil.setLoggingLevel(LogLevel.Debug)
    Daffodil.setDebugger(debugger)
    Daffodil.setDebugging(true)

    val c = Daffodil.compiler()
    c.setValidateDFDLSchemas(false)
    val extVarFile = getResource("/test/sapi/external_vars_1.xml")
    val schemaFile = getResource("/test/sapi/mySchemaWithVars.dfdl.xsd")
    val pf = c.compileFile(schemaFile)
    val dp = pf.onPath("/")
    dp.setExternalVariables(extVarFile)

    val file = getResource("/test/sapi/myData.dat")
    val fis = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(fis)
    val res = dp.parse(rbc, 2 << 3)
    val err = res.isError()
    if (!err) {
      val rootNode = res.result()
      val var1ValueNode = rootNode \ "var1Value"
      assertTrue(var1ValueNode.size == 1)
      val var1ValueText = var1ValueNode.text
      assertTrue(var1ValueText == "externallySet")
    }
    val diags = res.getDiagnostics
    diags.foreach { d =>
      System.err.println(d.getMessage())
    }
    assertTrue(res.location().isAtEnd())
    System.err.println("bitPos = " + res.location().bitPos1b())
    System.err.println("bytePos = " + res.location().bytePos1b())

    lw.errors.foreach(println)
    lw.warnings.foreach(println)
    assertEquals(0, lw.errors.size)
    assertEquals(0, lw.warnings.size)
    // assertTrue(lw.infos.size > 0)
    assertTrue(lw.others.size > 0)
    assertTrue(debugger.lines.size > 0)
    assertTrue(debugger.lines
        .contains("----------------------------------------------------------------- 1\n"))

    // reset the global logging and debugger state
    Daffodil.setLogWriter(new ConsoleLogWriter())
    Daffodil.setLoggingLevel(LogLevel.Info)
    Daffodil.setDebugger(null)
    Daffodil.setDebugging(false)
  }
}
