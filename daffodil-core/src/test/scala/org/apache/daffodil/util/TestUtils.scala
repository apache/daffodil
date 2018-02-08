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

package edu.illinois.ncsa.daffodil.util

import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import java.io.File
import java.io.FileNotFoundException
import edu.illinois.ncsa.daffodil.Implicits._; object INoWarnU2 { ImplicitsSuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.compiler.Compiler
import java.nio.channels.ReadableByteChannel
import org.junit.Assert.assertEquals
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.grammar.primitives.VariableMapFactory
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.debugger._
import java.nio.channels.Channels
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.infoset.ScalaXMLInfosetOutputter
import edu.illinois.ncsa.daffodil.infoset.ScalaXMLInfosetInputter
import edu.illinois.ncsa.daffodil.infoset.InfosetOutputter
import edu.illinois.ncsa.daffodil.infoset.InfosetInputter

/*
 * This is not a file of tests.
 *
 * These are utilities to support unit testing schemas
 */
object TestUtils {

  /**
   * Compares two XML Elements, after having (optionally) stripped off all attributes.
   *
   * TODO: we might start using xsi:type attributes at some point. If so fix this to
   * save that attribute.
   *
   * NOTE: Has Side Effects: strips off attributes
   */
  def assertEqualsXMLElements(expected: Node, actual: Node, stripAttributes: Boolean = true) {
    val (exp, act) = if (!stripAttributes) (expected, actual) else (XMLUtils.removeAttributes(expected), XMLUtils.removeAttributes(actual))
    XMLUtils.compareAndReport(Utility.trim(exp), Utility.trim(act))
  }

  /**
   * We want to be able to run tests from Eclipse or from batch builds that
   * are rooted in a different directory, so, since Java/JVMs don't have a notion
   * of setting the current directory to a specific value for interpreting things,
   * we have to do that ourselves manually like this.
   *
   * When you specify a file for use in a test, you want to specify it
   * relative to the root of the sub-project of which it is part. I.e., within core,
   * the file you specify should be relative to daffodil/sub-projects/core.
   *
   * Returns null if the file cannot be found.
   */
  def findFile(fn: String): File = findFile(new File(fn))
  def findFile(f: File): File = {
    if (f.exists()) return f
    val cwd = new File("").getAbsolutePath
    throw new FileNotFoundException("Couldn't find file " + f + " relative to " + cwd + ".")
  }

  def testString(testSchema: Node, data: String, isTracing: Boolean = false) = {
    runSchemaOnData(testSchema, Misc.stringToReadableByteChannel(data), isTracing)
  }

  def testBinary(testSchema: Node, hexData: String, areTracing: Boolean = false) = {
    val b = Misc.hex2Bytes(hexData)
    val rbc = Misc.byteArrayToReadableByteChannel(b)
    runSchemaOnData(testSchema, rbc, areTracing)
  }

  def testFile(testSchema: Node, fileName: String) = {
    runSchemaOnData(testSchema, Misc.fileToReadableByteChannel(new java.io.File(fileName)))
  }

  val useSerializedProcessor = true

  def testUnparsing(testSchema: scala.xml.Elem, infosetXML: Node, unparseTo: String, areTracing: Boolean = false): Seq[Diagnostic] = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = saveAndReload(pf.onPath("/").asInstanceOf[DataProcessor])
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    if (areTracing) {
      u.setDebugger(builtInTracer)
      u.setDebugging(true)
    }
    val inputter = new ScalaXMLInfosetInputter(infosetXML)
    val actual = u.unparse(inputter, out)
    if (actual.isProcessingError) {
      val msgs = actual.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val unparsed = outputStream.toString
    //    System.err.println("parsed: " + infoset)
    //    System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparseTo, unparsed)
    actual.getDiagnostics
  }

  def testUnparsingBinary(testSchema: scala.xml.Elem, infoset: Node, unparseTo: Array[Byte]) {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    val u = pf.onPath("/")
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val inputter = new ScalaXMLInfosetInputter(infoset)
    val actual = u.unparse(inputter, out)
    if (actual.isProcessingError) {
      val msgs = actual.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val unparsed = outputStream.toByteArray()
    //        System.err.println("parsed: " + infoset)
    //        System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparsed.length, unparseTo.length)
    for (i <- 0 until unparsed.length) {
      assertEquals(unparseTo(i), unparsed(i))
    }
  }

  private lazy val builtInTracer = new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompilers)

  private def saveAndReload(p: DataProcessor): DataProcessor = {
    if (this.useSerializedProcessor) {
      //
      // We want to serialize/deserialize here, to avoid strange debug artifacts
      // like where schema compilation is still happening at runtime (and
      // therefore generating lots of Debug messages to the log)
      //
      val os = new java.io.ByteArrayOutputStream()
      val output = Channels.newChannel(os)
      p.save(output)

      val is = new java.io.ByteArrayInputStream(os.toByteArray)
      val input = Channels.newChannel(is)
      val compiler_ = Compiler()
      compiler_.reload(input).asInstanceOf[DataProcessor]
    } else p
  }

  def runSchemaOnData(testSchema: Node, data: ReadableByteChannel, areTracing: Boolean = false) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    val isError = pf.isError
    val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")

    if (isError) {
      throw new Exception(msgs)
    }
    val p = saveAndReload(pf.onPath("/").asInstanceOf[DataProcessor])
    val pIsError = p.isError
    if (pIsError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val d = data
    if (areTracing) {
      p.setDebugger(builtInTracer)
      p.setDebugging(true)
    }
    p.setValidationMode(ValidationMode.Limited)

    val outputter = new ScalaXMLInfosetOutputter()
    val actual = p.parse(d, outputter)
    if (actual.isProcessingError) {
      val msgs = actual.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    (actual, outputter.getResult)
  }
}
/**
 * We need a schema document and such for unit testing, also our PrimType
 * needs a dummy schema document also so that our invariant, that *everything*
 * has a schema document, schema, and schema set
 * holds true even when we're not building up a "real" schema.
 */

object Fakes {
  def fakeDP = new Fakes().fakeDP
  def fakeElem = new Fakes().fakeElem
  def fakeSD = new Fakes().fakeSD
  def fakeGroupRef = new Fakes().fakeGroupRef
  def fakeChoiceGroupRef = new Fakes().fakeChoiceGroupRef
  def fakeSequenceGroupRef = new Fakes().fakeSequenceGroupRef
  def fakeGroupRefFactory = new Fakes().fakeGroupRefFactory
}

class Fakes private () {
  lazy val sch = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>
    <xs:element name="fake2" type="tns:fakeCT"/>
    <xs:complexType name="fakeCT">
      <xs:sequence>
        <xs:group ref="tns:fakeChoiceGroup"/>
        <xs:element ref="tns:fake"/>
        <xs:group ref="tns:fakeSequenceGroup"/>
      </xs:sequence>
    </xs:complexType>
    <xs:group name="fakeChoiceGroup">
      <xs:choice>
        <xs:sequence/>
      </xs:choice>
    </xs:group>
    <xs:group name="fakeSequenceGroup">
      <xs:sequence>
        <xs:sequence/>
      </xs:sequence>
    </xs:group>)
  val DummyPrimitiveFactory = null
  val tunable = DaffodilTunables()
  lazy val xsd_sset: SchemaSet = new SchemaSet(sch, "http://example.com", "fake")
  lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
  lazy val fakeSD = xsd_schema.schemaDocuments(0)
  lazy val fakeElem = fakeSD.getGlobalElementDecl("fake").get.forRoot()
  lazy val fakeCT = fakeSD.getGlobalElementDecl("fake2").get.forRoot().typeDef.asInstanceOf[GlobalComplexTypeDef]
  lazy val fakeSequence = fakeCT.sequence
  lazy val Seq(fs1, fs2, fs3) = fakeSequence.groupMembers
  lazy val fakeChoiceGroupRef = fs1.asInstanceOf[ChoiceGroupRef]
  lazy val fakeGroupRef = fakeChoiceGroupRef
  lazy val fakeSequenceGroupRef = fs3.asInstanceOf[SequenceGroupRef]
  lazy val fakeGroupRefFactory = new GroupRefFactory(fs1.xml, fs1, 1, false)

  class FakeDataProcessor extends DFDL.DataProcessor {
    protected var tunablesObj = DaffodilTunables()
    def setValidationMode(mode: ValidationMode.Type): Unit = {}
    def getValidationMode(): ValidationMode.Type = { ValidationMode.Full }
    def save(output: DFDL.Output): Unit = {}
    def setExternalVariables(extVars: Map[String, String]): Unit = {}
    def setExternalVariables(extVars: Seq[Binding]): Unit = {}
    def setExternalVariables(extVars: File): Unit = {}
    def setExternalVariables(extVars: File, tunable: DaffodilTunables): Unit = {}
    def getVariables(): VariableMap = VariableMapFactory.create(Nil)
    def parse(input: DFDL.Input, output: InfosetOutputter, lengthLimitInBits: Long = -1): DFDL.ParseResult = null
    def parse(file: File, output: InfosetOutputter): DFDL.ParseResult = null
    def unparse(inputter: InfosetInputter, output: DFDL.Output): DFDL.UnparseResult = null
    def getDiagnostics: Seq[Diagnostic] = Seq.empty
    def isError: Boolean = false
    def setTunables(tunables: DaffodilTunables): Unit = {}
    def setTunable(tunable: String, value: String): Unit = {}
    def setTunables(tunables: Map[String, String]): Unit = {}
    def getTunables(): DaffodilTunables = { tunablesObj }

  }
  lazy val fakeDP = new FakeDataProcessor

}
