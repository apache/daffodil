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

package org.apache.daffodil.util

import scala.xml._
import org.apache.daffodil.xml.XMLUtils
import java.io.File
import java.io.FileNotFoundException
import org.apache.daffodil.Implicits._; object INoWarnU2 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.compiler.Compiler
import java.nio.channels.ReadableByteChannel
import org.junit.Assert.assertEquals
import org.apache.daffodil.dsom._
import org.apache.daffodil.xml._
import org.apache.daffodil.grammar.primitives.VariableMapFactory
import org.apache.daffodil.api._
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.debugger._
import java.nio.channels.Channels
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.infoset.InfosetInputter
import org.apache.daffodil.io.InputSourceDataInputStream

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

  def testString(testSchema: Node, data: String, areTracing: Boolean = false) = {
    runSchemaOnData(testSchema, Misc.stringToReadableByteChannel(data), areTracing)
  }

  def testBinary(testSchema: Node, hexData: String, areTracing: Boolean = false): (DFDL.ParseResult, Node) = {
    val b = Misc.hex2Bytes(hexData)
    testBinary(testSchema, b, areTracing)
  }

  def testBinary(testSchema: Node, data: Array[Byte], areTracing: Boolean): (DFDL.ParseResult, Node) = {
    val rbc = Misc.byteArrayToReadableByteChannel(data)
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

  def throwDiagnostics(ds: Seq[Diagnostic]) {
    if (ds.length == 1) throw (ds(0))
    else {
      val msgs = ds.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
  }

  def testUnparsingBinary(testSchema: scala.xml.Elem, infoset: Node, unparseTo: Array[Byte], areTracing: Boolean = false) {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) throwDiagnostics(pf.diagnostics)
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) throwDiagnostics(u.getDiagnostics)
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val inputter = new ScalaXMLInfosetInputter(infoset)
    if (areTracing) {
      u.setDebugger(builtInTracer)
      u.setDebugging(true)
    }
    val actual = u.unparse(inputter, out)
    if (actual.isProcessingError) throwDiagnostics(actual.getDiagnostics)
    val unparsed = outputStream.toByteArray()
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
    if (areTracing) {
      p.setDebugger(builtInTracer)
      p.setDebugging(true)
    }
    p.setValidationMode(ValidationMode.Limited)

    val outputter = new ScalaXMLInfosetOutputter()
    val input = InputSourceDataInputStream(Channels.newInputStream(data))
    val actual = p.parse(input, outputter)
    if (actual.isProcessingError) {
      val diags = actual.getDiagnostics
      if (diags.length == 1) throw diags(0)
      val msgs = diags.map(_.getMessage()).mkString("\n")
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
    <dfdl:format ref="tns:GeneralFormat"/>,
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
    def parse(input: InputSourceDataInputStream, output: InfosetOutputter): DFDL.ParseResult = null
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
