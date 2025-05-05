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

package org.apache.daffodil.core.util

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.net.URI
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.xml._

import org.apache.daffodil.api
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.core.dsom._
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.MultiException
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.iapi._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml._
import org.apache.daffodil.runtime1.debugger._
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.VariableMap

object INoWarnU2 { ImplicitsSuppressUnusedImportWarning() }

/*
 * This is not a file of tests.
 *
 * These are utilities to support unit testing schemas
 */
object TestUtils {

  def assertEquals[T](expected: T, actual: T) =
    if (expected != actual) throw new AssertionError("assertEquals failed.")

  def testString(testSchema: Node, data: String, areTracing: Boolean = false) = {
    runSchemaOnRBC(testSchema, Misc.stringToReadableByteChannel(data), areTracing)
  }

  def testBinary(
    testSchema: Node,
    hexData: String,
    areTracing: Boolean = false
  ): (api.ParseResult, Node) = {
    val b = Misc.hex2Bytes(hexData)
    testBinary(testSchema, b, areTracing)
  }

  def testBinary(
    testSchema: Node,
    data: Array[Byte],
    areTracing: Boolean
  ): (api.ParseResult, Node) = {
    val rbc = Misc.byteArrayToReadableByteChannel(data)
    runSchemaOnRBC(testSchema, rbc, areTracing)
  }

  private val useSerializedProcessor = true

  def testUnparsing(
    testSchema: scala.xml.Elem,
    infosetXML: Node,
    unparseTo: String,
    areTracing: Boolean = false
  ): Seq[api.Diagnostic] = {
    val compiler = Compiler().withTunable("allowExternalPathExpressions", "true")
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) throwDiagnostics(pf.getDiagnostics)
    var u = saveAndReload(pf.onPath("/").asInstanceOf[DataProcessor])
    if (u.isError) throwDiagnostics(u.getDiagnostics)
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    u = if (areTracing) {
      u.withDebugger(builtInTracer).withDebugging(true)
    } else u
    val inputter = new ScalaXMLInfosetInputter(infosetXML)
    val actual = u.unparse(inputter, out)
    if (actual.isProcessingError) throwDiagnostics(actual.getDiagnostics)
    val unparsed = outputStream.toString
    //    System.err.println("parsed: " + infoset)
    //    System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparseTo, unparsed)
    actual.getDiagnostics
  }

  private def throwDiagnostics(ds: Seq[api.Diagnostic]): Nothing = {
    new MultiException(ds).toss
  }

  def testUnparsingBinary(
    testSchema: scala.xml.Elem,
    infoset: Node,
    unparseTo: Array[Byte],
    areTracing: Boolean = false
  ): Unit = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) throwDiagnostics(pf.diagnostics)
    var u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) throwDiagnostics(u.getDiagnostics)
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val inputter = new ScalaXMLInfosetInputter(infoset)
    u = if (areTracing) {
      u.withDebugger(builtInTracer).withDebugging(true)
    } else u
    val actual = u.unparse(inputter, out)
    if (actual.isProcessingError) throwDiagnostics(actual.getDiagnostics)
    val unparsed = outputStream.toByteArray()
    out.close()
    assertEquals(unparsed.length, unparseTo.length)
    for (i <- 0 until unparsed.length) {
      assertEquals(unparseTo(i), unparsed(i))
    }
  }

  private lazy val builtInTracer =
    new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompilers)

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

  def compileSchema(testSchema: Node) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) throwDiagnostics(pf.getDiagnostics)
    val p = saveAndReload(pf.onPath("/").asInstanceOf[DataProcessor])
    if (p.isError) throwDiagnostics(p.getDiagnostics)
    p
  }

  private def runSchemaOnRBC(
    testSchema: Node,
    data: ReadableByteChannel,
    areTracing: Boolean = false
  ): (api.ParseResult, Node) = {
    runSchemaOnInputStream(testSchema, Channels.newInputStream(data), areTracing)
  }

  private def runSchemaOnInputStream(
    testSchema: Node,
    is: InputStream,
    areTracing: Boolean = false
  ): (api.ParseResult, Node) = {
    val p = compileSchema(testSchema)
    runDataProcessorOnInputStream(p, is, areTracing)
  }

  def runDataProcessorOnInputStream(
    dp: DataProcessor,
    is: InputStream,
    areTracing: Boolean = false
  ): (api.ParseResult, Node) = {
    val p1 =
      if (areTracing) {
        dp.withDebugger(builtInTracer).withDebugging(true)
      } else dp

    val p = p1.withValidator(api.validation.ValidatorsFactory.getLimitedValidator)

    val outputter = new ScalaXMLInfosetOutputter()
    val input = InputSourceDataInputStream(is)
    val actual = p.parse(input, outputter)
    if (actual.isProcessingError) throwDiagnostics(actual.getDiagnostics)
    (actual, outputter.getResult())
  }

  private val defaultIncludeImports =
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
  private val defaultTopLevels =
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited" encoding="US-ASCII"/>

  /**
   * For convenient unit testing of schema compiler attributes defined on Term types.
   */
  def getRoot(
    contentElements: Seq[Node],
    elementFormDefault: String = "unqualified",
    includeImports: Seq[Node] = defaultIncludeImports,
    topLevels: Seq[Node] = defaultTopLevels
  ): Root = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      includeImports,
      topLevels,
      contentElements,
      elementFormDefault = elementFormDefault
    )
    val sset = SchemaSet(testSchema)
    sset.root
  }

  private def compileAndSave(
    compiler: Compiler,
    schemaSource: URISchemaSource,
    output: WritableByteChannel
  ): Try[(api.compiler.ProcessorFactory, api.DataProcessor)] = {
    Try {
      val pf = compiler.compileSource(schemaSource)
      if (pf.isError) throwDiagnostics(pf.getDiagnostics)
      val dp = pf.onPath("/")
      dp.save(output)
      if (dp.isError)
        throwDiagnostics(dp.getDiagnostics ++ pf.getDiagnostics)
      (pf, dp)
    }
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
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
    </xs:group>
  )
  val DummyPrimitiveFactory = null
  val tunables = DaffodilTunables()
  lazy val xsd_sset: SchemaSet = SchemaSet(sch, "http://example.com", "fake")
  lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
  lazy val fakeSD = xsd_schema.schemaDocuments(0)
  lazy val fakeElem = fakeSD.getGlobalElementDecl("fake").get

  lazy val fakeCT =
    fakeSD.getGlobalElementDecl("fake2").get.typeDef.asInstanceOf[GlobalComplexTypeDef]
  lazy val fakeSequence = fakeCT.sequence
  lazy val Seq(fs1, fs2, fs3) = fakeSequence.groupMembers
  lazy val fakeChoiceGroupRef = fs1.asInstanceOf[ChoiceGroupRef]
  lazy val fakeGroupRef = fakeChoiceGroupRef
  lazy val fakeSequenceGroupRef = fs3.asInstanceOf[SequenceGroupRef]
  lazy val fakeGroupRefFactory = GroupRefFactory(fs1.xml, fs1, 1, false)

  private class FakeDataProcessor extends DFDL.DataProcessor {
    override def save(output: DFDL.Output): Unit = {}
    override def parse(
      input: api.InputSourceDataInputStream,
      output: api.infoset.InfosetOutputter
    ): DFDL.ParseResult = null
    override def unparse(
      inputter: api.infoset.InfosetInputter,
      output: DFDL.Output
    ): DFDL.UnparseResult =
      null
    override def getDiagnostics: java.util.List[api.Diagnostic] = Seq.empty
    override def isError: Boolean = false

    override def tunables: DaffodilTunables = DaffodilTunables()
    override def variableMap: VariableMap = VariableMap(Nil)
    override def walkMetadata(handler: api.MetadataHandler): Unit = {}

    override def withExternalVariables(extVars: Seq[Binding]): DFDL.DataProcessor = this
    override def withExternalVariables(extVars: java.io.File): DFDL.DataProcessor = this
    override def withExternalVariables(
      extVars: java.util.Map[String, String]
    ): DFDL.DataProcessor = this
    override def withTunable(tunable: String, value: String): DFDL.DataProcessor = this
    override def withTunables(tunables: Map[String, String]): DFDL.DataProcessor = this
    override def withValidator(validator: api.validation.Validator): DFDL.DataProcessor = this
    override def withDebugger(dbg: api.debugger.Debugger): DFDL.DataProcessor = this
    override def withDebugging(flag: Boolean): DFDL.DataProcessor = this

    override def newXMLReaderInstance: DFDL.DaffodilParseXMLReader = null
    override def newContentHandlerInstance(
      output: DFDL.Output
    ): DFDL.DaffodilUnparseContentHandler = null

    override def getMainSchemaURIForFullValidation: URI = null
  }
  lazy val fakeDP: DFDL.DataProcessor = new FakeDataProcessor

}

/**
 * Testing class for streaming message parse behavior
 */
object StreamParser {
  case class CompileFailure(diags: Seq[api.Diagnostic]) extends MultiException(diags)

  /**
   * Result object for parse calls. Just a tuple.
   */
  case class Result(
    message: Node, // document that is the current parse result, or null
    diags: Seq[api.Diagnostic], // diagnostics.
    isProcessingError: Boolean,
    isValidationError: Boolean,
    bitPos1b: Long
  ) {

    def toXML: Node = {
      <Result>
        {message}
        {
        if (!diags.isEmpty) {
          <diagnostics>
          {diags.map { diag => <diagnostic>{diag.toString} </diagnostic> }}
        </diagnostics>
        } else Null
      }
      </Result> %
        (if (isProcessingError)
           new UnprefixedAttribute("isProcessingError", isProcessingError.toString, Null)
         else Null) %
        (if (isValidationError)
           new UnprefixedAttribute("isValidationError", isValidationError.toString, Null)
         else Null) %
        new UnprefixedAttribute("bitPos1b", bitPos1b.toString, Null)
    }
  }

  def doStreamTest(schema: Node, data: String): LazyList[Result] = {
    val mp = new StreamParser(schema)
    val is: InputStream = new ByteArrayInputStream(data.getBytes("ascii"))
    mp.setInputStream(is)
    var r: StreamParser.Result = null
    val results = new ArrayBuffer[Result]
    val resStream = LazyList.continually(mp.parse).takeWhile(r => !r.isProcessingError)
    resStream
  }
}

class StreamParser private (val schema: Node) {

  private lazy val outputter = new ScalaXMLInfosetOutputter()
  private var dis: InputSourceDataInputStream = _

  //
  // First compile the DFDL Schema
  private lazy val c = Compiler()
  private lazy val pf = c.compileNode(schema)
  private val dp = {
    if (pf.isError) throw new StreamParser.CompileFailure(pf.getDiagnostics)
    val dataproc1 = pf
      .onPath("/")
    val dataproc = dataproc1
      .withValidator(
        api.validation.ValidatorsFactory.getXercesValidator(
          dataproc1.getMainSchemaURIForFullValidation
        )
      )
    // .withDebuggerRunner(new TraceDebuggerRunner()) // DAFFODIL-2624 - cannot trace in streaming SAPI
    // .withDebugging(true)
    if (dataproc.isError)
      throw new StreamParser.CompileFailure(
        dataproc.getDiagnostics
      )
    dataproc
  }

  lazy val compilationWarnings: Seq[api.Diagnostic] =
    pf.getDiagnostics ++: dp.getDiagnostics

  def setInputStream(inputStream: InputStream): Unit = {
    dis = InputSourceDataInputStream(inputStream)
  }

  /**
   * Called to pull messages from the data stream.
   *
   * @return a Result object containing the results of the parse including diagnostic information.
   */
  def parse = {
    if (dis == null)
      throw new IllegalStateException("Input stream must be provided by setInputStream() call.")
    val res: DFDL.ParseResult = dp.parse(dis, outputter).asInstanceOf[DFDL.ParseResult]
    val procErr = res.isProcessingError
    val validationErr = res.isValidationError
    val diags = res.getDiagnostics
    val doc = if (!procErr) outputter.getResult() else null
    val bitPos1b = res.resultState.currentLocation.bitPos1b
    val r = new StreamParser.Result(doc, diags, procErr, validationErr, bitPos1b)
    outputter.reset()
    r
  }
}
