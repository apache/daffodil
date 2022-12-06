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

import java.io.File
import java.io.FileNotFoundException
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import scala.util.Try
import org.apache.commons.io.output.NullOutputStream
import org.junit.Assert.assertEquals
import org.apache.daffodil.Implicits._
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api._
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.debugger._
import org.apache.daffodil.dsom._
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.grammar.VariableMapFactory
import org.apache.daffodil.infoset.InfosetInputter
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml._

import java.io.ByteArrayInputStream
import org.apache.daffodil.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.io.InputSourceDataInputStream

import java.io.InputStream
import scala.collection.mutable.ArrayBuffer
import scala.xml._



object INoWarnU2 { ImplicitsSuppressUnusedImportWarning() }

/*
 * This is not a file of tests.
 *
 * These are utilities to support unit testing schemas
 */
object TestUtils {

  /**
   * Compares two XML Elements, after having (optionally) stripped off all attributes.
   */
  def assertEqualsXMLElements(expected: Node, actual: Node): Unit = {
    XMLUtils.compareAndReport(expected, actual)
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
    runSchemaOnRBC(testSchema, Misc.stringToReadableByteChannel(data), areTracing)
  }

  def testBinary(testSchema: Node, hexData: String, areTracing: Boolean = false): (DFDL.ParseResult, Node) = {
    val b = Misc.hex2Bytes(hexData)
    testBinary(testSchema, b, areTracing)
  }

  def testBinary(testSchema: Node, data: Array[Byte], areTracing: Boolean): (DFDL.ParseResult, Node) = {
    val rbc = Misc.byteArrayToReadableByteChannel(data)
    runSchemaOnRBC(testSchema, rbc, areTracing)
  }

  def testFile(testSchema: Node, fileName: String) = {
    runSchemaOnRBC(testSchema, Misc.fileToReadableByteChannel(new java.io.File(fileName)))
  }

  val useSerializedProcessor = true

  def testUnparsing(testSchema: scala.xml.Elem, infosetXML: Node, unparseTo: String, areTracing: Boolean = false): Seq[Diagnostic] = {
    val compiler = Compiler().withTunable("allowExternalPathExpressions", "true")
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    var u = saveAndReload(pf.onPath("/").asInstanceOf[DataProcessor])
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    u = if (areTracing) {
      u.withDebugger(builtInTracer).withDebugging(true)
    } else u
    val inputter = new ScalaXMLInfosetInputter(infosetXML)
    val actual = u.unparse(inputter, out)
    if (actual.isProcessingError) {
      throwDiagnostics(actual.getDiagnostics)
    }
    val unparsed = outputStream.toString
    //    System.err.println("parsed: " + infoset)
    //    System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparseTo, unparsed)
    actual.getDiagnostics
  }

  def throwDiagnostics(ds: Seq[Diagnostic]): Unit = {
    if (ds.length == 1) throw (ds(0))
    else {
      val msgs = ds.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
  }

  def testUnparsingBinary(testSchema: scala.xml.Elem, infoset: Node, unparseTo: Array[Byte], areTracing: Boolean = false): Unit = {
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

  def compileSchema(testSchema: Node) = {
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
    p
  }

  def runSchemaOnRBC(testSchema: Node, data: ReadableByteChannel, areTracing: Boolean = false): (DFDL.ParseResult, Node) = {
    runSchemaOnInputStream(testSchema, Channels.newInputStream(data), areTracing)
  }

  def runSchemaOnInputStream(testSchema: Node, is: InputStream, areTracing: Boolean = false): (DFDL.ParseResult, Node) = {
    val p = compileSchema(testSchema)
    runDataProcessorOnInputStream(p, is, areTracing)
  }

  def runDataProcessorOnInputStream(dp: DataProcessor, is: InputStream, areTracing: Boolean = false): (DFDL.ParseResult, Node) = {
    val p1 =
      if (areTracing) {
        dp.withDebugger(builtInTracer).withDebugging(true)
      } else dp

    val p = p1.withValidationMode(ValidationMode.Limited)

    val outputter = new ScalaXMLInfosetOutputter()
    val input = InputSourceDataInputStream(is)
    val actual = p.parse(input, outputter)
    if (actual.isProcessingError) {
      val diags = actual.getDiagnostics
      if (diags.length == 1) throw diags(0)
      val msgs = diags.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    (actual, outputter.getResult)
  }

  private val defaultIncludeImports =
    <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
  private val defaultTopLevels =
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited" encoding="US-ASCII"/>

  /**
   * For convenient unit testing of schema compiler attributes defined on Term types.
   */
  def getRoot(
    contentElements: Seq[Node],
    elementFormDefault: String = "unqualified",
    includeImports: Seq[Node] = defaultIncludeImports,
    topLevels: Seq[Node] = defaultTopLevels): Root = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      includeImports,
      topLevels,
      contentElements,
      elementFormDefault = elementFormDefault)
    val sset = SchemaSet(testSchema)
    sset.root
  }

  private def compileAndSave(compiler: Compiler, schemaSource: URISchemaSource, output: WritableByteChannel) = {
    Try {
      val pf = compiler.compileSource(schemaSource)
      if (!pf.isError) {
        val dp = pf.onPath("/")
        dp.save(output)
        if (!dp.isError) {
          (pf, dp)
        } else {
          throw new Exception(
            (dp.getDiagnostics ++ pf.getDiagnostics).map { _.getMessage() }.mkString("\n"))
        }
      } else
        throw new Exception(pf.getDiagnostics.map { _.getMessage() }.mkString("\n"))
    }
  }

  def testCompileTime(resourcePathString: String): Unit = {
    val nos = NullOutputStream.NULL_OUTPUT_STREAM
    val nullChannel = java.nio.channels.Channels.newChannel(nos)
    val compiler = Compiler()
    val uri = Misc.getRequiredResource(resourcePathString)
    val schemaSource = URISchemaSource(uri)
    val theTry = Timer.getResult(compileAndSave(compiler, schemaSource, nullChannel))
    theTry.get
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
    <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
  val tunables = DaffodilTunables()
  lazy val xsd_sset: SchemaSet = SchemaSet(sch, "http://example.com", "fake")
  lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
  lazy val fakeSD = xsd_schema.schemaDocuments(0)
  lazy val fakeElem = fakeSD.getGlobalElementDecl("fake").get

  lazy val fakeCT = fakeSD.getGlobalElementDecl("fake2").get.typeDef.asInstanceOf[GlobalComplexTypeDef]
  lazy val fakeSequence = fakeCT.sequence
  lazy val Seq(fs1, fs2, fs3) = fakeSequence.groupMembers
  lazy val fakeChoiceGroupRef = fs1.asInstanceOf[ChoiceGroupRef]
  lazy val fakeGroupRef = fakeChoiceGroupRef
  lazy val fakeSequenceGroupRef = fs3.asInstanceOf[SequenceGroupRef]
  lazy val fakeGroupRefFactory = GroupRefFactory(fs1.xml, fs1, 1, false)

  class FakeDataProcessor extends DFDL.DataProcessor {
    override def save(output: DFDL.Output): Unit = {}
    override def parse(input: InputSourceDataInputStream, output: InfosetOutputter): DFDL.ParseResult = null
    override def unparse(inputter: InfosetInputter, output: DFDL.Output): DFDL.UnparseResult = null
    override def getDiagnostics: Seq[Diagnostic] = Seq.empty
    override def isError: Boolean = false

    override def tunables: DaffodilTunables = DaffodilTunables()
    override def variableMap: VariableMap = VariableMapFactory.create(Nil)
    override def validationMode: ValidationMode.Type = ValidationMode.Full

    override def withExternalVariables(extVars: Seq[Binding]): DFDL.DataProcessor = this
    override def withExternalVariables(extVars: java.io.File): DFDL.DataProcessor = this
    override def withExternalVariables(extVars: Map[String,String]): DFDL.DataProcessor = this
    override def withTunable(tunable: String, value: String): DFDL.DataProcessor = this
    override def withTunables(tunables: Map[String,String]): DFDL.DataProcessor = this
    override def withValidationMode(mode: ValidationMode.Type): DFDL.DataProcessor = this
    override def withDebugger(dbg:AnyRef): DFDL.DataProcessor = this
    override def withDebugging(flag: Boolean): DFDL.DataProcessor = this

    override def newXMLReaderInstance: DFDL.DaffodilParseXMLReader = null
    override def newContentHandlerInstance(output: DFDL.Output): DFDL.DaffodilUnparseContentHandler = null
  }
  lazy val fakeDP = new FakeDataProcessor

}

/**
 * Testing class for streaming message parse behavior
 */
object StreamParser {
  case class CompileFailure (diags: Seq[Diagnostic]) extends Exception("DFDL Schema Compile Failure"){
    override def getMessage() = diags.map{ _.toString }.mkString(",\n")
  }

  /**
   * Result object for parse calls. Just a tuple.
   */
  case class Result (message: Node, // document that is the current parse result, or null
    diags: Seq[Diagnostic],  // diagnostics.
    isProcessingError: Boolean,
    isValidationError: Boolean,
    bitPos1b: Long) {

    def toXML: Node = {
      <Result>
        { message }
        { if (!diags.isEmpty) {
        <diagnostics>
          { diags map { diag => <diagnostic>{ diag.toString} </diagnostic> } }
        </diagnostics>
        }
      else Null
      }
      </Result> %
        (if (isProcessingError) new UnprefixedAttribute("isProcessingError",isProcessingError.toString, Null) else Null) %
        (if (isValidationError) new UnprefixedAttribute("isValidationError", isValidationError.toString, Null) else Null) %
        new UnprefixedAttribute("bitPos1b", bitPos1b.toString, Null)
    }
  }


  def doStreamTest(schema: Node, data: String): Seq[Result] = {
    val mp = new StreamParser(schema)
    val is: InputStream = new ByteArrayInputStream(data.getBytes("ascii"))
    mp.setInputStream(is)
    var r: StreamParser.Result = null
    val results = new ArrayBuffer[Result]
    val resStream = Stream.continually( mp.parse ).takeWhile( r => !r.isProcessingError)
    resStream.toSeq
  }
}

class StreamParser(val schema: Node) {

  val outputter = new ScalaXMLInfosetOutputter()
  var dis: InputSourceDataInputStream = _
  var dp: DFDL.DataProcessor = _
  //
  // First compile the DFDL Schema
  val c = Compiler()
  val pf = c.compileNode(schema)
  val pfDiags = pf.getDiagnostics
  if (pf.isError) throw new StreamParser.CompileFailure(pfDiags)
  dp = pf.onPath("/")
    .withValidationMode(ValidationMode.Full)
  // .withDebuggerRunner(new TraceDebuggerRunner()) // DAFFODIL-2624 - cannot trace in streaming SAPI
  // .withDebugging(true)
  val dpDiags = dp.getDiagnostics
  if (dp.isError) throw new StreamParser.CompileFailure(dpDiags)
  val compilationWarnings = if (!pfDiags.isEmpty) pfDiags else dpDiags // dpDiags might be empty. That's ok.

  def setInputStream(inputStream: InputStream): Unit = {
    dis = InputSourceDataInputStream(inputStream)
  }

  /**
   * Called to pull messages from the data stream.
   *
   * @return a Result object containing the results of the parse including diagnostic information.
   */
  def parse = {
    if (dis == null) throw new IllegalStateException("Input stream must be provided by setInputStream() call.")
    val res: DFDL.ParseResult = dp.parse(dis, outputter)
    val procErr = res.isProcessingError
    val validationErr = res.isValidationError
    val diags = res.getDiagnostics
    val doc = if (!procErr) outputter.getResult else null
    val bitPos1b = res.resultState.currentLocation.bitPos1b
    val r = new StreamParser.Result(doc, diags, procErr, validationErr, bitPos1b)
    outputter.reset()
    r
  }
}
