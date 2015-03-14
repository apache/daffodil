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
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import java.io.File
import java.io.FileNotFoundException
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import java.nio.channels.ReadableByteChannel
import org.junit.Assert.assertEquals
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.api.DFDL._
import edu.illinois.ncsa.daffodil.processors.unparsers.InfosetSource

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

  def testString(testSchema: Node, data: String) = {
    runSchemaOnData(testSchema, Misc.stringToReadableByteChannel(data))
  }

  def testBinary(testSchema: Node, hexData: String) = {
    val b = Misc.hex2Bytes(hexData)
    val rbc = Misc.byteArrayToReadableByteChannel(b)
    runSchemaOnData(testSchema, rbc)
  }

  def testFile(testSchema: Node, fileName: String) = {
    runSchemaOnData(testSchema, Misc.fileToReadableByteChannel(new java.io.File(fileName)))
  }

  def testUnparsing(testSchema: scala.xml.Elem, infosetXML: Node, unparseTo: String) {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val xmlEventReader = XMLUtils.nodeToXMLEventReader(infosetXML)
    val actual = u.unparse(out, xmlEventReader)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val unparsed = outputStream.toString
    //    System.err.println("parsed: " + infoset)
    //    System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparseTo, unparsed)
  }

  def testUnparsingBinary(testSchema: scala.xml.Elem, infoset: Node, unparseTo: Array[Byte]) {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    val u = pf.onPath("/")
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val xmlEventReader = XMLUtils.nodeToXMLEventReader(infoset)
    val actual = u.unparse(out, xmlEventReader)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
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

  def runSchemaOnData(testSchema: Node, data: ReadableByteChannel) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    val isError = pf.isError
    val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")

    if (isError) {
      throw new Exception(msgs)
    }
    val p = pf.onPath("/")
    val pIsError = p.isError
    if (pIsError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val d = data
    val actual = p.parse(d)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    actual
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
}

class Fakes private () {
  lazy val sch = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>
    <xs:element name="fake2" type="tns:fakeCT"/>
    <xs:complexType name="fakeCT">
      <xs:sequence>
        <xs:group ref="tns:fakeGroup"/>
        <xs:element ref="tns:fake"/>
      </xs:sequence>
    </xs:complexType>
    <xs:group name="fakeGroup">
      <xs:choice>
        <xs:sequence/>
      </xs:choice>
    </xs:group>)
  val DummyPrimitiveFactory = null
  lazy val xsd_sset: SchemaSet = new SchemaSet(sch, "http://example.com", "fake")
  lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
  lazy val fakeSD = xsd_schema.schemaDocuments(0)
  lazy val fakeElem = fakeSD.getGlobalElementDecl("fake").get.forRoot()
  lazy val fakeCT = fakeSD.getGlobalElementDecl("fake2").get.forRoot().typeDef.asInstanceOf[GlobalComplexTypeDef]
  lazy val fakeSequence = fakeCT.modelGroup.asInstanceOf[Sequence]
  lazy val Seq(fs1, fs2) = fakeSequence.groupMembers
  lazy val fakeGroupRef = fs1.asInstanceOf[GroupRef]

  class FakeDataProcessor extends DataProcessor {
    def setValidationMode(mode: ValidationMode.Type): Unit = {}
    def getValidationMode(): ValidationMode.Type = { ValidationMode.Full }
    def save(output: DFDL.Output): Unit = {}
    def setExternalVariables(extVars: Map[String, String]): Unit = {}
    def setExternalVariables(extVars: Seq[Binding]): Unit = {}
    def setExternalVariables(extVars: File): Unit = {}
    def getVariables(): VariableMap = EmptyVariableMap
    def parse(input: Input, lengthLimitInBits: Long = -1): ParseResult = null
    def parse(file: File): ParseResult = null
    def unparse(output: Output, xmlEventReader: Iterator[scala.xml.pull.XMLEvent]): UnparseResult = null
    def unparse(output: DFDL.Output, infosetXML: scala.xml.Node): UnparseResult = null
    def unparse(output: DFDL.Output, infosetSource: InfosetSource): UnparseResult = null
    def getDiagnostics: Seq[Diagnostic] = Seq.empty
    //final lazy val canProceed: Boolean = !isError
    def isError: Boolean = false
  }
  lazy val fakeDP = new FakeDataProcessor
}

