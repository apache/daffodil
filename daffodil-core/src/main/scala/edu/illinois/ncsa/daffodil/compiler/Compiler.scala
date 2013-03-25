package edu.illinois.ncsa.daffodil.compiler

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

import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import scala.xml.Node
import edu.illinois.ncsa.daffodil.api.DFDL._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.Misc.hex2Bytes
import junit.framework.Assert.assertEquals
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.dsom.SchemaSet
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import java.io.File
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentBase
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.processors.WithDiagnosticsImpl

/**
 * Contains a specification of the root element to be used.
 *
 * The whole RootSpec is generally optional, but if you have one,
 * the namespace part of it is optional as well.
 *
 * When the namespace part is None, it means "you, daffodil, figure out the namespace".
 * Which it will do so long as it is unambiguous.
 */
case class RootSpec(ns: Option[NS], name: String) {
  override def toString() = {
    val nsStr = ns.getOrElse("")
    "{" + nsStr + "}" + name
  }
}

class ProcessorFactory(val sset: SchemaSet)
  extends SchemaComponentBase(<pf/>, sset)
  with ImplementsThrowsSDE
  with DFDL.ProcessorFactory
  with HavingRootSpec {

  requiredEvaluations(rootElem, sset)

  // println("Creating Processor Factory")
  lazy val rootElem = rootElem_.value
  private val rootElem_ = LV('rootElem) {
    sset.rootElement(rootSpec)
  }

  override def isError = {
    OOLAG.keepGoing(true) {
      val valid = sset.isValid
      val res = if (valid) sset.isError
      else true
      res
    }
  }

  override def diagnostics = sset.diagnostics

  override lazy val fileName = sset.fileName

  def onPath(xpath: String): DFDL.DataProcessor = {
    Assert.usage(canProceed)
    if (xpath != "/") rootElem.notYetImplemented("""Path must be "/". Other path support is not yet implemented.""")
    val dataProc = new DataProcessor(this, rootElem)
    if (dataProc.isError) {
      val diags = dataProc.getDiagnostics
      log(Error("Compilation (DataProcessor) reports %s compile errors/warnings.", diags.length))
      diags.foreach { diag => log(Error(diag.toString())) }
    } else {
      log(Compile("Parser = %s.", dataProc.parser.toString))
      log(Compile("Unparser = %s.", dataProc.unparser.toString))
      log(Compile("Compilation (DataProcesor) completed with no errors."))
    }
    dataProc
  }

}

/**
 * Both Compiler and ProcessorFactory share this same API call.
 */
trait HavingRootSpec extends Logging {
  var rootSpec: Option[RootSpec] = None

  def setDistinguishedRootNode(name: String, namespace: String): Unit = {
    val ns =
      if (namespace != null) Some(NS(namespace))
      else None
    rootSpec = Some(RootSpec(ns, name))
    // log(Info("%s setDistinguishedRootNode to %s", Misc.getNameFromClass(this), rootSpec))
    //
    // null means we search for the namespace
    // Must be only one answer.
    //

  }
}

class Compiler extends DFDL.Compiler with Logging with HavingRootSpec {

  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    Assert.notYetImplemented()
  }

  def setDebugging(flag: Boolean) {
    Debugger.setDebugging(flag)
  }

  /**
   * Controls whether we check everything in the schema, or just the element
   * we care about (and everything reachable from it.)
   *
   * You need this control, since many of the big TDML test files have many things
   * in them, some of which use unimplemented features. Each time we run exactly one
   * test from the set, we want to ignore errors in compilation of the others.
   */
  private var checkAllTopLevel = false
  def setCheckAllTopLevel(flag: Boolean) {
    checkAllTopLevel = flag
  }

  /*
   * for unit testing of front end
   */
  def frontEnd(xml: Node): (SchemaSet, GlobalElementDecl) = {
    val (sset, pf) = compileInternal(xml)
    val ge = pf.rootElem
    (sset, ge)
  }

  def reload(fileNameOfSavedParser: String): DFDL.ProcessorFactory = {
    Assert.notYetImplemented()
  }

  /**
   * Compilation works entirely off of schema files because that allows XMLCatalogs
   * to work for Xerces without (much) pain.
   *
   * This method exposes both the schema set and processor factory as results because
   * our tests often want to do things on the schema set.
   */
  def compileInternal(schemaFileNames: Seq[String]): (SchemaSet, ProcessorFactory) = {
    Assert.usage(schemaFileNames.length >= 1)
    val sset = new SchemaSet(schemaFileNames, rootSpec, checkAllTopLevel)
    val pf = new ProcessorFactory(sset)
    val err = pf.isError
    val diags = pf.getDiagnostics // might be warnings even if not isError
    def printDiags = diags.foreach { diag => log(Error(diag.toString())) }
    if (err) {
      Assert.invariant(diags.length > 0)
      log(Error("Compilation (ProcessorFactory) produced %d errors/warnings.", diags.length))
    } else {
      if (diags.length > 0) {
        log(Info("Compilation (ProcessorFactory) produced %d warnings.", diags.length))

      } else {
        log(Compile("ProcessorFactory completed with no errors."))
      }
    }
    printDiags
    (sset, pf)
  }

  /**
   * Just hides the schema set, and returns the processor factory only.
   */
  def compile(fNames: String*): DFDL.ProcessorFactory = compileInternal(fNames)._2

  /**
   * For convenient unit testing allow a literal XML node.
   */
  def compile(xml: Node) = {
    compileInternal(xml)._2
  }

  def compileInternal(xml: Node): (SchemaSet, ProcessorFactory) = {
    val tempSchemaFile = XMLUtils.convertNodeToTempFile(xml)
    compileInternal(List(tempSchemaFile))
  }

}

/**
 * Factory for Compiler instances
 *
 * Size and length limit constants used by the code, some of which will be tunable
 * by the user. Turning them to lower sizes/lengths may improve performance and
 * diagnostic behavior when a format does not need their full range,
 * both by reducing memory footprint, but
 * also by reducing the amount of time taken to scan to the end of what is allowed
 * and fail (and backtrack to try something else) when, for an example, a delimiter
 * is missing from the data.
 *
 * Also has many convenience methods for common test scenarios.
 */
object Compiler {

  //TODO: make tunable via setter call of compiler
  def maxFieldContentLengthInBytes: Long = 1024 // Can be as large as Int.MaxValue
  def occursCountMax: Long = 1024 // Can be as large as Int.MaxValue 
  def maxSkipLength: Long = 1024 // applicable to leadingSkip and trailingSkip
  // TODO: want to lift limit of Int.MaxValue, since these are supposed to be Long integers.
  def readerByteBufferSize: Long = 8192
  def generatedNamespacePrefixStem = "tns"

  def apply() = new Compiler()

  def stringToReadableByteChannel(s: String): DFDL.Input = {
    val bytes = s.getBytes("utf-8") // never use default charset. NEVER.
    byteArrayToReadableByteChannel(bytes)
  }

  def stringToWritableByteChannel(s: String): DFDL.Output = {
    val size = s.length() // TODO: get byte count by encoding
    byteArrayToWritableByteChannel(size)
  }

  def byteArrayToReadableByteChannel(bytes: Array[Byte]): DFDL.Input = {
    val inputStream = new ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def byteArrayToWritableByteChannel(size: Int): DFDL.Output = {
    val outputStream = new ByteArrayOutputStream(size);
    val wbc = java.nio.channels.Channels.newChannel(outputStream);
    wbc
  }

  def fileToReadableByteChannel(file: java.io.File): DFDL.Input = {
    val inputStream = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def runSchemaOnData(testSchema: Node, data: DFDL.Input) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val isError = pf.isError
    val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")

    if (isError) {
      throw new Exception(msgs)
    }
    val p = pf.onPath("/")
    val pIsError = p.isError
    if (pIsError) {
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

  def testString(testSchema: Node, data: String) = {
    runSchemaOnData(testSchema, Compiler.stringToReadableByteChannel(data))
  }

  def testBinary(testSchema: Node, hexData: String) = {
    val b = hex2Bytes(hexData)
    val rbc = byteArrayToReadableByteChannel(b)
    runSchemaOnData(testSchema, rbc)
  }

  def testFile(testSchema: Node, fileName: String) = {
    runSchemaOnData(testSchema, Compiler.fileToReadableByteChannel(new java.io.File(fileName)))
  }

  def testUnparsing(testSchema: scala.xml.Elem, infoset: Node, unparseTo: String) {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/")
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val actual = u.unparse(out, infoset)
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
    val pf = compiler.compile(testSchema)
    val u = pf.onPath("/")
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val actual = u.unparse(out, infoset)
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
}

