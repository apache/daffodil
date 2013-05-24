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
import edu.illinois.ncsa.daffodil.ExecutionMode

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
    ExecutionMode.usingCompilerMode {
      OOLAG.keepGoing(true) {
        val valid = sset.isValid
        val res = if (valid) sset.isError
        else true
        res
      }
    }
  }

  override def diagnostics = sset.diagnostics

  override lazy val fileName = sset.fileName

  def onPath(xpath: String): DFDL.DataProcessor = {
    ExecutionMode.usingCompilerMode {
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
  def compileInternal(schemaFiles: Seq[File]): (SchemaSet, ProcessorFactory) = {
    ExecutionMode.usingCompilerMode {
      Assert.usage(schemaFiles.length >= 1)

      val filesNotFound = schemaFiles.map { f => (f.exists(), f.getPath()) }.filter { case (exists, _) => !exists }.map { case (_, name) => name }
      if (filesNotFound.length > 0) throw new java.io.FileNotFoundException("Failed to find the following file(s): " + filesNotFound.mkString(", "))

      val sset = new SchemaSet(schemaFiles, rootSpec, checkAllTopLevel)
      val pf = new ProcessorFactory(sset)
      val err = pf.isError
      val diags = pf.getDiagnostics // might be warnings even if not isError
      def printDiags = diags.foreach { diag =>
        val msg = diag.toString()
        log(Error(msg))
      }
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
  }

  /**
   * Just hides the schema set, and returns the processor factory only.
   */
  def compile(files: File*): DFDL.ProcessorFactory = compileInternal(files)._2
  //def compile(fNames: String*): DFDL.ProcessorFactory = compileInternal(fNames)._2

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

  //FIXME: These tunables need to be changable per compilation, not global like this. 
  //Some - like time limits, should be settable on the processor, not the PF or Compiler.
  //TODO: make tunable via setter call of compiler
  def maxFieldContentLengthInBytes: Long = 1024 // Can be as large as Int.MaxValue
  def occursCountMax: Long = 1024 // Can be as large as Int.MaxValue 
  def maxSkipLength: Long = 1024 // applicable to leadingSkip and trailingSkip
  // TODO: want to lift limit of Int.MaxValue, since these are supposed to be Long integers.
  def readerByteBufferSize: Long = 8192
  def generatedNamespacePrefixStem = "tns"

  def apply() = new Compiler()

}

