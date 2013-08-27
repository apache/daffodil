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

import java.io.File
import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentBase
import edu.illinois.ncsa.daffodil.dsom.SchemaSet
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.dsom.Fakes
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.processors.PrimitiveFactory
import edu.illinois.ncsa.daffodil.util.Compile
import edu.illinois.ncsa.daffodil.util.Error
import edu.illinois.ncsa.daffodil.util.Info
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.externalvars.Binding
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader

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

  private val externalDFDLVariables: Queue[Binding] = Queue.empty

  /**
   * Sets externally defined variables.
   *
   * @param name The variable name excluding the namespace or namespace prefix.
   *
   * @param namespace The namespace where empty string is interpreted as NoNamespace and null
   * is interpreted as 'figure out the namespace'.
   *
   * @param value The variable's value.
   *
   */
  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    // We must tolerate null here for namespace in order to be compatible with Java
    val ns = namespace match {
      case null => None // Figure out the namespace
      case _ => Some(NS(namespace))
    }
    val b = Binding(name, ns, value)
    externalDFDLVariables.enqueue(b)
  }

  def setExternalDFDLVariable(variable: Binding) = externalDFDLVariables.enqueue(variable)
  def setExternalDFDLVariables(variables: Seq[Binding]) = variables.foreach(b => setExternalDFDLVariable(b))
  def setExternalDFDLVariables(extVarsFile: File): Unit = {
    val extVars = ExternalVariablesLoader.getVariables(extVarsFile)
    setExternalDFDLVariables(extVars)
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

      val sset = new SchemaSet(externalDFDLVariables, PrimitiveFactory, schemaFiles, rootSpec, checkAllTopLevel)
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
 */
object Compiler {

  def apply() = new Compiler()

}

