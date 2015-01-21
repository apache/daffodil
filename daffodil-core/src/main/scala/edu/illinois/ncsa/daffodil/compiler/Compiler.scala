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
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.nio.channels.Channels
import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentBase
import edu.illinois.ncsa.daffodil.dsom.SchemaSet
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.util.Compile
import edu.illinois.ncsa.daffodil.util.Error
import edu.illinois.ncsa.daffodil.util.Warning
import edu.illinois.ncsa.daffodil.util.Info
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DaffodilSchemaSource
import edu.illinois.ncsa.daffodil.api.UnitTestSchemaSource
import edu.illinois.ncsa.daffodil.externalvars.Binding
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.processors.SchemaSetRuntimeData
import edu.illinois.ncsa.daffodil.util.CheckJavaVersion
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.processors.VariableMap
import java.util.zip.GZIPInputStream
import java.util.zip.ZipException
import java.io.StreamCorruptedException
import org.xml.sax.InputSource
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.api.URISchemaSource

class ProcessorFactory(val sset: SchemaSet)
  extends SchemaComponentBase(<pf/>, sset)
  with DFDL.ProcessorFactory
  with HavingRootSpec {

  //
  // breaking this into these lines causes the order things are
  // evaluated in to be more rational than if pure demand-driven
  // lazy evaluation was followed.
  //
  // We want pretty much nothing to be done by the data processor
  //
  requiredEvaluations(sset)
  requiredEvaluations(rootElem)
  requiredEvaluations(rootElem.document)
  requiredEvaluations(rootElem.documentElement)
  requiredEvaluations(rootElem.documentElement.gram)
  requiredEvaluations(rootElem.document.parser)
  requiredEvaluations(rootElem.runtimeData)

  lazy val rootElem = rootElem_.value
  private val rootElem_ = LV('rootElem) {
    sset.rootElement(rootSpec)
  }

  override def isError = {
    ExecutionMode.usingCompilerMode {
      OOLAG.keepGoing(true) {
        val valid = sset.isValid
        if (valid) {
          // no point in going forward with more
          // checks if the schema isn't valid
          // The code base is written assuming valid
          // schema input. It's just going to hit 
          // assertion failures and such if we 
          // try to compile invalid schemas.
          val requiredErr = super.isError
          val ssetErr = sset.isError
          val res = requiredErr || ssetErr
          res
        } else true
      }
    }
  }

  override def diagnostics = sset.diagnostics

  def onPath(xpath: String): DFDL.DataProcessor = {
    ExecutionMode.usingCompilerMode {
      Assert.usage(canProceed)
      if (xpath != "/") rootElem.notYetImplemented("""Path must be "/". Other path support is not yet implemented.""")
      val validationMode = ValidationMode.Off
      val variables: VariableMap = rootElem.schemaDocument.schemaSet.variableMap
      val p = if (rootElem.canProceed) rootElem.document.parser else null
      val d = this.diagnostics
      val ssrd = new SchemaSetRuntimeData(
        p,
        this.diagnostics,
        rootElem.elementRuntimeData,
        rootElem.encodingInfo,
        variables,
        validationMode)
      CheckJavaVersion.checkJavaVersion(ssrd)
      val dataProc = new DataProcessor(ssrd)
      if (dataProc.isError) {
        // NO longer printing anything here. Callers must do this.
        //        val diags = dataProc.getDiagnostics
        //        log(Error("Compilation (DataProcessor) reports %s compile errors/warnings.", diags.length))
        //        diags.foreach { diag => log(Error(diag.toString())) }
      } else {
        log(Compile("Parser = %s.", ssrd.parser.toString))
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
class InvalidParserException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

class Compiler(var validateDFDLSchemas: Boolean = true)
  extends DFDL.Compiler
  with Logging
  with HavingRootSpec {

  def setValidateDFDLSchemas(value: Boolean) = validateDFDLSchemas = value

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

  def setTunable(tunable: String, value: String): Unit = {
    tunable.toLowerCase match {
      case "requirebitorderproperty" => DaffodilTunableParameters.requireBitOrderProperty = java.lang.Boolean.valueOf(value)
      case "requireencodingerrorpolicyproperty" => DaffodilTunableParameters.requireEncodingErrorPolicyProperty = java.lang.Boolean.valueOf(value)
      case "maxfieldcontentlengthinbytes" => DaffodilTunableParameters.maxFieldContentLengthInBytes = java.lang.Long.valueOf(value)
      case "maxoccursbounds" => DaffodilTunableParameters.maxOccursBounds = java.lang.Long.valueOf(value)
      case "maxskiplength" => DaffodilTunableParameters.maxSkipLength = java.lang.Long.valueOf(value)
      case "maxbinarydecimalvirtualpoint" => DaffodilTunableParameters.maxBinaryDecimalVirtualPoint = java.lang.Integer.valueOf(value)
      case "minbinarydecimalvirtualpoint" => DaffodilTunableParameters.minBinaryDecimalVirtualPoint = java.lang.Integer.valueOf(value)
      case "generatednamespaceprefixstem" => DaffodilTunableParameters.generatedNamespacePrefixStem = value
      case "readerbytebuffersize" => DaffodilTunableParameters.readerByteBufferSize = java.lang.Long.valueOf(value)
      case "maxlengthforvariablelengthdelimiterdisplay" => DaffodilTunableParameters.maxLengthForVariableLengthDelimiterDisplay = java.lang.Integer.valueOf(value)
      case "inputfilememorymaplowthreshold" => DaffodilTunableParameters.inputFileMemoryMapLowThreshold = java.lang.Long.valueOf(value)
      case "initialelementoccurrenceshint" => DaffodilTunableParameters.initialElementOccurrencesHint = java.lang.Long.valueOf(value)
      case _ => log(Warning("Ignoring unknown tunable: %s", tunable))
    }
  }

  def setTunables(tunables: Map[String, String]): Unit = {
    tunables.foreach { case (k, v) => setTunable(k, v) }
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

  def reload(savedParser: File) = reload(new FileInputStream(savedParser).getChannel())

  def reload(savedParser: DFDL.Input): DFDL.DataProcessor = {
    try {
      val objInput = new ObjectInputStream(new GZIPInputStream(Channels.newInputStream(savedParser))) {

        ///
        /// This override is here because of a bug in sbt where the wrong class loader is being
        /// used when deserializing an object.
        //  For more information, see https://github.com/sbt/sbt/issues/163
        ///
        override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
          try { Class.forName(desc.getName, false, getClass.getClassLoader) }
          catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
        }
      }

      val dpObj = objInput.readObject()
      objInput.close()
      val dp = dpObj.asInstanceOf[DataProcessor]
      CheckJavaVersion.checkJavaVersion(dp.ssrd)
      dp
    } catch {
      case ex: ZipException => {
        throw new InvalidParserException("The saved parser file is not the correct format.", ex)
      }
      case ex: StreamCorruptedException => {
        throw new InvalidParserException("The saved parser file is not a valid parser.", ex)
      }
    }
  }
  /**
   * Compilation returns a parser factory, which must be interrogated for diagnostics
   * to see if compilation was successful or not.
   */
  def compileFile(file: File): ProcessorFactory = {
    val source = URISchemaSource(file.toURI)
    compileSource(source)
  }

  def compileSource(schemaSource: DaffodilSchemaSource): ProcessorFactory = {
    val noParent = null // null indicates this is the root, and has no parent
    val sset = new SchemaSet(rootSpec, externalDFDLVariables, Seq(schemaSource), validateDFDLSchemas, checkAllTopLevel, noParent)
    val pf = new ProcessorFactory(sset)
    val err = pf.isError
    val diags = pf.getDiagnostics // might be warnings even if not isError
    if (err) {
      Assert.invariant(diags.length > 0)
      log(Compile("Compilation (ProcessorFactory) produced %d errors/warnings.", diags.length))
    } else {
      if (diags.length > 0) {
        log(Compile("Compilation (ProcessorFactory) produced %d warnings.", diags.length))
      } else {
        log(Compile("ProcessorFactory completed with no errors."))
      }
    }
    log(Compile("Schema had %s elements.", ElementBase.count))
    pf
  }

  /**
   * For convenient unit testing allow a literal XML node.
   */
  def compileNode(xml: Node): ProcessorFactory = {
    compileSource(UnitTestSchemaSource(xml, "anon"))
  }

}

/**
 * Factory for Compiler instances
 */
object Compiler {

  def apply(validateDFDLSchemas: Boolean) = new Compiler(validateDFDLSchemas)
  def apply() = new Compiler()

}

