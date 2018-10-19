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

package org.apache.daffodil.compiler

import java.io.{ File, FileInputStream, ObjectInputStream, StreamCorruptedException }
import java.nio.channels.Channels
import java.util.zip.{ GZIPInputStream, ZipException }

import scala.collection.mutable.Queue
import scala.xml.Node

import org.apache.daffodil.ExecutionMode
import org.apache.daffodil.api.{ DFDL, DaffodilSchemaSource, DaffodilTunables, URISchemaSource, UnitTestSchemaSource, ValidationMode }
import org.apache.daffodil.dsom.{ ElementBase, SchemaComponent, SchemaComponentImpl, SchemaSet }
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.externalvars.{ Binding, ExternalVariablesLoader }
import org.apache.daffodil.oolag.OOLAG
import org.apache.daffodil.processors.{ DataProcessor, Processor, SchemaSetRuntimeData, SerializableDataProcessor, VariableMap }
import org.apache.daffodil.processors.parsers.NotParsableParser
import org.apache.daffodil.processors.unparsers.NotUnparsableUnparser
import org.apache.daffodil.schema.annotation.props.gen.ParseUnparsePolicy
import org.apache.daffodil.util.{ LogLevel, Logging, Misc }
import org.apache.daffodil.xml._

/**
 * Some grammar rules need to be conditional based on whether we're trying
 * for a parser or an unparser.
 *
 * As a result, many grammar rules now have to be def, not lazy val, since
 * they are parameterized by this.
 *
 * Note that using a dynamic variable doesn't work - because the time when the grammar rules
 * are evaluated isn't necessarily within the dynamic scope of the variable
 * binding. It might happen later.
 */
sealed class ParserOrUnparser
object ForParser extends ParserOrUnparser
object ForUnparser extends ParserOrUnparser
object BothParserAndUnparser extends ParserOrUnparser

final class ProcessorFactory(val sset: SchemaSet)
  extends SchemaComponentImpl(<pf/>, sset)
  with DFDL.ProcessorFactory
  with HavingRootSpec {

  final override def enclosingComponentDef: Option[SchemaComponent] = None

  lazy val (generateParser, generateUnparser) = {
    val (context, policy) =
      if (tunable.parseUnparsePolicy.isDefined) {
        (None, tunable.parseUnparsePolicy.get)
      } else {
        (Some(rootElem), rootElem.rootParseUnparsePolicy)
      }
    rootElem.checkParseUnparsePolicyCompatibility(context, policy)
    policy match {
      case ParseUnparsePolicy.Both => (true, true)
      case ParseUnparsePolicy.ParseOnly => (true, false)
      case ParseUnparsePolicy.UnparseOnly => (false, true)
    }
  }

  lazy val parser = LV('parser) {
    val par = if (generateParser) rootElem.document.parser else new NotParsableParser(rootElem.erd)
    Processor.initialize(par)
    par
  }.value

  lazy val unparser = LV('unparser) {
    val unp = if (generateUnparser) rootElem.document.unparser else new NotUnparsableUnparser(rootElem.erd)
    Processor.initialize(unp)
    unp
  }.value

  lazy val rootElem = LV('rootElem) {
    sset.rootElement(rootSpec)
  }.value

  //
  // breaking this into these lines causes the order things are
  // evaluated in to be more rational than if pure demand-driven
  // lazy evaluation was followed.
  //
  // We want pretty much nothing to be done by the data processor
  //
  requiredEvaluations(sset)
  requiredEvaluations(rootElem)
  requiredEvaluations(parser)
  requiredEvaluations(unparser)
  requiredEvaluations(rootElem.runtimeData)

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
      Assert.usage(!isError)
      if (xpath != "/") rootElem.notYetImplemented("""Path must be "/". Other path support is not yet implemented.""")
      val rootERD = rootElem.elementRuntimeData
      rootElem.schemaDefinitionUnless(rootERD.outputValueCalcExpr.isEmpty,
        "The root element cannot have the dfdl:outputValueCalc property.")
      val validationMode = ValidationMode.Off
      val variables: VariableMap = rootElem.schemaDocument.schemaSet.variableMap
      val p = if (!rootElem.isError) parser else null
      val u = if (!rootElem.isError) unparser else null
      val ssrd = new SchemaSetRuntimeData(
        p,
        u,
        this.diagnostics,
        rootERD,
        variables,
        validationMode)
      val dataProc = new DataProcessor(ssrd)
      if (dataProc.isError) {
        // NO longer printing anything here. Callers must do this.
        //        val diags = dataProc.getDiagnostics
        //        log(LogLevel.Error,"Compilation (DataProcessor) reports %s compile errors/warnings.", diags.length)
        //        diags.foreach { diag => log(LogLevel.Error, diag.toString()) }
      } else {
        log(LogLevel.Compile, "Parser = %s.", ssrd.parser.toString)
        //log(LogLevel.Error, "Unparser = %s.", ssrd.unparser.toString)
        log(LogLevel.Compile, "Compilation (DataProcesor) completed with no errors.")
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

  private var tunablesObj = DaffodilTunables()

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
    val extVars = ExternalVariablesLoader.getVariables(extVarsFile, tunablesObj)
    setExternalDFDLVariables(extVars)
  }

  def setTunable(tunable: String, value: String): Unit = {
    tunablesObj = tunablesObj.setTunable(tunable, value)
  }

  def setTunables(tunables: Map[String, String]): Unit = {
    tunablesObj = tunablesObj.setTunables(tunables)
  }

  def resetTunables(): Unit = {
    tunablesObj = DaffodilTunables()
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

  def reload(savedParser: java.nio.channels.ReadableByteChannel): DFDL.DataProcessor = {
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
      val dp = dpObj.asInstanceOf[SerializableDataProcessor]
      dp
    } catch {
      case ex: ZipException => {
        throw new InvalidParserException("The saved parser file is not the correct format.", ex)
      }
      case ex: StreamCorruptedException => {
        throw new InvalidParserException("The saved parser file is not a valid parser.", ex)
      }
      //
      // If we are running on Java 7, and a class such as Base64 (only in Java 8)
      // needs to be created as part of loading the schema, then we'll get a
      // class not found exception. This catches that and issues a
      // sensible diagnostic.
      //
      // Similarly, if a class *should* be on the classpath in order for this
      // schema to reload, then we will get CNF, and we issue a diagnostic
      // which also displays the classpath.
      //
      case cnf: ClassNotFoundException => {
        val cpString =
          if (Misc.classPath.length == 0) " empty."
          else ":\n" + Misc.classPath.mkString("\n\t")
        val msg = "%s\nThe class may not exist in this Java JVM version (%s)," +
          "or it is missing from the classpath which is%s".format(
            cnf.getMessage(),
            scala.util.Properties.javaVersion,
            cpString
          )
        throw new InvalidParserException(msg, cnf)
      }
    }
  }
  /**
   * Compilation returns a parser factory, which must be interrogated for diagnostics
   * to see if compilation was successful or not.
   */
  def compileFile(file: File): ProcessorFactory = {
    val source = new URISchemaSource(file.toURI)
    compileSource(source)
  }

  /**
   * Synchronized on the global Compiler singleton object.
   *
   * This is to avoid issues when TDML tests are running in parallel
   * and compiling schemas that are not in files, but just embedded in the tests.
   */
  def compileSource(schemaSource: DaffodilSchemaSource): ProcessorFactory =
    Compiler.synchronized {
      val noParent = null // null indicates this is the root, and has no parent
      val sset = new SchemaSet(rootSpec, externalDFDLVariables, Seq(schemaSource), validateDFDLSchemas, checkAllTopLevel, noParent, tunablesObj)
      val pf = new ProcessorFactory(sset)
      val err = pf.isError
      val diags = pf.getDiagnostics // might be warnings even if not isError
      if (err) {
        Assert.invariant(diags.length > 0)
        log(LogLevel.Compile, "Compilation (ProcessorFactory) produced %d errors/warnings.", diags.length)
      } else {
        if (diags.length > 0) {
          log(LogLevel.Compile, "Compilation (ProcessorFactory) produced %d warnings.", diags.length)
        } else {
          log(LogLevel.Compile, "ProcessorFactory completed with no errors.")
        }
      }
      log(LogLevel.Compile, "Schema had %s elements.", ElementBase.count)
      pf
    }

  /**
   * For convenient unit testing allow a literal XML node.
   */
  def compileNode(xml: Node, optTmpDir: Option[File] = None): ProcessorFactory = {
    compileSource(UnitTestSchemaSource(xml, "anon", optTmpDir))
  }

}

/**
 * Factory for Compiler instances
 */
object Compiler {

  def apply(validateDFDLSchemas: Boolean) = new Compiler(validateDFDLSchemas)
  def apply() = new Compiler()

}
