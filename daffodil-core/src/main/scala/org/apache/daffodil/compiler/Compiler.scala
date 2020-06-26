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

import java.io.File
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.StreamCorruptedException
import java.nio.channels.Channels
import java.util.zip.GZIPInputStream
import java.util.zip.ZipException

import scala.collection.immutable.Queue
import scala.xml.Node

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilSchemaSource
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.api.UnitTestSchemaSource
import org.apache.daffodil.dsom.SchemaSet
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.Misc
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

final class ProcessorFactory private(
  private var optRootSpec: Option[RootSpec],
  /*
   * compilerExternalVarSettings supports the deprecated API
   * where external variable settings can be supplied to the compiler
   * instance.
   *
   * The non-deprecated API is to deal with external variable settings
   * on the data processor instance. This is passed here so that
   * the PF can propagate it to the DP.
   */
  var compilerExternalVarSettings: Queue[Binding],
  schemaSource: DaffodilSchemaSource,
  val validateDFDLSchemas: Boolean,
  checkAllTopLevel: Boolean,
  tunables: DaffodilTunables,
  optSchemaSet: Option[SchemaSet])
  extends DFDL.ProcessorFactory {

  def this(optRootName: Option[String],
    optRootNamespace: Option[String],
    compilerExternalVarSettings: Queue[Binding],
    schemaSource: DaffodilSchemaSource,
    validateDFDLSchemas: Boolean,
    checkAllTopLevel: Boolean,
    tunables: DaffodilTunables) =
    this(
      RootSpec.makeRootSpec(optRootName, optRootNamespace), // compute root-spec object
      compilerExternalVarSettings, schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables, None)

  private def copy(
    optRootSpec: Option[RootSpec] = optRootSpec,
    compilerExternalVarSettings: Queue[Binding] = compilerExternalVarSettings): ProcessorFactory =
    new ProcessorFactory(optRootSpec, compilerExternalVarSettings, schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables, Some(sset))

  lazy val sset: SchemaSet =
    optSchemaSet.getOrElse(
      new SchemaSet(optRootSpec, schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables,
        compilerExternalVarSettings))

  def elementBaseInstanceCount = sset.elementBaseInstanceCount

  def diagnostics = sset.diagnostics
  def getDiagnostics = diagnostics

  override def onPath(xpath: String) = sset.onPath(xpath)

  override def isError = sset.isError

  @deprecated("Use arguments to Compiler.compileSource or compileFile.", "2.6.0")
  override def setDistinguishedRootNode(name: String, namespace: String): Unit = {
    Assert.usage(name ne null)
    optRootSpec = RootSpec.makeRootSpec(Option(name), Option(namespace))
  }

  def withDistinguishedRootNode(name: String, namespace: String) : ProcessorFactory = {
    Assert.usage(name ne null)
    copy(optRootSpec = RootSpec.makeRootSpec(Option(name), Option(namespace)))
  }
}

class InvalidParserException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

class Compiler private (var validateDFDLSchemas: Boolean,
  var tunables : DaffodilTunables,
  /*
   * Supports deprecated feature of establishing external vars on the compiler object.
   * These are just saved and passed to the processor factory which incorporates them into
   * the variable map of the data processor.
   *
   * This argument can be removed once this deprecated feature is removed.
   */
  private var externalDFDLVariables: Queue[Binding],
  private var checkAllTopLevel : Boolean,
  private var optRootName: Option[String],
  private var optRootNamespace: Option[String])
  extends DFDL.Compiler
  with Logging {

  private def this(validateDFDLSchemas: Boolean = true) = this(validateDFDLSchemas, DaffodilTunables(), Queue.empty, true, None, None)

  private def copy(validateDFDLSchemas: Boolean = validateDFDLSchemas,
    tunables : DaffodilTunables = tunables,
    externalDFDLVariables: Queue[Binding] = externalDFDLVariables,
    checkAllTopLevel : Boolean = checkAllTopLevel,
    optRootName: Option[String] = optRootName,
    optRootNamespace: Option[String] = optRootNamespace) =
    new Compiler(validateDFDLSchemas, tunables, externalDFDLVariables, checkAllTopLevel, optRootName, optRootNamespace)

  @deprecated("Pass arguments to compileSource, or compileFile.", "2.6.0")
  override def setDistinguishedRootNode(name: String, namespace: String): Unit = {
    Assert.usage(name ne null)
    optRootName = Option(name)
    optRootNamespace = Option(namespace)
  }

  def withDistinguishedRootNode(name: String, namespace: String) : Compiler = {
    Assert.usage(name ne null)
    copy(optRootName = Option(name), optRootNamespace = Option(namespace))
  }

  @deprecated("Use constructor argument.", "2.6.0")
  def setValidateDFDLSchemas(value: Boolean) = validateDFDLSchemas = value

  def withValidateDFDLSchemas(value: Boolean) = copy(validateDFDLSchemas = value)

  @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
  override def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    externalDFDLVariables = externalDFDLVariables.enqueue(getBinding(name, namespace, value))
  }

  /**
   * Supports binding external variables programatically from the API.
   */
  private def getBinding(name: String, namespace: String, value: String): Binding = {
    // We must tolerate null here for namespace in order to be compatible with Java
    val ns = namespace match {
      case null => None // Figure out the namespace
      case _ => Some(NS(namespace))
    }
    val b = Binding(name, ns, value)
    b
  }

  @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
  def setExternalDFDLVariable(variable: Binding): Unit =
    externalDFDLVariables = externalDFDLVariables.enqueue(variable)

  @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
  def setExternalDFDLVariables(variables: Seq[Binding]): Unit =
    variables.foreach(b => externalDFDLVariables = externalDFDLVariables.enqueue(b))

  //
  // Not deprecated so that we can implement the deprecated things
  // and reuse code.
  // When the deprecated methods go away, so should this.
  //
  def withExternalDFDLVariablesImpl(variables: Seq[Binding]): Compiler = {
    var extVars = externalDFDLVariables
    variables.foreach(b => extVars = extVars.enqueue(b))
    copy(externalDFDLVariables = extVars)
  }

  @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
  def setExternalDFDLVariables(extVarsFile: File): Unit = {
    val extVars: Seq[Binding] = ExternalVariablesLoader.fileToBindings(extVarsFile)
    extVars.foreach(b => externalDFDLVariables = externalDFDLVariables.enqueue(b))
  }

  @deprecated("Use withTunable.", "2.6.0")
  def setTunable(tunable: String, value: String): Unit =
    tunables = tunables.setTunable(tunable, value)

  def withTunable(tunable: String, value: String): Compiler =
    copy(tunables = tunables.setTunable(tunable, value))

  @deprecated("Use withTunables.", "2.6.0")
  def setTunables(tunablesArg: Map[String, String]): Unit =
    tunables = tunables.setTunables(tunablesArg)

  def withTunables(tunablesArg: Map[String, String]): Compiler =
    copy(tunables = tunables.setTunables(tunablesArg))

  /**
   * Controls whether we check everything in the schema, or just the element
   * we care about (and everything reachable from it.)
   *
   * You need this control, since many of the big TDML test files have many things
   * in them, some of which use unimplemented features. Each time we run exactly one
   * test from the set, we want to ignore errors in compilation of the others.
   */
  @deprecated("Use withCheckAllTopLevel.", "2.6.0")
  def setCheckAllTopLevel(flag: Boolean): Unit =
    checkAllTopLevel = flag

  def withCheckAllTopLevel(flag: Boolean): Compiler =
    copy(checkAllTopLevel = flag)

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
      val dp = dpObj.asInstanceOf[DataProcessor]
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
        val fmtMsg = "%s\nThe class may not exist in this Java JVM version (%s), or it is missing from the classpath which is%s"
        val msg = fmtMsg.format(cnf.getMessage(), scala.util.Properties.javaVersion, cpString)
        throw new InvalidParserException(msg, cnf)
      }
    }
  }
  /**
   * Compilation returns a parser factory, which must be interrogated for diagnostics
   * to see if compilation was successful or not.
   */
  def compileFile(file: File,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None): ProcessorFactory = {
    val source = URISchemaSource(file.toURI)
    compileSource(source, optRootName, optRootNamespace)
  }

  /**
   * Synchronized on the global Compiler singleton object.
   *
   * This is to avoid issues when TDML tests are running in parallel
   * and compiling schemas that are not in files, but just embedded in the tests.
   *
   * The optRootName and optRootNamespace supplied here supercede any provided via
   * a setter/with-er method.
   */
  def compileSource(
    schemaSource: DaffodilSchemaSource,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None): ProcessorFactory = {
    Compiler.compileSourceSynchronizer(this, schemaSource, optRootName, optRootNamespace)
  }

  private def compileSourceInternal(
    schemaSource: DaffodilSchemaSource,
    optRootNameArg: Option[String],
    optRootNamespaceArg: Option[String]): ProcessorFactory = {

    val pf: ProcessorFactory = {
      val rootName = optRootNameArg.orElse(optRootName) // arguments override things set with setters
      val rootNamespace = optRootNamespaceArg.orElse(optRootNamespace)
      new ProcessorFactory(
        rootName, rootNamespace, externalDFDLVariables, schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables)
    }

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
    log(LogLevel.Compile, "Schema had %s elements.", pf.elementBaseInstanceCount)
    pf
  }

  /**
   * For convenient unit testing allow a literal XML node.
   */
  def compileNode(xml: Node, optTmpDir: Option[File] = None): ProcessorFactory = {
    compileSource(UnitTestSchemaSource(xml, "anon", optTmpDir), None, None)
  }

}

/**
 * Factory for Compiler instances
 */
object Compiler {

  def apply(validateDFDLSchemas: Boolean = true) = new Compiler(validateDFDLSchemas)

  private def compileSourceSynchronizer(
    c: Compiler,
    schemaSource: DaffodilSchemaSource,
    optRootName: Option[String],
    optRootNamespace: Option[String]) : ProcessorFactory = {
    synchronized {
      c.compileSourceInternal(schemaSource, optRootName, optRootNamespace)
    }
  }

}
