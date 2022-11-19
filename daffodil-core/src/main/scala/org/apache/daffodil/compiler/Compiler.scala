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
import java.io.InvalidClassException
import java.io.ObjectInputStream
import java.io.StreamCorruptedException
import java.nio.channels.Channels
import java.util.zip.GZIPInputStream
import java.util.zip.ZipException

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.xml.Node

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilSchemaSource
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.api.UnitTestSchemaSource
import org.apache.daffodil.dsom.SchemaSet
import org.apache.daffodil.dsom.walker.RootView
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.util.Logger
import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml._
import java.net.URI

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
  private val optRootSpec: Option[RootSpec],
  schemaSource: DaffodilSchemaSource,
  val validateDFDLSchemas: Boolean,
  checkAllTopLevel: Boolean,
  tunables: DaffodilTunables,
  optSchemaSet: Option[SchemaSet])
  extends DFDL.ProcessorFactory {

  def this(optRootName: Option[String],
    optRootNamespace: Option[String],
    schemaSource: DaffodilSchemaSource,
    validateDFDLSchemas: Boolean,
    checkAllTopLevel: Boolean,
    tunables: DaffodilTunables) =
    this(
      RootSpec.makeRootSpec(optRootName, optRootNamespace), // compute root-spec object
      schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables, None)

  private def copy(
    optRootSpec: Option[RootSpec] = optRootSpec): ProcessorFactory =
      new ProcessorFactory(optRootSpec, schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables, Some(sset))

  lazy val sset: SchemaSet =
    optSchemaSet.getOrElse(
      SchemaSet(optRootSpec, schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables))

  lazy val rootView: RootView = sset.root

  def elementBaseInstanceCount = sset.elementBaseInstanceCount

  def diagnostics = sset.diagnostics
  def getDiagnostics = diagnostics

  override def onPath(xpath: String): DFDL.DataProcessor = sset.onPath(xpath)

  override def forLanguage(language: String): DFDL.CodeGenerator = {
    Assert.usage(!isError)

    // Do a poor man's pluggable code generator implementation - we can replace
    // it after we observe how the validator SPI evolves and wait for our
    // requirements to become clearer
    val className = language match {
      case "c" => "org.apache.daffodil.runtime2.CodeGenerator"
      case _ => throw new InvalidParserException(s"code generator; source language $language is not supported")
    }
    import scala.language.existentials // Needed to make next line compile
    val clazz = Try(Class.forName(className))
    val constructor = clazz.map { _.getDeclaredConstructor(sset.root.getClass) }
    val tryInstance = constructor.map { _.newInstance(sset.root).asInstanceOf[DFDL.CodeGenerator] }
    val codeGenerator = tryInstance.recover {
      case ex => throw new InvalidParserException(s"Error creating $className", ex)
    }.get

    codeGenerator
  }

  override def isError = sset.isError

  def withDistinguishedRootNode(name: String, namespace: String) : ProcessorFactory = {
    Assert.usage(name ne null)
    copy(optRootSpec = RootSpec.makeRootSpec(Option(name), Option(namespace)))
  }
}

class InvalidParserException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

class Compiler private (val validateDFDLSchemas: Boolean,
  val tunables : DaffodilTunables,

  /**
   * checkAllTopLevel should normally be true. There are some schemas where
   * it must be false. Those are schemas where there are top-level elements that
   * are unused (when certain roots are selected) which have "up and out" relative paths.
   *
   * That sort of element isn't ever intended to be a root, it's intended to be
   * used by way of an element reference within a context that makes the relative path
   * meaningful.
   *
   * Compiling a schema with that sort of element in it and compileAllTopLevel true
   * causes an SDE about "relative path past root".
   */
  private val checkAllTopLevel : Boolean,
  private val optRootName: Option[String],
  private val optRootNamespace: Option[String])
  extends DFDL.Compiler {

  private def this(validateDFDLSchemas: Boolean = true) =
    this(validateDFDLSchemas,
      tunables = DaffodilTunables(),
      checkAllTopLevel = true,
      optRootName = None,
      optRootNamespace = None)

  private def copy(validateDFDLSchemas: Boolean = validateDFDLSchemas,
    tunables : DaffodilTunables = tunables,
    checkAllTopLevel : Boolean = checkAllTopLevel,
    optRootName: Option[String] = optRootName,
    optRootNamespace: Option[String] = optRootNamespace) =
    new Compiler(validateDFDLSchemas, tunables, checkAllTopLevel, optRootName, optRootNamespace)

  def withDistinguishedRootNode(name: String, namespace: String) : Compiler = {
    Assert.usage(name ne null)
    copy(optRootName = Option(name), optRootNamespace = Option(namespace))
  }

  def withValidateDFDLSchemas(value: Boolean) = copy(validateDFDLSchemas = value)

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

  def withTunable(tunable: String, value: String): Compiler =
    copy(tunables = tunables.withTunable(tunable, value))

  def withTunables(tunablesArg: Map[String, String]): Compiler =
    copy(tunables = tunables.withTunables(tunablesArg))

  /**
   * Controls whether we check everything in the schema, or just the element
   * we care about (and everything reachable from it.)
   *
   * You need this control, since many of the big TDML test files have many things
   * in them, some of which use unimplemented features. Each time we run exactly one
   * test from the set, we want to ignore errors in compilation of the others.
   */
  def withCheckAllTopLevel(flag: Boolean): Compiler =
    copy(checkAllTopLevel = flag)

  def reload(savedParser: File) = reload(new FileInputStream(savedParser))

  def reload(savedParser: java.nio.channels.ReadableByteChannel): DFDL.DataProcessor =
     reload(Channels.newInputStream(savedParser))

  def reload(schemaSource: DaffodilSchemaSource): DFDL.DataProcessor =
    reload(schemaSource.uriForLoading)

  def reload(uri: URI): DFDL.DataProcessor = reload(uri.toURL.openStream())

  def reload(is: java.io.InputStream): DFDL.DataProcessor = {
    try {
      // Read the required prefix and version information for this saved parser
      // directly from the input stream. This information is not compressed or
      // serialized by a Java ObjectOutputStream

      val requiredDataPrefix = "DAFFODIL "
      requiredDataPrefix.foreach { c =>
        if (is.read() != c.toInt) throw new InvalidParserException("The saved parser is only compatible with an older version of Daffodil")
      }

      val ab = new ArrayBuffer[Byte]()
      var byte = -1
      while ({ byte = is.read(); byte > 0 }) {
        ab.append(byte.toByte)
      }
      if (byte == -1) {
        throw new InvalidParserException("The saved parser is corrupted")
      }
      val curVersion = Misc.getDaffodilVersion
      val savedVersion = new String(ab.toArray, "utf-8")
      if (savedVersion != curVersion) {
        throw new InvalidParserException("The saved parser is only compatible with Daffodil " + savedVersion + ". Current version is " + curVersion)
      }

      // Decompress and deserilize the rest of the file using java object deserializtion

      val objInput = new ObjectInputStream(new GZIPInputStream(is)) {

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
        throw new InvalidParserException("The saved parser is corrupted")
      }
      case ex: StreamCorruptedException => {
        throw new InvalidParserException("The saved parser is corrupted")
      }
      case ex: InvalidClassException => {
        // This should only happen if users saves a schema with one version of
        // dependency and tries to reload with a different version that is not
        // serialization-compatible (e.g. save with scala 2.12.6 but reload
        // with scala 2.12.11). We don't really know which versions of
        // dependencies maintain serialization compatibility, and we it would
        // be nice to allow users to upgrade libraries themselves for security
        // purposes. So locking Daffodil to specific versions isn't ideal. If
        // this exception is thrown, try to figure out which jar the
        // incompatible class came from and point the user towards that to help
        // figure out the issue.
        val cls = Class.forName(ex.classname)
        val src = cls.getProtectionDomain.getCodeSource
        val dependencyStr = if (src != null) (new File(src.getLocation().getFile)).getName else "a dependency"
        throw new InvalidParserException("The saved parser was created with a different version of " + dependencyStr + " with incompatible class: " + ex.classname)
      }
      //
      case ex @ (_: ClassNotFoundException | _: NoClassDefFoundError) => {
        // Both of these exception happens if a class that was used when saving
        // is no longer on the classpath when reloading.
        //
        // One example of this happening is saving a schema using Java 8 but
        // then reloading on Java 7, since some features (like base64 layers)
        // rely on classes only available in Java 8.
        //
        // Another likely cause is if a user just has their classpath all wrong
        // when reloading a schema, and dependencies are just missing, or if a
        // user switches depenency versions and the new version completely
        // removes a class.
        throw new InvalidParserException("The saved parser was created with a different set of dependencies containing a class no longer on the classpath: " + ex.getMessage)
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
        rootName, rootNamespace, schemaSource, validateDFDLSchemas, checkAllTopLevel, tunables)
    }

    val err = pf.isError
    val diags = pf.getDiagnostics // might be warnings even if not isError
    if (err) {
      Assert.invariant(diags.nonEmpty)
      Logger.log.debug(s"Compilation (ProcessorFactory) produced ${diags.length} errors/warnings.")
    } else {
      if (diags.nonEmpty) {
        Logger.log.debug(s"Compilation (ProcessorFactory) produced ${diags.length} warnings.")
      } else {
        Logger.log.debug(s"ProcessorFactory completed with no errors.")
      }
    }
    Logger.log.debug(s"Schema had ${pf.elementBaseInstanceCount} elements.")
    pf
  }

  /**
   * For convenient unit testing allow a literal XML node.
   */
  def compileNode(
    xml: Node,
    optTmpDir: Option[File] = None,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None): ProcessorFactory = {
    compileSource(UnitTestSchemaSource(xml, "anon", optTmpDir), optRootName, optRootNamespace)
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
