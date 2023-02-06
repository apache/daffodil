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
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.io.Source

object FixImports extends App {
  val inline = true
  val importMap = Map(
    "daffodil-cli" -> "cli",
    "daffodil-core" -> "core",
    "daffodil-io" -> "io",
    "daffodil-lib" -> "lib",
    "daffodil-macro-lib" -> "lib",
    "daffodil-runtime1" -> "runtime1",
    "daffodil-runtime1-layers" -> "layers.runtime1",
    "daffodil-runtime1-unparser" -> "unparsers.runtime1",
    "daffodil-tdml-processor" -> "processor.tdml"
  )
  val removeSuffix = Map(
    "daffodil-runtime1-layers" -> "layers",
    "daffodil-runtime1-unparser" -> "processors.unparsers",
    "daffodil-tdml-processor" -> "tdml.processor",
  )

  def loadSymbolTable(file: File): Map[String, String] = {
    val source = Source.fromFile(file)
    val symbols = Map.empty[String, String]
    for (line <- source.getLines()) {
      val parts = line.split(":")
      val libName = parts(0)
      var packageName = parts(1)
      symbols += packageName -> libName
      symbols += s"${packageName}._" -> libName
    }
    symbols ++= Map(
      "org.apache.daffodil.Main.ExitCode" -> "daffodil-cli",
      "org.apache.daffodil.CLI.Util._" -> "daffodil-cli",
      "org.apache.daffodil.compiler._" -> "daffodil-core",
      "org.apache.daffodil.dsom.walker._" -> "daffodil-core",
      "org.apache.daffodil.equality._" -> "daffodil-lib",
      "org.apache.daffodil.equality.TypeEqual" -> "daffodil-lib",
      "org.apache.daffodil.equality.ViewEqual" -> "daffodil-lib",
      "org.apache.daffodil.exceptions._" -> "daffodil-lib",
      "org.apache.daffodil.Implicits.intercept" -> "daffodil-lib",
      "org.apache.daffodil.infoset._" -> "daffodil-runtime1",
      "org.apache.daffodil.oolag.OOLAG.OOLAGHost" -> "daffodil-lib",
      "org.apache.daffodil.oolag.OOLAG.OOLAGHostImpl" -> "daffodil-lib",
      "org.apache.daffodil.processors._" -> "daffodil-runtime1",
      "org.apache.daffodil.processors.dfa" -> "daffodil-runtime1",
      "org.apache.daffodil.processors.parsers._" -> "daffodil-runtime1",
      "org.apache.daffodil.schema.annotation.props._" -> "daffodil-lib",
      "org.apache.daffodil.schema.annotation.props.gen._" -> "daffodil-lib",
      "org.apache.daffodil.util._" -> "daffodil-lib",
      "org.apache.daffodil.util.Implicits.using" -> "daffodil-lib",
      "org.apache.daffodil.util.Maybe.Nope" -> "daffodil-lib",
      "org.apache.daffodil.util.Maybe.One" -> "daffodil-lib",
      "org.apache.daffodil.util.Misc.hex2Bits" -> "daffodil-lib",
      "org.apache.daffodil.util.Misc.bits2Bytes" -> "daffodil-lib",
      "org.apache.daffodil.util.Numbers._" -> "daffodil-lib",
      "org.apache.daffodil.validation.XercesValidator.XercesValidatorImpl" -> "daffodil-lib",
      "org.apache.daffodil.xml._" -> "daffodil-lib",
      "org.apache.daffodil.xml.NS.implicitNStoString" -> "daffodil-lib",
      "org.apache.daffodil.xml.QNameRegex" -> "daffodil-lib",
    )
    symbols
  }

  def updateName(libName: String, packageNameParam: String): String = {
    var packageName = packageNameParam
    importMap.get(libName) match {
      case Some(sub) => {
        removeSuffix.get(libName) match {
          case Some(toRemove) => packageName = packageName.replace(s".$toRemove", "")
          case None =>
        }
        val newPrefix = s"org.apache.daffodil.$sub"
        if (packageName.startsWith(newPrefix)) packageName
        else packageName.replace("org.apache.daffodil", newPrefix)
      }
      case None => packageName
    }
  }

  def fixPackage(libName: String, packageName: String): String = {
    updateName(libName, packageName)
  }

  def fixImport(name: String, symbols: Map[String, String]): String = {
    symbols.get(name) match {
      case Some(libName) => updateName(libName, name)
      case None => name
    }
  }

  val reImports = """^ ?import (.+)$""".r
  val rePackage = """^ ?package (.+)$""".r

  def fixLine(libName: String, line: String, symbols: Map[String,String]) = {
      line match {
        case rePackage(name) => s"package ${fixPackage(libName, name)}"
        case reImports(name) => s"import ${fixImport(name, symbols)}"
        case _ => line
      }
  }

  val replacements = Seq(
    ("new org.apache.daffodil.xml", "new org.apache.daffodil.lib.xml"),
    ("org.apache.daffodil.schema.annotation.props.gen.{ BinaryNumberCheckPolicy, TextZonedSignStyle }", "org.apache.daffodil.lib.schema.annotation.props.gen.{ BinaryNumberCheckPolicy, TextZonedSignStyle }"),
    ("import org.apache.daffodil.util.MaybeULong", "import org.apache.daffodil.lib.util.MaybeULong"),
    ("import org.apache.daffodil.Implicits._", "import org.apache.daffodil.lib.Implicits._"),
    ("import org.apache.daffodil.infoset.DataValue", "import org.apache.daffodil.runtime1.infoset.DataValue"),
    ("org.apache.daffodil.api", "org.apache.daffodil.lib.api"),
    ("org.apache.daffodil.calendar", "org.apache.daffodil.lib.calendar"),
    ("org.apache.daffodil.dpath.NodeInfo", "org.apache.daffodil.runtime1.dpath.NodeInfo"),
    ("org.apache.daffodil.dsom", "org.apache.daffodil.runtime1.dsom"),
    ("org.apache.daffodil.equality", "org.apache.daffodil.lib.equality"),
    ("org.apache.daffodil.exceptions", "org.apache.daffodil.lib.exceptions"),
    ("org.apache.daffodil.infoset", "org.apache.daffodil.runtime1.infoset"),
    ("org.apache.daffodil.Implicits", "org.apache.daffodil.lib.Implicits"),
    ("org.apache.daffodil.processors", "org.apache.daffodil.runtime1.processors"),
    ("org.apache.daffodil.schema", "org.apache.daffodil.lib.schema"),
    ("org.apache.daffodil.xml", "org.apache.daffodil.lib.xml"),
    ("org.apache.daffodil.udf.UserDefinedFunctionService", "org.apache.daffodil.runtime1.udf.UserDefinedFunctionService"),
    ("org.apache.daffodil.util", "org.apache.daffodil.lib.util"),
    ("org.apache.daffodil.grammar", "org.apache.daffodil.core.grammar"),
    ("org.apache.daffodil.lib.api.DFDL", "org.apache.daffodil.runtime1.api.DFDL"),
    ("import org.apache.daffodil.runtime1.dsom.{ChoiceTermBase, GroupRef, InitiatedTerminatedMixin, ModelGroup, SequenceTermBase}", "import org.apache.daffodil.core.dsom.{ChoiceTermBase, GroupRef, InitiatedTerminatedMixin, ModelGroup, SequenceTermBase}"),
    ("org.apache.daffodil.compiler", "org.apache.daffodil.core.compiler"),
    ("import org.apache.daffodil.debugger", "import org.apache.daffodil.runtime1.debugger"),
    ("org.apache.daffodil.lib.util.TestUtils", "org.apache.daffodil.core.util.TestUtils"),
    ("org.apache.daffodil.lib.util.Fakes", "org.apache.daffodil.core.util.Fakes"),
    ("org.apache.daffodil.runtime1.dsom.{ ElementBase, Root }", "org.apache.daffodil.core.dsom.{ ElementBase, Root }"),
    ("org.apache.daffodil.lib.util.StreamParser", "org.apache.daffodil.core.util.StreamParser"),
    ("org.apache.daffodil.cli.CLI.Util._", "org.apache.daffodil.cli.cliTest.Util._"),
    ("package org.apache.daffodil.cli.CLI", "package org.apache.daffodil.cli.cliTest"),
    ("case TDMLImplementation.Daffodil => \"org.apache.daffodil.tdml.processor.TDMLDFDLProcessorFactory\"", "case TDMLImplementation.Daffodil => \"org.apache.daffodil.processor.tdml.TDMLDFDLProcessorFactory\""),
    ("case TDMLImplementation.DaffodilC => \"org.apache.daffodil.tdml.processor.Runtime2TDMLDFDLProcessorFactory\"", "case TDMLImplementation.DaffodilC => \"org.apache.daffodil.processor.tdml.Runtime2TDMLDFDLProcessorFactory\""),
    ("case TDMLImplementation.Ibm => \"org.apache.daffodil.tdml.processor.TDMLDFDLProcessorFactory\"", "case TDMLImplementation.Ibm => \"org.apache.daffodil.processor.tdml.TDMLDFDLProcessorFactory\""),
    ("private[tdml] def getTS = this.synchronized {", "def getTS = this.synchronized {"),
    ("package org.apache.daffodil.processor.tdml.tdml;", "package org.apache.daffodil.processor.tdml;")
  )

  def applyReplacements(line: String): String = {
    var applied = line
    for (replacement <- replacements) {
      val (from, to) = replacement
      applied = applied.replace(from, to)
    }
    applied
  }

  def fixFile(libNameParam: String, file: File, symbols: Map[String, String]): Unit = {
    val path = file.getPath
    if (path.endsWith(".refactor.scala")) return
    if (path.contains("propGen")) return
    if (file.getName == "PointOfUncertaintyMacros.scala") return
    val libName = if (file.getName == "IOMacros.scala") "io" else libNameParam
    val source = Source.fromFile(file)
    val fixed = ListBuffer.empty[String]
    for (line <- source.getLines()) {
      fixed += applyReplacements(fixLine(libName, line, symbols))
    }
    source.close

    val outName = if (inline) file.getPath() else file.getPath().replace(".scala", ".refactor.scala")
    val out = new BufferedWriter(new FileWriter(outName))
    for (line <- fixed) out.write(line + "\n")
    out.close
  }

  def processDir(libName: String, path: File, symbols: Map[String,String]): Unit = {
    for (file <- path.listFiles) {
      if (file.isDirectory) processDir(libName, file, symbols)
      if (file.getName.endsWith(".scala") || file.getName.endsWith(".java")) fixFile(libName, file, symbols)
    }
  }

  def processLib(libPath: String, symbols: Map[String,String]) = {
    var path = new File(libPath)
    val libName = path.getName
    processDir(libName, path, symbols)
  }

  val symbolFile = new File(args(0))
  val symbols = loadSymbolTable(symbolFile)

  var libPaths = args.drop(1)
  for (libPath <- libPaths) {
    processLib(libPath, symbols)
  }
}
