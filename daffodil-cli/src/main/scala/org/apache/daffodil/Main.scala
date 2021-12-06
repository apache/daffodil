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

package org.apache.daffodil

import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.net.URI
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.util.Scanner
import java.util.concurrent.Executors

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.matching.Regex

import scala.xml.Node
import scala.xml.SAXParser

import com.typesafe.config.ConfigFactory

import org.rogach.scallop
import org.rogach.scallop.ArgType
import org.rogach.scallop.ScallopOption
import org.rogach.scallop.ValueConverter

import org.xml.sax.XMLReader

import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator

import org.apache.commons.io.IOUtils
import org.apache.commons.io.output.NullOutputStream
import org.apache.daffodil.Main.ExitCode
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DFDL.DaffodilUnparseErrorSAXException
import org.apache.daffodil.api.DFDL.ParseResult
import org.apache.daffodil.api.DFDL.UnparseResult
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.api.WithDiagnostics
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.compiler.InvalidParserException
import org.apache.daffodil.debugger.CLIDebuggerRunner
import org.apache.daffodil.debugger.DebuggerExitException
import org.apache.daffodil.debugger.InteractiveDebugger
import org.apache.daffodil.debugger.TraceDebuggerRunner
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.NotYetImplementedException
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.externalvars.BindingException
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.infoset.InfosetInputter
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.infoset.JDOMInfosetInputter
import org.apache.daffodil.infoset.JDOMInfosetOutputter
import org.apache.daffodil.infoset.JsonInfosetInputter
import org.apache.daffodil.infoset.JsonInfosetOutputter
import org.apache.daffodil.infoset.NullInfosetOutputter
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.infoset.W3CDOMInfosetInputter
import org.apache.daffodil.infoset.W3CDOMInfosetOutputter
import org.apache.daffodil.infoset.XMLTextInfosetInputter
import org.apache.daffodil.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.io.DataDumper
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.processors.DaffodilParseOutputStreamContentHandler
import org.apache.daffodil.processors.DataLoc
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.ExternalVariableException
import org.apache.daffodil.processors.HasSetDebugger
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.TDMLException
import org.apache.daffodil.tdml.TDMLTestNotCompatibleException
import org.apache.daffodil.udf.UserDefinedFunctionFatalErrorException
import org.apache.daffodil.util.Logger
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Timer
import org.apache.daffodil.validation.Validators
import org.apache.daffodil.xml.DaffodilSAXParserFactory
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.apache.daffodil.xml.QName
import org.apache.daffodil.xml.RefQName
import org.apache.daffodil.xml.XMLUtils

object InfosetType extends Enumeration {
  type Type = Value

  val JDOM = Value("jdom")
  val JSON = Value("json")
  val SAX = Value("sax")
  val SCALA_XML = Value("scala-xml")
  val W3CDOM = Value("w3cdom")
  val XML = Value("xml")
  val NULL = Value("null")
}

class CLIConf(arguments: Array[String]) extends scallop.ScallopConf(arguments) {

  /**
   * This is used when the flag is optional and so is its
   * argument.
   *
   * Let's use --debug [file] as an example.
   *
   * This optionalValueConverter first determines if the
   * --debug flag was given.  If it wasn't, Right(None)
   * is returned.
   *
   * If the flag was given we then need to determine
   * if 'file' was also given.  If file was not given, Right(Some(None))
   * is returned meaning that the --debug flag was there but 'file'
   * was not.  If the file was given, Right(Some(Some(conv(file)))) is
   * returned.  This means that the --debug flag was there as well as
   * a filename for 'file'.
   *
   * conv(mode) simply performs the necessary conversion
   * of the string to the type [A].
   *
   */
  def optionalValueConverter[A](conv: String => A): scallop.ValueConverter[Option[A]] =
    new scallop.ValueConverter[Option[A]] {

      // From the Scallop wiki:
      //
      // parse is a method, that takes a list of arguments to all option invocations:
      // for example, "-a 1 2 -a 3 4 5" would produce List(List(1,2),List(3,4,5)).
      // parse returns Left with error message, if there was an error while parsing
      // if no option was found, it returns Right(None)
      // and if option was found, it returns Right(...)
      def parse(s: List[(String, List[String])]): Either[String, Option[Option[A]]] = {
        s match {
          case Nil => Right(None) // flag was not present
          case (_, Nil) :: Nil => Right(Some(None)) // flag was present but had no parameter
          case (_, v :: Nil) :: Nil => { // flag was present with a parameter, convert the parameter
            try {
              Right(Some(Some(conv(v))))
            } catch {
              case s: scala.util.control.ControlThrowable => throw s
              case u: UnsuppressableException => throw u
              case e: Exception => {
                Left(e.getMessage())
              }
            }
          }
          case _ => Left("you should provide no more than one argument for this option") // Error because we expect there to be at most one flag
        }
      }
      val argType = scallop.ArgType.LIST
      override def argFormat(name: String): String = "[" + name + "]"
    }

  implicit def validateConverter = singleArgConverter[ValidationMode.Type]((s: String) => {
    import ValidatorPatterns._
    s match {
      case "on" => ValidationMode.Full
      case "limited" => ValidationMode.Limited
      case "off" => ValidationMode.Off
      case DefaultArgPattern(name, arg) if Validators.isRegistered(name) =>
        val config = if(arg.endsWith(".conf")) ConfigFactory.parseFile(new File(arg)) else ConfigFactory.parseString(s"$name=$arg")
        ValidationMode.Custom(Validators.get(name).make(config))
      case NoArgsPattern(name) if Validators.isRegistered(name) =>
        ValidationMode.Custom(Validators.get(name).make(ConfigFactory.empty))
      case _ => throw new Exception("Unrecognized ValidationMode %s.  Must be 'on', 'limited', 'off', or name of spi validator.".format(s))
    }
  })

  implicit def infosetTypeConverter = singleArgConverter[InfosetType.Type]((s: String) => {
    try {
      InfosetType.withName(s.toLowerCase)
    } catch {
      case _: NoSuchElementException => throw new Exception("Unrecognized infoset type: %s.  Must be one of %s".format(s, InfosetType.values.mkString(", ")))
    }
  })

  def qnameConvert(s: String): RefQName = {
    val eQN = QName.refQNameFromExtendedSyntax(s)
    eQN.get
  }

  def singleArgConverter[A](conv: String => A) = new ValueConverter[A] {
    def parse(s: List[(String, List[String])]) = {
      s match {
        case (_, i :: Nil) :: Nil =>
          try {
            Right(Some(conv(i)))
          } catch {
            case s: scala.util.control.ControlThrowable => throw s
            case u: UnsuppressableException => throw u
            case e: Throwable => Left(e.getMessage())
          }
        case Nil => Right(None)
        case _ => Left("you should provide exactly one argument for this option")
      }
    }
    val argType = ArgType.SINGLE
  }

  implicit def rootNSConverter = org.rogach.scallop.singleArgConverter[RefQName](qnameConvert _)

  implicit def fileResourceURIConverter = singleArgConverter[URI]((s: String) => {
    val file = new File(s)
    val uri =
      if (file.isFile()) {
        Some(file.toURI)
      } else {
        Misc.getResourceRelativeOption(s, None)
      }
    uri.getOrElse(throw new Exception("Could not find file or resource %s" format s))
  })

  printedName = "Apache Daffodil"

  val width = 87
  helpWidth(width)

  def error(msg: String) = errorMessageHandler(msg)

  errorMessageHandler = { message =>
    val msg =
      if (message.indexOf("Wrong format for option 'schema'") >= 0) {
        // the 'wrong format' error only occurs on --schema when options are
        // provided after the trailing arg, so let's give a more helpful error
        // message
        "Options are not allowed after a trailing argument"
      } else {
        message
      }

    Logger.log.error(msg)
    sys.exit(ExitCode.Usage.id)
  }

  banner("""|Usage: daffodil [GLOBAL_OPTS] <subcommand> [SUBCOMMAND_OPTS]
            |
            |Global Options:""".stripMargin)

  footer("""|
            |Run 'daffodil <subcommand> --help' for subcommand specific options""".stripMargin)

  version({
    val version = Misc.getDaffodilVersion
    val strVers = "%s %s".format(printedName, version)
    strVers
  })

  shortSubcommandsHelp()

  // Global Options
  val debug = opt[Option[String]](argName = "file", descr = "Enable the interactive debugger. Optionally, read initial debugger commands from [file] if provided.")(optionalValueConverter[String](a => a))
  val trace = opt[Boolean](descr = "Run this program with verbose trace output")
  val verbose = tally(descr = "Increment verbosity level, one level for each -v")
  val version = opt[Boolean](descr = "Show Daffodil's version")

  // Parse Subcommand Options
  object parse extends scallop.Subcommand("parse") {
    banner("""|Usage: daffodil parse (-s <schema> [-r <root>] | -P <parser>)
              |                      [-c <file>] [-D<variable>=<value>...] [-I <infoset_type>]
              |                      [-o <output>] [--stream] [-T<tunable>=<value>...] [-V <mode>]
              |                      [infile]
              |
              |Parse a file, using either a DFDL schema or a saved parser
              |
              |Parse Options:""".stripMargin)

    descr("Parse data to a DFDL infoset")
    helpWidth(width)

    val config = opt[File](short = 'c', argName = "file", descr = "XML file containing configuration items")
    val vars = props[String](name = 'D', keyName = "variable", valueName = "value", descr = "Variables to be used when parsing. Can be prefixed with {namespace}.")
    val infosetType = opt[InfosetType.Type](short = 'I', argName = "infoset_type", descr = "Infoset type to output. Use 'xml', 'scala-xml', 'json', 'jdom', 'w3cdom', 'sax', or 'null'. Defaults to 'xml'.", default = Some(InfosetType.XML))
    val output = opt[String](argName = "file", descr = "Output file to write infoset to. If not given or is -, infoset is written to stdout.")
    val parser = opt[File](short = 'P', argName = "file", descr = "Previously saved parser to reuse")
    val path = opt[String](argName = "path", descr = "Path from root element to node from which to start parsing", hidden = true)
    val rootNS = opt[RefQName]("root", argName = "node", descr = "Root element to use. Can be prefixed with {namespace}. Must be a top-level element. Defaults to first top-level element of DFDL schema.")
    val schema = opt[URI]("schema", argName = "file", descr = "DFDL schema to use to create parser")(fileResourceURIConverter)
    val stream = toggle(noshort = true, default = Some(false), descrYes = "When left over data exists, parse again with remaining data, separating infosets by a NUL character", descrNo = "Stop after the first parse, throwing an error if left over data exists")
    val tunables = props[String](name = 'T', keyName = "tunable", valueName = "value", descr = "Tunable configuration options to change Daffodil's behavior")
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "Validation mode. Use 'on', 'limited', 'off', or a validator plugin name.")

    val infile = trailArg[String](required = false, descr = "Input file to parse. If not specified, or a value of -, reads from stdin.")

    requireOne(schema, parser) // must have one of --schema or --parser
    conflicts(parser, List(rootNS)) // if --parser is provided, cannot also provide --root
    validateFileIsFile(config) // --config must be a file that exists

    validateOpt(debug, infile) {
      case (Some(_), Some("-")) | (Some(_), None) => Left("Input must not be stdin during interactive debugging")
      case _ => Right(Unit)
    }

    validateOpt(parser, validate) {
      case (Some(_), Some(ValidationMode.Full)) => Left("The validation mode must be 'limited' or 'off' when using a saved parser.")
      case _ => Right(Unit)
    }

  }

  // Unparse Subcommand Options
  object unparse extends scallop.Subcommand("unparse") {
    banner("""|Usage: daffodil unparse (-s <schema> [-r <root>] | -P <parser>)
              |                        [-c <file>] [-D<variable>=<value>...] [-I <infoset_type>]
              |                        [-o <output>] [--stream] [-T<tunable>=<value>...] [-V <mode>]
              |                        [infile]
              |
              |Unparse an infoset file, using either a DFDL schema or a saved parser
              |
              |Unparse Options:""".stripMargin)

    descr("Unparse a DFDL infoset")
    helpWidth(width)

    val config = opt[File](short = 'c', argName = "file", descr = "XML file containing configuration items")
    val vars = props[String](name = 'D', keyName = "variable", valueName = "value", descr = "Variables to be used when parsing. Can be prefixed with {namespace}.")
    val infosetType = opt[InfosetType.Type](short = 'I', argName = "infoset_type", descr = "Infoset type to unparse. Use 'xml', 'scala-xml', 'json', 'jdom', 'w3cdom', or 'sax'. Defaults to 'xml'.", default = Some(InfosetType.XML))
    val output = opt[String](argName = "file", descr = "Output file to write data to. If not given or is -, data is written to stdout.")
    val parser = opt[File](short = 'P', argName = "file", descr = "Previously saved parser to reuse")
    val path = opt[String](argName = "path", descr = "Path from root element to node from which to start unparsing", hidden = true)
    val rootNS = opt[RefQName]("root", argName = "node", descr = "Root element to use. Can be prefixed with {namespace}. Must be a top-level element. Defaults to first top-level element of DFDL schema.")
    val schema = opt[URI]("schema", argName = "file", descr = "DFDL schema to use to create parser")(fileResourceURIConverter)
    val stream = toggle(noshort = true, default = Some(false), descrYes = "Split the input data on the NUL character, and unparse each chuck separately", descrNo = "Treat the entire input data as one infoset")
    val tunables = props[String](name = 'T', keyName = "tunable", valueName = "value", descr = "Tunable configuration options to change Daffodil's behavior")
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "Validation mode. Use 'on', 'limited', 'off', or a validator plugin name.")

    val infile = trailArg[String](required = false, descr = "Input file to unparse. If not specified, or a value of -, reads from stdin.")

    requireOne(schema, parser) // must have one of --schema or --parser
    conflicts(parser, List(rootNS)) // if --parser is provided, cannot also provide --root
    validateFileIsFile(config) // --config must be a file that exists

    validateOpt(debug, infile) {
      case (Some(_), Some("-")) | (Some(_), None) => Left("Input must not be stdin during interactive debugging")
      case _ => Right(Unit)
    }

    validateOpt(infosetType) {
      case (Some(InfosetType.NULL)) => Left("Invalid infoset type: null") // null is not valid for unparsing
      case _ => Right(Unit)
    }
  }

  // Save Parser Subcommand Options
  object save extends scallop.Subcommand("save-parser") {
    banner("""|Usage: daffodil save-parser -s <schema> [-r <root>]
              |                            [-c <file>] [-D<variable>=<value>...] [-T<tunable>=<value>...]
              |                            [outfile]
              |
              |Create and save a parser using a DFDL schema
              |
              |Save Parser Options:""".stripMargin)

    descr("Save a Daffodil parser for reuse")
    helpWidth(width)

    val config = opt[File](short = 'c', argName = "file", descr = "XML file containing configuration items")
    val vars = props[String](name = 'D', keyName = "variable", valueName = "value", descr = "Variables to be used when parsing. Can be prefixed with {namespace}.")
    val path = opt[String](argName = "path", descr = "Path from root element to node from which to start parsing", hidden = true)
    val rootNS = opt[RefQName]("root", argName = "node", descr = "Root element to use. Can be prefixed with {namespace}. Must be a top-level element. Defaults to first top-level element of DFDL schema.")
    val schema = opt[URI]("schema", required = true, argName = "file", descr = "DFDL schema to use to create parser")(fileResourceURIConverter)
    val tunables = props[String](name = 'T', keyName = "tunable", valueName = "value", descr = "Tunable configuration options to change Daffodil's behavior")

    val outfile = trailArg[String](required = false, descr = "Output file to save parser to. If not specified, or a value of -, saves to stdout.")

    requireOne(schema) // --schema must be provided
    validateFileIsFile(config) // --config must be a file that exists
  }

  // Test Subcommand Options
  object test extends scallop.Subcommand("test") {
    banner("""|Usage: daffodil test [-l] [-r] [-i] <tdmlfile> [testnames...]
              |
              |List or execute tests in a TDML file
              |
              |Test Options:""".stripMargin)

    descr("List or execute TDML tests")
    helpWidth(width)

    val info = tally(descr = "Increment test result information output level, one level for each -i")
    val list = opt[Boolean](descr = "Show names and descriptions instead of running test cases")
    val regex = opt[Boolean](descr = "Treat <testnames...> as regular expressions")
    val tdmlfile = trailArg[String](required = true, descr = "Test Data Markup Language (TDML) file")
    val testnames = trailArg[List[String]](required = false, descr = "Name(s) of test cases in tdmlfile. If not given, all tests in tdmlfile are run.")
  }

  // Performance Subcommand Options
  object performance extends scallop.Subcommand("performance") {
    banner("""|Usage: daffodil performance (-s <schema> [-r <root>] | -P <parser>)
              |                            [-c <file>] [-D<variable>=<value>...] [-I <infoset_type>]
              |                            [-N <number>] [-t <threads>] [-T<tunable>=<value>...]
              |                            [-u] [-V <mode>]
              |                            <infile>
              |
              |Run a performance test, using either a DFDL schema or a saved parser
              |
              |Performance Options:""".stripMargin)

    descr("Run performance test")
    helpWidth(width)

    val config = opt[File](short = 'c', argName = "file", descr = "XML file containing configuration items")
    val vars = props[String](name = 'D', keyName = "variable", valueName = "value", descr = "Variables to be used when parsing. Can be prefixed with {namespace}.")
    val infosetType = opt[InfosetType.Type](short = 'I', argName = "infoset_type", descr = "Infoset type to output or unparse. Use 'xml', 'scala-xml', 'json', 'jdom', 'w3cdom', 'sax', or 'null'. Defaults to 'xml'.", default = Some(InfosetType.XML))
    val number = opt[Int](short = 'N', argName = "number", default = Some(1), descr = "Total number of files to process. Defaults to 1.")
    val parser = opt[File](short = 'P', argName = "file", descr = "Previously saved parser to reuse")
    val path = opt[String](argName = "path", descr = "Path from root element to node from which to start parsing or unparsing", hidden = true)
    val rootNS = opt[RefQName]("root", argName = "node", descr = "Root element to use. Can be prefixed with {namespace}. Must be a top-level element. Defaults to first top-level element of DFDL schema.")
    val schema = opt[URI]("schema", argName = "file", descr = "DFDL schema to use to create parser")(fileResourceURIConverter)
    val threads = opt[Int](short = 't', argName = "threads", default = Some(1), descr = "Number of threads to use. Defaults to 1.")
    val tunables = props[String](name = 'T', keyName = "tunable", valueName = "value", descr = "Tunable configuration options to change Daffodil's behavior")
    val unparse = opt[Boolean](default = Some(false), descr = "Perform unparse instead of parse for performance test")
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "Validation mode. Use 'on', 'limited', 'off', or a validator plugin name.")

    val infile = trailArg[String](required = true, descr = "Input file or directory containing input files to parse or unparse")

    requireOne(schema, parser) // must have one of --schema or --parser
    conflicts(parser, List(rootNS)) // if --parser is provided, cannot also provide --root
    validateFileIsFile(config) // --config must be a file that exists

    validateOpt(infosetType, unparse) {
      case (Some(InfosetType.NULL), Some(true)) => Left("null infoset type not valid with performance --unparse")
      case _ => Right(Unit)
    }
  }

  // Generate Subcommand Options
  object generate extends scallop.Subcommand("generate") {
    descr("Generate <language> code from a DFDL schema")

    banner("""|Usage: daffodil [GLOBAL_OPTS] generate <language> [SUBCOMMAND_OPTS]
              |""".stripMargin)
    shortSubcommandsHelp()
    footer("""|
              |Run 'daffodil generate <language> --help' for subcommand specific options""".stripMargin)

    object c extends scallop.Subcommand("c") {
      banner("""|Usage: daffodil generate c -s <schema> [-r <root>]
                |                           [-c <file>] [-T<tunable>=<value>...]
                |                           [outdir]
                |
                |Generate C code from a DFDL schema to parse or unparse data
                |
                |Generate Options:""".stripMargin)

      descr("Generate C code from a DFDL schema")
      helpWidth(width)

      val language = "c"
      val config = opt[File](short = 'c', argName = "file", descr = "XML file containing configuration items")
      val rootNS = opt[RefQName]("root", argName = "node", descr = "Root element to use. Can be prefixed with {namespace}. Must be a top-level element. Defaults to first top-level element of DFDL schema.")
      val schema = opt[URI]("schema", required = true, argName = "file", descr = "DFDL schema to use to create parser")(fileResourceURIConverter)
      val tunables = props[String](name = 'T', keyName = "tunable", valueName = "value", descr = "Tunable configuration options to change Daffodil's behavior")

      val outdir = trailArg[String](required = false, descr = "Output directory in which to create 'c' subdirectory. If not specified, uses current directory.")

      requireOne(schema) // --schema must be provided
      validateFileIsFile(config) // --config must be a file that exists
    }
    addSubcommand(c)
    requireSubcommand()
  }

  addSubcommand(parse)
  addSubcommand(unparse)
  addSubcommand(save)
  addSubcommand(test)
  addSubcommand(performance)
  addSubcommand(generate)

  mutuallyExclusive(trace, debug) // cannot provide both --trace and --debug
  requireSubcommand()

  verify()
}

object ValidatorPatterns {
  val NoArgsPattern: Regex = "(.+?)".r.anchored
  val DefaultArgPattern: Regex = "(.+?)=(.+)".r.anchored
}

object Main {

  val traceCommands = Seq(
    "display info parser",
    "display info data",
    "display info infoset",
    "display info diff",
    "trace")

  /* indents a multi-line string */
  def indent(str: String, pad: Int): String = {
    val lines = str.split("\n")
    val prefix = " " * pad
    val indented = lines.map(prefix + _)
    indented.mkString("\n")
  }

  /**
   * Loads and validates the configuration file.
   *
   * @param file The path to the file.
   * @return The Node representation of the file.
   */
  def loadConfigurationFile(file: File) = {
    val loader = new DaffodilXMLLoader()
    val node = loader.load(URISchemaSource(file.toURI), Some(XMLUtils.dafextURI))
    node
  }

  /**
   * Overrides bindings specified via the configuration file with those
   * specified via the -D command.
   *
   * @param bindings A sequence of Bindings (external variables)
   * @param bindingsToOverride The sequence of Bindings that could be overridden.
   */
  def overrideBindings(bindings: Seq[Binding], bindingsToOverride: Seq[Binding]) = {
    val inBoth = bindings.intersect(bindingsToOverride).distinct
    val bindingsMinusBoth = bindings.diff(inBoth)
    val bindingsToOverrideMinusBoth = bindingsToOverride.diff(inBoth)
    val bindingsWithCorrectValues = bindings.filter(b => inBoth.exists(p => b.hashCode == p.hashCode))

    val bindingsMinusUpdates = bindingsMinusBoth.union(bindingsToOverrideMinusBoth)
    val bindingsWithUpdates = bindingsMinusUpdates.union(bindingsWithCorrectValues)

    bindingsWithUpdates
  }

  def displayDiagnostics(pr: WithDiagnostics): Unit = {
    pr.getDiagnostics.foreach { d =>
      if (d.isError) {
        Logger.log.error(d.getMessage())
      } else {
        Logger.log.warn(d.getMessage())
      }
    }
  }

  /**
   * Retrieves all external variables specified via the command line interface.
   *
   * @param vars The individual variables input via the command line using the -D command.
   * @param configFileNode The Node representing the configuration file if there is one.
   */
  def retrieveExternalVariables(vars: Map[String, String], configFileNode: Option[Node]): Seq[Binding] = {
    val configFileVars: Seq[Binding] = configFileNode match {
      case None => Seq.empty
      case Some(configNode) => {
        // We have a configuration file node, we now need to grab
        // the externalVariableBindings node.
        val extVarBindingNodeOpt = (configNode \ "externalVariableBindings").headOption
        extVarBindingNodeOpt match {
          case None => Seq.empty
          case Some(extVarBindingsNode) => {
            Binding.getBindings(extVarBindingsNode)
          }
        }
      }
    }

    val individualVars = ExternalVariablesLoader.mapToBindings(vars)

    val bindings = overrideBindings(individualVars, configFileVars)
    bindings
  }

  def createProcessorFromParser(savedParser: File, path: Option[String], mode: ValidationMode.Type) = {
    try {
      val compiler = Compiler()
      val processor = Timer.getResult("reloading", compiler.reload(savedParser))
      displayDiagnostics(processor)
      if (!processor.isError) {
        Some(processor.withValidationMode(mode))
      } else {
        None
      }
    } catch {
      case e: InvalidParserException => {
        Logger.log.error(e.getMessage())
        None
      }
    }
  }

  def retrieveTunables(tunables: Map[String, String], configFileNode: Option[Node]) = {
    val configFileTunables: Map[String, String] = configFileNode match {
      case None => Map.empty
      case Some(configNode) => {
        val tunablesOpt = (configNode \ "tunables").headOption
        tunablesOpt match {
          case None => Map.empty
          case Some(tunableNode) => {
            tunableNode.child.map { n => (n.label, n.text) }.toMap
          }
        }
      }
    }

    // Note, ++ on Maps replaces any key/value pair from the left with that on the
    // right, so key/value pairs defined in tunables overrule those defiend in
    // the config file
    val combined = configFileTunables ++ tunables
    combined
  }

  def setupDebugOrTrace(proc: HasSetDebugger, conf: CLIConf) = {
    if (conf.trace() || conf.debug.isDefined) {
      val runner =
        if (conf.trace()) {
          new TraceDebuggerRunner
        } else {
          if (System.console == null) {
            Logger.log.warn(s"Using --debug on a non-interactive console may result in display issues")
          }
          conf.debug() match {
            case Some(f) => new CLIDebuggerRunner(new File(f))
            case None => new CLIDebuggerRunner()
          }
        }
      val id = new InteractiveDebugger(runner, ExpressionCompilers)
      proc.setDebugging(true)
      proc.setDebugger(id)
    }
  }

  def createProcessorFromSchema(schema: URI, rootNS: Option[RefQName], path: Option[String],
    tunables: Map[String, String],
    mode: ValidationMode.Type): Option[DFDL.DataProcessor] = {
    val compiler = {
      val c = Compiler().withTunables(tunables)
      rootNS match {
        case None => c
        case Some(RefQName(_, root, ns)) => c.withDistinguishedRootNode(root, ns.toStringOrNullIfNoNS)
      }
    }

    // Wrap timing around the whole of compilation
    //
    // compilation extends from the call to compile
    // to also include the call to pf.onPath. (which is the last phase
    // of compilation, where it asks for the parser)
    //
    val schemaSource = URISchemaSource(schema)
    val res = Timer.getResult("compiling", {
      val processorFactory = compiler.compileSource(schemaSource)
      if (!processorFactory.isError) {
        val processor = processorFactory.onPath(path.getOrElse("/")).withValidationMode(mode)
        displayDiagnostics(processor)
        if (processor.isError) {
          None
        } else {
          Some(processor)
        }
      } else {
        displayDiagnostics(processorFactory)
        None
      }
    })
    res
  }

  def createGeneratorFromSchema(schema: URI, rootNS: Option[RefQName], tunables: Map[String, String],
                                language: String): Option[DFDL.CodeGenerator] = {
    val compiler = {
      val c = Compiler().withTunables(tunables)
      rootNS match {
        case None => c
        case Some(RefQName(_, root, ns)) => c.withDistinguishedRootNode(root, ns.toStringOrNullIfNoNS)
      }
    }

    val schemaSource = URISchemaSource(schema)
    val cg = Timer.getResult("compiling", {
      val processorFactory = compiler.compileSource(schemaSource)
      if (!processorFactory.isError) {
        val generator = processorFactory.forLanguage(language)
        displayDiagnostics(generator)
        Some(generator)
      } else {
        displayDiagnostics(processorFactory)
        None
      }
    })
    cg
  }

  // write blobs to $PWD/daffodil-blobs/*.bin
  val blobDir = Paths.get(System.getProperty("user.dir"), "daffodil-blobs")
  val blobSuffix = ".bin"

  def getInfosetOutputter(infosetType: InfosetType.Type, os: java.io.OutputStream)
  : Either[InfosetOutputter, DaffodilParseOutputStreamContentHandler] = {
    val outputter = infosetType match {
      case InfosetType.XML => Left(new XMLTextInfosetOutputter(os, pretty = true))
      case InfosetType.SCALA_XML => Left(new ScalaXMLInfosetOutputter())
      case InfosetType.JSON => Left(new JsonInfosetOutputter(os, pretty = true))
      case InfosetType.JDOM => Left(new JDOMInfosetOutputter())
      case InfosetType.W3CDOM => Left(new W3CDOMInfosetOutputter())
      case InfosetType.SAX => Right(new DaffodilParseOutputStreamContentHandler(os, pretty = true))
      case InfosetType.NULL => Left(new NullInfosetOutputter())
    }
    if (outputter.isLeft) {
      outputter.left.map(_.setBlobAttributes(blobDir, null, blobSuffix))
    } else {
      // do nothing here, we set the blobAttributes using the Blob* properties
      // within the sax parse calls
    }
    outputter
  }

  /**
   * Convert the data to whatever form the InfosetInputter will expect
   *
   * If the data parameter is a Left[Array[Byte]], the return value must be
   * thread safe and immutable since it could potentially be shared and mutated
   * by different InfosetInputters.
   *
   * If the data parameter is a Right[InputStream], we can assume the caller
   * knows that the infoset represented by this InputStream will only be
   * unparsed once and so it is acceptable if the result is mutable or
   * non-thread safe.
   *
   * So for infoset types like "xml" and "json" where InfosetInputters accept
   * an InputStream, if this function receives a Right[InputStream], it will
   * simply return that InputStream. This avoids reading the entire infoset
   * into memory and makes it possible to unparse large infosets.
   *
   * For InfosetInputters that do not accept InputStreams, we must read in the
   * entire InputStream and convert it to whatever they expect (e.g. Scala XML
   * Node for "scala-xml"). Supporting large inputs with this infoset types is
   * not possible.
   *
   * Because this function may read large amounts of data from disk and parse
   * it into an object, this should be called outside of a performance loop,
   * with getInfosetInputter called inside the performance loop.
   */
  def infosetDataToInputterData(infosetType: InfosetType.Type, data: Either[Array[Byte],InputStream]): AnyRef = {
    infosetType match {
      case InfosetType.XML | InfosetType.JSON | InfosetType.SAX => data match {
        case Left(bytes) => bytes
        case Right(is) => is
      }
      case InfosetType.SCALA_XML => {
        val is = data match {
          case Left(bytes) => new ByteArrayInputStream(bytes)
          case Right(is) => is
        }
        val parser: SAXParser = {
          val f = DaffodilSAXParserFactory()
          f.setNamespaceAware(false)
          val p = f.newSAXParser()
          p
        }
        scala.xml.XML.withSAXParser(parser).load(is)
      }
      case InfosetType.JDOM => {
        val is = data match {
          case Left(bytes) => new ByteArrayInputStream(bytes)
          case Right(is) => is
        }
        val builder = new org.jdom2.input.SAXBuilder() {
          override protected def createParser(): XMLReader = {
            val rdr = super.createParser()
            XMLUtils.setSecureDefaults(rdr)
            rdr
          }
        }
        builder.build(is)
      }
      case InfosetType.W3CDOM => {
        val byteArr = data match {
          case Left(bytes) => bytes
          case Right(is) => IOUtils.toByteArray(is)
        }
        new ThreadLocal[org.w3c.dom.Document] {
          override def initialValue = {
            val dbf = DocumentBuilderFactory.newInstance()
            dbf.setNamespaceAware(true)
            dbf.setFeature(XMLUtils.XML_DISALLOW_DOCTYPE_FEATURE, true)
            val db = dbf.newDocumentBuilder()
            db.parse(new ByteArrayInputStream(byteArr))
          }
        }
      }
    }
  }

  def getInfosetInputter(
    infosetType: InfosetType.Type,
    anyRef: AnyRef,
    processor: DFDL.DataProcessor,
    outChannel: DFDL.Output): Either[InfosetInputter, DFDL.DaffodilUnparseContentHandler] = {
    infosetType match {
      case InfosetType.XML => {
        val is = anyRef match {
          case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
          case is: InputStream => is
        }
        Left(new XMLTextInfosetInputter(is))
      }
      case InfosetType.JSON => {
        val is = anyRef match {
          case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
          case is: InputStream => is
        }
        Left(new JsonInfosetInputter(is))
      }
      case InfosetType.SCALA_XML => {
        Left(new ScalaXMLInfosetInputter(anyRef.asInstanceOf[scala.xml.Node]))
      }
      case InfosetType.JDOM => {
        Left(new JDOMInfosetInputter(anyRef.asInstanceOf[org.jdom2.Document]))
      }
      case InfosetType.W3CDOM => {
        val tl = anyRef.asInstanceOf[ThreadLocal[org.w3c.dom.Document]]
        Left(new W3CDOMInfosetInputter(tl.get))
      }
      case InfosetType.SAX => {
        val dp = processor
        Right(dp.newContentHandlerInstance(outChannel))
      }
    }
  }

  def setLogLevel(verbose: Int): Unit = {
    val verboseLevel = verbose match {
      case 0 => Level.WARN
      case 1 => Level.INFO
      case 2 => Level.DEBUG
      case _ => Level.TRACE
    }
    Configurator.setLevel("org.apache.daffodil", verboseLevel)
  }

  def run(arguments: Array[String]): ExitCode.Value = {
    val conf = new CLIConf(arguments)

    setLogLevel(conf.verbose())

    val ret: ExitCode.Value = conf.subcommand match {

      case Some(conf.parse) => {
        val parseOpts = conf.parse

        val validate = parseOpts.validate.toOption.get

        val cfgFileNode: Option[Node] = parseOpts.config.toOption match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }

        val processor: Option[DFDL.DataProcessor] = {
          if (parseOpts.parser.isDefined) {
            createProcessorFromParser(parseOpts.parser(), parseOpts.path.toOption, validate)
          } else {
            val tunables = retrieveTunables(parseOpts.tunables, cfgFileNode)
            createProcessorFromSchema(parseOpts.schema(), parseOpts.rootNS.toOption, parseOpts.path.toOption, tunables, validate)
          }
        }.map{ _.withExternalVariables(retrieveExternalVariables(parseOpts.vars, cfgFileNode))}

        val rc = processor match {
          case None => ExitCode.UnableToCreateProcessor
          case Some(proc) => {
            Assert.invariant(!proc.isError)
            var processor = proc
            val input = parseOpts.infile.toOption match {
              case Some("-") | None => System.in
              case Some(file) => {
                val f = new File(file)
                new FileInputStream(f)
              }
            }
            val inStream = InputSourceDataInputStream(input)

            processor = processor.withValidationMode(validate)

            setupDebugOrTrace(processor.asInstanceOf[DataProcessor], conf)

            val output = parseOpts.output.toOption match {
              case Some("-") | None => System.out
              case Some(file) => new FileOutputStream(file)
            }
            val infosetType = parseOpts.infosetType.toOption.get
            val eitherOutputterOrHandler = getInfosetOutputter(infosetType, output)

            var lastParseBitPosition = 0L
            var keepParsing = true
            var exitCode = ExitCode.Success

            while (keepParsing) {

              val parseResult = eitherOutputterOrHandler match {
                case Right(saxContentHandler) =>
                  // reset in case we are streaming
                  saxContentHandler.reset()
                  Timer.getResult("parsing",
                    parseWithSAX(processor, inStream, saxContentHandler))
                case Left(outputter) =>
                  outputter.reset() // reset in case we are streaming
                  Timer.getResult("parsing", processor.parse(inStream, outputter))
              }
              val finfo = parseResult.resultState.asInstanceOf[FormatInfo]
              val loc = parseResult.resultState.currentLocation.asInstanceOf[DataLoc]
              displayDiagnostics(parseResult)

              if (parseResult.isProcessingError || parseResult.isValidationError) {
                keepParsing = false
                exitCode = ExitCode.ParseError
              } else {
                // only XMLTextInfosetOutputter, JsonInfosetOutputter and
                // DaffodilParseOutputStreamContentHandler write directly to the output stream. Other
                // InfosetOutputters must manually get the result and write it to the stream below
                eitherOutputterOrHandler match {
                  case Left(sxml: ScalaXMLInfosetOutputter) => {
                    val writer = new java.io.OutputStreamWriter(output, StandardCharsets.UTF_8)
                    scala.xml.XML.write(writer, sxml.getResult, "UTF-8", true, null)
                    writer.flush()
                  }
                  case Left(jdom: JDOMInfosetOutputter) => {
                    new org.jdom2.output.XMLOutputter().output(jdom.getResult, output)
                  }
                  case Left(w3cdom: W3CDOMInfosetOutputter) => {
                    val tf = TransformerFactory.newInstance()
                    val transformer = tf.newTransformer()
                    val result = new StreamResult(output)
                    val source = new DOMSource(w3cdom.getResult)
                    transformer.transform(source, result)
                  }
                  case _ => // do nothing
                }
                output.flush()

                if (!inStream.hasData()) {
                  // not even 1 more bit is available.
                  // do not try to keep parsing, nothing left to parse
                  keepParsing = false
                } else {
                  // There is more data available.
                  if (parseOpts.stream.toOption.get) {
                    // Streaming mode
                    if (lastParseBitPosition == loc.bitPos0b) {
                      // this parse consumed no data, that means this would get
                      // stuck in an infinite loop if we kept trying to stream,
                      // so we need to quit
                      val remainingBits =
                        if (loc.bitLimit0b.isDefined) {
                          (loc.bitLimit0b.get - loc.bitPos0b).toString
                        } else {
                          "at least " + (inStream.inputSource.bytesAvailable * 8)
                        }
                      Logger.log.error(s"Left over data after consuming 0 bits while streaming. Stopped after consuming ${loc.bitPos0b} bit(s) with ${remainingBits} bit(s) remaining.")
                      keepParsing = false
                      exitCode = ExitCode.LeftOverData
                    } else {
                      // last parse did consume data, and we know there is more
                      // data to come, so try to parse again.
                      lastParseBitPosition = loc.bitPos0b
                      keepParsing = true
                      output.write(0) // NUL-byte separates streams
                    }
                  } else {
                    // not streaming mode, and there is more data available,
                    // so show left over data warning
                    val Dump = new DataDumper
                    val bitsAlreadyConsumed = loc.bitPos0b % 8
                    val firstByteString = if (bitsAlreadyConsumed != 0) {
                      val bitsToDisplay = 8 - bitsAlreadyConsumed
                      val pbp = inStream.inputSource.position + 1
                      val firstByteBitArray = inStream.getByteArray(bitsToDisplay, finfo)
                      val fbs = firstByteBitArray(0).toBinaryString.takeRight(8).reverse.padTo(8, '0').reverse
                      val bits = if (finfo.bitOrder == BitOrder.MostSignificantBitFirst) {
                        "x" * bitsAlreadyConsumed + fbs.dropRight(bitsAlreadyConsumed)
                      } else {
                        fbs.takeRight(bitsToDisplay) + "x" * bitsAlreadyConsumed
                      }
                      val dumpString = f"\nLeft over data starts with partial byte. Left over data (Binary) at byte $pbp is: (0b$bits)"
                      dumpString
                    } else ""
                    val curBytePosition1b = inStream.inputSource.position + 1
                    val bytesAvailable = inStream.inputSource.bytesAvailable
                    val bytesLimit = math.min(8, bytesAvailable).toInt
                    val destArray = new Array[Byte](bytesLimit)
                    val destArrayFilled = inStream.inputSource.get(destArray, 0, bytesLimit)
                    val dumpString = if (destArrayFilled) Dump.dump(Dump.TextOnly(Some("utf-8")), 0, destArray.length * 8, ByteBuffer.wrap(destArray), includeHeadingLine = false).mkString("\n") else ""
                    val dataText = if (destArrayFilled) s"\nLeft over data (UTF-8) starting at byte ${curBytePosition1b} is: (${dumpString}...)" else ""
                    val dataHex = if (destArrayFilled) s"\nLeft over data (Hex) starting at byte ${curBytePosition1b} is: (0x${destArray.map { a => f"$a%02x" }.mkString}...)" else ""
                    val remainingBits =
                      if (loc.bitLimit0b.isDefined) {
                        (loc.bitLimit0b.get - loc.bitPos0b).toString
                      } else {
                        "at least " + (bytesAvailable * 8)
                      }
                    val leftOverDataMessage = s"Left over data. Consumed ${loc.bitPos0b} bit(s) with ${remainingBits} bit(s) remaining." + firstByteString + dataHex + dataText
                    Logger.log.error(leftOverDataMessage)
                    keepParsing = false
                    exitCode = ExitCode.LeftOverData
                  }
                }
              }
            }
          exitCode
          }
        }
        rc
      }

      case Some(conf.performance) => {
        val performanceOpts = conf.performance

        val validate = performanceOpts.validate.toOption.get

        val cfgFileNode = performanceOpts.config.toOption match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }

        val processor = {
          if (performanceOpts.parser.isDefined) {
            createProcessorFromParser(performanceOpts.parser(), performanceOpts.path.toOption, validate)
          } else {
            val tunables = retrieveTunables(performanceOpts.tunables, cfgFileNode)
            createProcessorFromSchema(performanceOpts.schema(), performanceOpts.rootNS.toOption, performanceOpts.path.toOption, tunables, validate)
          }
        }.map{ _.withExternalVariables(retrieveExternalVariables(performanceOpts.vars, cfgFileNode)) }
         .map{ _.withValidationMode(validate) }

        val rc: ExitCode.Value = processor match {
          case None => ExitCode.UnableToCreateProcessor
          case Some(processor) => {
            val infile = new java.io.File(performanceOpts.infile())

            val files = {
              if (infile.isDirectory()) {
                infile.listFiles.filter(!_.isDirectory)
              } else {
                Array(infile)
              }
            }

            val infosetType = performanceOpts.infosetType.toOption.get

            val dataSeq: Seq[Either[AnyRef, Array[Byte]]] = files.map { filePath =>
              // For performance testing, we want everything in memory so as to
              // remove I/O from consideration. Additionally, for both parse
              // and unparse we need immutable inputs since we could parse the
              // same input data multiple times in different performance runs.
              // So read the file data into an Array[Byte], and use that for
              // everything.
              val input = (new FileInputStream(filePath))
              val dataSize = filePath.length()
              val bytes = new Array[Byte](dataSize.toInt)
              input.read(bytes)
              val data = performanceOpts.unparse() match {
                case true => Left(infosetDataToInputterData(infosetType, Left(bytes)))
                case false => Right(bytes)
              }
              data
            }

            val inputs = (0 until performanceOpts.number()).map { n =>
              val index = n % dataSeq.length
              dataSeq(index)
            }
            val inputsWithIndex = inputs.zipWithIndex

            implicit val executionContext = new ExecutionContext {
              val threadPool = Executors.newFixedThreadPool(performanceOpts.threads())

              def execute(runnable: Runnable): Unit = {
                threadPool.submit(runnable)
              }

              def reportFailure(t: Throwable): Unit = {
                //do nothing
              }
            }

            val nullChannelForUnparse = Channels.newChannel(NullOutputStream.NULL_OUTPUT_STREAM)
            val nullOutputStreamForParse = NullOutputStream.NULL_OUTPUT_STREAM

            //the following line allows output verification
            //val nullChannelForUnparse = Channels.newChannel(System.out)
            val NSConvert = 1000000000.0
            val (totalTime, results) = Timer.getTimeResult({
              val tasks = inputsWithIndex.map {
                case (inData, n) =>
                  val task: Future[(Int, Long, Boolean)] = Future {
                    val (time, result) = inData match {
                      case Left(anyRef) => Timer.getTimeResult({
                        val inputterForUnparse = getInfosetInputter(infosetType, anyRef, processor, nullChannelForUnparse)
                        inputterForUnparse match {
                          case Left(inputter) =>
                            processor.unparse(inputter, nullChannelForUnparse)
                          case Right(contentHandler) =>
                            val is = anyRef match {
                              case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
                              case is: InputStream => is
                            }
                            unparseWithSAX(is, contentHandler)
                        }
                      })
                      case Right(data) => Timer.getTimeResult({
                        val input = InputSourceDataInputStream(data)
                        val eitherOutputterOrHandlerForParse = getInfosetOutputter(infosetType, nullOutputStreamForParse)
                        eitherOutputterOrHandlerForParse match {
                          case Left(outputter) => processor.parse(input, outputter)
                          case Right(saxContentHandler) =>
                            val parseResult = parseWithSAX(processor, input, saxContentHandler)
                            parseResult
                        }
                      })
                    }

                    (n, time, result.isError)
                  }
                  task
              }
              val results = tasks.map { Await.result(_, Duration.Inf) }
              results
            })

            val rates = results.map { results =>
              val (runNum: Int, nsTime: Long, error: Boolean) = results
              val rate = 1 / (nsTime / NSConvert)
              val status = if (error) "fail" else "pass"
              Logger.log.info(s"run: ${runNum}, seconds: ${nsTime / NSConvert}, rate: ${rate}, status: ${status}")
              rate
            }

            val numFailures = results.map { _._3 }.filter { e => e }.length
            if (numFailures > 0) {
              Logger.log.error(s"${numFailures} failures found\n")
            }

            val sec = totalTime / NSConvert
            val action = performanceOpts.unparse() match {
              case true => "unparse"
              case false => "parse"
            }
            printf("total %s time (sec): %f\n", action, sec)
            printf("min rate (files/sec): %f\n", rates.min)
            printf("max rate (files/sec): %f\n", rates.max)
            printf("avg rate (files/sec): %f\n", (performanceOpts.number() / sec))

            if (numFailures == 0) ExitCode.Success else ExitCode.PerformanceTestError
          }

        }
        rc
      }

      case Some(conf.unparse) => {
        val unparseOpts = conf.unparse

        val validate = unparseOpts.validate.toOption.get

        val cfgFileNode = unparseOpts.config.toOption match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }

        val processor = {
          if (unparseOpts.parser.isDefined) {
            createProcessorFromParser(unparseOpts.parser(), unparseOpts.path.toOption, validate)
          } else {
            val tunables = retrieveTunables(unparseOpts.tunables, cfgFileNode)
            createProcessorFromSchema(unparseOpts.schema(), unparseOpts.rootNS.toOption, unparseOpts.path.toOption, tunables, validate)
          }
        }.map{ _.withExternalVariables(retrieveExternalVariables(unparseOpts.vars, cfgFileNode)) }

        val output = unparseOpts.output.toOption match {
          case Some("-") | None => System.out
          case Some(file) => new FileOutputStream(file)
        }

        val outChannel = Channels.newChannel(output)
        //
        // We are not loading a schema here, we're loading the infoset to unparse.
        //
        val is = unparseOpts.infile.toOption match {
          case Some("-") | None => System.in
          case Some(fileName) => new FileInputStream(fileName)
        }

        val rc = processor match {
          case None => ExitCode.UnableToCreateProcessor
          case Some(processor) => {
            Assert.invariant(processor.isError == false)
            setupDebugOrTrace(processor.asInstanceOf[DataProcessor], conf)

            val maybeScanner =
              if (unparseOpts.stream.toOption.get) {
                val scnr = new Scanner(is)
                scnr.useDelimiter("\u0000")
                Some(scnr)
              } else {
                None
              }

            var keepUnparsing = maybeScanner.isEmpty || maybeScanner.get.hasNext
            var exitCode = ExitCode.Success

            while (keepUnparsing) {

              val eitherBytesOrStream =
                if (maybeScanner.isDefined) {
                  // The scanner reads the entire infoset up unto the delimiter
                  // into memory. No way around that with the --stream option.
                  Left(maybeScanner.get.next().getBytes())
                } else {
                  // We are not using the --stream option and won't need to
                  // unparse the infoset more than once. So pass the
                  // InputStream into infosetDataToInputterData. For some
                  // cases, such as "xml" or "json", we can create an
                  // InfosetInputter directly on this stream so that we can
                  // avoid reading the entire InputStream into memory
                  Right(is)
                }

              val inputterData = infosetDataToInputterData(unparseOpts.infosetType.toOption.get, eitherBytesOrStream)
              val inputterOrContentHandler = getInfosetInputter(unparseOpts.infosetType.toOption
                .get, inputterData, processor, outChannel)
              val unparseResult = inputterOrContentHandler match {
                case Left(inputter) =>
                  Timer.getResult("unparsing", processor.unparse(inputter, outChannel))
                case Right(contentHandler) =>
                  val is = inputterData match {
                    case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
                    case is: InputStream => is
                  }
                  Timer.getResult("unparsing", unparseWithSAX(is, contentHandler))
              }

              displayDiagnostics(unparseResult)

              if (unparseResult.isError) {
                keepUnparsing = false
                exitCode = ExitCode.UnparseError
              } else {
                keepUnparsing = maybeScanner.isDefined && maybeScanner.get.hasNext
              }
            }
            exitCode
          }
        }

        is.close()
        outChannel.close()

        rc
      }

      case Some(conf.save) => {
        val saveOpts = conf.save

        val validate = ValidationMode.Off

        val cfgFileNode = saveOpts.config.toOption match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }
        val tunables = retrieveTunables(saveOpts.tunables, cfgFileNode)
        val tunablesObj = DaffodilTunables(tunables)

        val processor = createProcessorFromSchema(saveOpts.schema(), saveOpts.rootNS.toOption, saveOpts.path.toOption, tunables, validate)

        val output = saveOpts.outfile.toOption match {
          case Some("-") | None => Channels.newChannel(System.out)
          case Some(file) => new FileOutputStream(file).getChannel()
        }

        val rc = processor match {
          case Some(processor) => {
            Assert.invariant(processor.isError == false)
            Timer.getResult("saving", processor.save(output))
            ExitCode.Success
          }
          case None => ExitCode.UnableToCreateProcessor
        }
        rc
      }

      case Some(conf.test) => {
        val testOpts = conf.test

        val tdmlFile = testOpts.tdmlfile()
        val tdmlRunner = new Runner(new java.io.File(tdmlFile))

        val tests = {
          if (testOpts.testnames.isDefined) {
            testOpts.testnames().flatMap(testName => {
              if (testOpts.regex()) {
                val regex = testName.r
                val matches = tdmlRunner.testCases.filter(testCase => regex.pattern.matcher(testCase.tcName).matches)
                matches.map(testCase => (testCase.tcName, Some(testCase)))
              } else {
                List((testName, tdmlRunner.testCases.find(_.tcName == testName)))
              }
            })
          } else {
            tdmlRunner.testCases.map(test => (test.tcName, Some(test)))
          }
        }.distinct.sortBy(_._1)

        tdmlRunner.reset

        if (testOpts.list()) {
          if (testOpts.info() > 0) {
            // determine the max lengths of the various pieces of atest
            val headers = List("Name", "Model", "Root", "Description")
            val maxCols = tests.foldLeft(headers.map(_.length)) {
              (maxVals, testPair) =>
                {
                  testPair match {
                    case (name, None) => List(
                      maxVals(0).max(name.length),
                      maxVals(1),
                      maxVals(2),
                      maxVals(3))
                    case (name, Some(test)) => List(
                      maxVals(0).max(name.length),
                      maxVals(1).max(test.model.length),
                      maxVals(2).max(test.rootName.length),
                      maxVals(3).max(test.description.length))
                  }
                }
            }
            val formatStr = maxCols.map(max => "%" + -max + "s").mkString("  ")
            println(formatStr.format(headers: _*))
            tests.foreach { testPair =>
              testPair match {
                case (name, Some(test)) => println(formatStr.format(name, test.model, test.rootName, test.description))
                case (name, None) => println(formatStr.format(name, "[Not Found]", "", ""))
              }
            }
          } else {
            tests.foreach { testPair =>
              testPair match {
                case (name, Some(test)) => println(name)
                case (name, None) => println("%s  [Not Found]".format(name))
              }
            }
          }
          ExitCode.Success
        } else {
          var pass = 0
          var fail = 0
          var notfound = 0
          tests.foreach { testPair =>
            testPair match {
              case (name, Some(test)) => {
                try {
                  test.run()
                  println("[Pass] %s".format(name))
                  pass += 1
                } catch {
                  case s: scala.util.control.ControlThrowable => throw s
                  case u: UnsuppressableException => throw u
                  case e: TDMLTestNotCompatibleException => {
                    println("[Skipped] %s (Not compatible implementation.)".format(name))
                  }
                  case e: Throwable => {
                    println("[Fail] %s".format(name))
                    fail += 1
                    if (testOpts.info() > 0) {
                      println("  Failure Information:")
                      println(indent(e.getMessage(), 4))
                    }
                    if (testOpts.info() > 1) {
                      println("  Backtrace:")
                      e.getStackTrace.foreach { st => println(indent(st.toString, 4)) }
                    }
                  }
                }
              }

              case (name, None) => {
                println("[Not Found] %s".format(name))
                notfound += 1
              }
            }
          }
          println("")
          println("Total: %d, Pass: %d, Fail: %d, Not Found: %s".format(pass + fail + notfound, pass, fail, notfound))

          if (fail == 0) ExitCode.Success else ExitCode.TestError
        }
      }

      case Some(conf.generate) => {
        conf.subcommands match {
          case List(conf.generate, conf.generate.c) => {
            val generateOpts = conf.generate.c

            // Read any config file and any tunables given as arguments
            val cfgFileNode = generateOpts.config.toOption match {
              case None => None
              case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
            }
            val tunables = retrieveTunables(generateOpts.tunables, cfgFileNode)

            // Create a CodeGenerator from the DFDL schema
            val generator = createGeneratorFromSchema(generateOpts.schema(), generateOpts.rootNS.toOption,
              tunables, generateOpts.language)

            // Ask the CodeGenerator to generate source code from the DFDL schema
            val rootNS = generateOpts.rootNS.toOption
            val outputDir = generateOpts.outdir.toOption.getOrElse(".")
            val rc = generator match {
              case Some(generator) => {
                Timer.getResult("generating", generator.generateCode(rootNS, outputDir))
                displayDiagnostics(generator)
                if (generator.isError) ExitCode.GenerateCodeError else ExitCode.Success
              }
              case None => ExitCode.GenerateCodeError
            }
            rc
          }
          // Required to avoid "match may not be exhaustive", but should never happen
          case _ => Assert.impossible()
        }
      }

      // Required to avoid "match may not be exhaustive", but should never happen
      case _ => Assert.impossible()
    }

    ret
  }

  private def unparseWithSAX(
    is: InputStream,
    contentHandler: DFDL.DaffodilUnparseContentHandler): UnparseResult = {
    val xmlReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    xmlReader.setContentHandler(contentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    try {
      xmlReader.parse(new org.xml.sax.InputSource(is))
    } catch {
      case _: DaffodilUnparseErrorSAXException => // do nothing, unparseResult has error info
    }

    val ur = contentHandler.getUnparseResult
    ur
  }

  private def parseWithSAX(
    processor: DFDL.DataProcessor,
    data: InputSourceDataInputStream,
    saxContentHandler: DaffodilParseOutputStreamContentHandler): ParseResult = {
    val saxXmlRdr = processor.newXMLReaderInstance
    saxXmlRdr.setContentHandler(saxContentHandler)
    saxXmlRdr.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY, blobDir)
    saxXmlRdr.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX, blobSuffix)
    saxXmlRdr.parse(data)
    val pr = saxXmlRdr.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    pr
  }

  def bugFound(e: Exception): Int = {
    System.err.println("""|
                          |!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          |!!   An unexpected exception occurred. This is a bug!   !!
                          |!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          |
                          | Please report this bug and help us fix it:
                          |
                          |  https://daffodil.apache.org/community/#issue-tracker
                          |
                          | Please include the following exception, the command you
                          | ran, and any input, schema, or tdml files used that led
                          | to this bug.
                          |
                          |""".stripMargin)
    e.printStackTrace
    1
  }

  def nyiFound(e: NotYetImplementedException): Int = {
    System.err.println("""|
                          |!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          |!!                 Not Yet Implemented                  !!
                          |!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          |
                          | You are using a feature that is not yet implemented:
                          |
                          | %s
                          |
                          | You can create a bug and track the progress of this
                          | feature at:
                          |
                          |  https://issues.apache.org/jira/projects/DAFFODIL
                          |
                          |""".format(Misc.getSomeMessage(e)).stripMargin)
    1
  }

  def oomError(e: OutOfMemoryError): Int = {
    System.err.println("""|
                          |!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          |!!             Daffodil ran out of memory!              !!
                          |!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          |
                          | Try increasing the amount of memory by changing or
                          | setting the DAFFODIL_JAVA_OPTS environment variable.
                          | "DAFFODIL_JAVA_OPTS=-Xmx5G" for 5GB.
                          |
                          |""".stripMargin)
    1
  }

  object ExitCode extends Enumeration {

    val Success = Value(0)
    val Failure = Value(1)

    val FileNotFound = Value(2)
    val OutOfMemory = Value(3)
    val BugFound = Value(4)
    val NotYetImplemented = Value(5)


    val ParseError = Value(20)
    val UnparseError = Value(21)
    val GenerateCodeError = Value(23)
    val TestError = Value(24)
    val PerformanceTestError = Value(25)


    val LeftOverData = Value(31)
    val InvalidParserException = Value(32)
    val BadExternalVariable = Value(33)
    val UserDefinedFunctionError = Value(34)
    val UnableToCreateProcessor = Value(35)

    val Usage = Value(64)

  }

  def main(arguments: Array[String]): Unit = {

    val ret = try {
      run(arguments)
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case e: java.io.FileNotFoundException => {
        Logger.log.error(Misc.getSomeMessage(e).get)
        ExitCode.FileNotFound
      }
      case e: ExternalVariableException => {
        Logger.log.error(Misc.getSomeMessage(e).get)
        ExitCode.BadExternalVariable
      }
      case e: BindingException => {
        Logger.log.error(Misc.getSomeMessage(e).get)
        ExitCode.BadExternalVariable
      }
      case e: NotYetImplementedException => {
        nyiFound(e)
        ExitCode.NotYetImplemented
      }
      case e: TDMLException => {
        Logger.log.error(Misc.getSomeMessage(e).get)
        ExitCode.TestError
      }
      case e: OutOfMemoryError => {
        oomError(e)
        ExitCode.OutOfMemory
      }
      case e: UserDefinedFunctionFatalErrorException => {
        Logger.log.error(Misc.getSomeMessage(e).get)
        e.cause.getStackTrace.take(10).foreach { ste =>
          Logger.log.error(s"    at ${ste}")
        }
        ExitCode.UserDefinedFunctionError
      }
      case e: DebuggerExitException => {
        ExitCode.Failure
      }
      case e: Exception => {
        bugFound(e)
        ExitCode.BugFound
      }
    }

    System.exit(ret.id)
  }
}
