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

import java.io.FileOutputStream
import java.io.OutputStream
import java.io.FileInputStream
import java.io.ByteArrayInputStream
import java.nio.channels.Channels
import java.nio.file.Paths
import java.util.Scanner

import scala.xml.{SAXParseException, Node}
import org.rogach.scallop
import org.apache.daffodil.debugger.{InteractiveDebugger, CLIDebuggerRunner, TraceDebuggerRunner}
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Timer
import org.apache.daffodil.xml._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.api.{WithDiagnostics, URISchemaSource, DFDL, DaffodilTunables}
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.LogWriter
import org.apache.daffodil.util.LoggingDefaults
import org.apache.daffodil.exceptions.NotYetImplementedException
import java.io.File

import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.externalvars.{Binding, BindingException}
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.configuration.ConfigurationLoader
import org.apache.daffodil.api.ValidationMode

import scala.language.reflectiveCalls
import scala.concurrent.Future
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import org.rogach.scallop.ScallopOption

import scala.concurrent.duration.Duration
import scala.concurrent.Await
import org.apache.daffodil.xml.QName
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.compiler.InvalidParserException
import java.net.URI

import org.apache.daffodil.tdml.TDMLException
import org.apache.daffodil.xml.RefQName
import org.rogach.scallop.ArgType
import org.rogach.scallop.ValueConverter
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.DataLoc
import org.apache.daffodil.processors.HasSetDebugger
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.infoset.NullInfosetOutputter
import org.apache.daffodil.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.infoset.JsonInfosetOutputter
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.infoset.JDOMInfosetOutputter
import org.apache.daffodil.infoset.W3CDOMInfosetOutputter
import org.apache.daffodil.infoset.XMLTextInfosetInputter
import org.apache.daffodil.infoset.JsonInfosetInputter
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.infoset.JDOMInfosetInputter
import org.apache.daffodil.infoset.W3CDOMInfosetInputter
import org.apache.daffodil.infoset.InfosetInputter
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.parsers.DocumentBuilderFactory
import org.apache.commons.io.IOUtils
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.tdml.TDMLTestNotCompatibleException
import org.apache.daffodil.io.DataDumper
import java.nio.ByteBuffer

import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.udf.UserDefinedFunctionFatalErrorException

class NullOutputStream extends OutputStream {
  override def close(): Unit = {
    //do nothing
  }
  override def flush(): Unit = {
    //do nothing
  }
  override def write(b: Array[Byte]): Unit = {
    //do nothing
  }
  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    //do nothing
  }
  override def write(b: Int): Unit = {
    //do nothing
  }
}

class CommandLineXMLLoaderErrorHandler() extends org.xml.sax.ErrorHandler with Logging {

  def warning(exception: SAXParseException) = {
    log(LogLevel.Warning, "loading schema: " + exception.getMessage())
  }

  def error(exception: SAXParseException) = {
    log(LogLevel.Error, "loading schema: " + exception.getMessage())
    System.exit(1)
  }

  def fatalError(exception: SAXParseException) = {
    error(exception)
  }
}

trait CLILogPrefix extends LogWriter {
  override def prefix(lvl: LogLevel.Type, logID: String): String = {
    "[" + lvl.toString.toLowerCase + "] "
  }

  override def suffix(logID: String): String = {
    ""
  }
}

object CLILogWriter extends CLILogPrefix {

  def write(msg: String): Unit = {
    Console.err.println(msg)
    Console.flush
  }
}

object TDMLLogWriter extends CLILogPrefix {
  var logs: scala.collection.mutable.Queue[String] = scala.collection.mutable.Queue.empty

  def write(msg: String): Unit = {
    logs += msg
  }

  def reset(): Unit = {
    logs = scala.collection.mutable.Queue.empty
  }
}

class CLIConf(arguments: Array[String]) extends scallop.ScallopConf(arguments)
  with Logging {

  /**
   * This is used when the flag is optional and so is its
   * argument.
   *
   * Let's use --validate [mode] as an example.
   *
   * This optionalValueConverter first determines if the
   * --validate flag was given.  If it wasn't, Right(None)
   * is returned.
   *
   * If the flag was given we then need to determine
   * if 'mode' was also given.  If mode was not given, Right(Some(None))
   * is returned meaning that the --validate flag was there but 'mode'
   * was not.  If the mode was given, Right(Some(Some(conv(mode)))) is
   * returned.  This means that the --validate flag was there as well as
   * a value for 'mode'.
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

  def validateConf(c1: => Option[scallop.ScallopConfBase])(fn: (Option[scallop.ScallopConfBase]) => Either[String, Unit]): Unit = {
    validations :+= new Function0[Either[String, Unit]] {
      def apply = {
        fn(c1)
      }
    }
  }

  implicit def validateConverter = singleArgConverter[ValidationMode.Type]((s: String) => {
    s.toLowerCase match {
      case "on" => ValidationMode.Full
      case "limited" => ValidationMode.Limited
      case "off" => ValidationMode.Off
      case _ => throw new Exception("Unrecognized ValidationMode %s.  Must be 'on', 'limited' or 'off'.".format(s))
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

  printedName = "Apache Daffodil (incubating)"

  helpWidth(76)

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

    log(LogLevel.Error, "%s", msg)
    sys.exit(1)
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
  val debug = opt[Option[String]](argName = "file", descr = "enable debugging. Optionally, read initial debugger commands from [file] if provided.")(optionalValueConverter[String](a => a))
  val trace = opt[Boolean](descr = "run the debugger with verbose trace output")
  val verbose = tally(descr = "increment verbosity level, one level for each -v")
  val version = opt[Boolean](descr = "Show version of this program")

  // Parse Subcommand Options
  val parse = new scallop.Subcommand("parse") {
    banner("""|Usage: daffodil parse (-s <schema> [-r [{namespace}]<root>] [-p <path>] |
              |                       -P <parser>)
              |                      [--validate [mode]]
              |                      [-D[{namespace}]<variable>=<value>...] [-o <output>]
              |                      [-I <infoset_type>]
              |                      [--stream]
              |                      [-c <file>] [infile]
              |
              |Parse a file, using either a DFDL schema or a saved parser
              |
              |Parse Options:""".stripMargin)

    descr("parse data to a DFDL infoset")
    helpWidth(76)

    val schema = opt[URI]("schema", argName = "file", descr = "the annotated DFDL schema to use to create the parser.")(fileResourceURIConverter)
    val rootNS = opt[RefQName]("root", argName = "node", descr = "the root element of the XML file to use.  An optional namespace may be provided. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema")
    val path = opt[String](argName = "path", descr = "path to the node to create parser.")
    val parser = opt[File](short = 'P', argName = "file", descr = "use a previously saved parser.")
    val output = opt[String](argName = "file", descr = "write output to a given file. If not given or is -, output is written to stdout.")
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'.")
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when parsing. An optional namespace may be provided.")
    val tunables = props[String]('T', keyName = "tunable", valueName = "value", descr = "daffodil tunable to be used when parsing.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")
    val infosetType = opt[String](short = 'I', argName = "infoset_type", descr = "infoset type to output. Must be one of 'xml', 'scala-xml', 'json', 'jdom', 'w3cdom', or 'null'.", default = Some("xml")).map { _.toLowerCase }
    val stream = toggle(noshort = true, default = Some(false), descrYes = "when left over data exists, parse again with remaining data, separating infosets by a NUL character", descrNo = "stop after the first parse, even if left over data exists")
    val infile = trailArg[String](required = false, descr = "input file to parse. If not specified, or a value of -, reads from stdin.")

    validateOpt(debug, infile) {
      case (Some(None), Some("-")) | (Some(None), None) => Left("Input must not be stdin during interactive debugging")
      case _ => Right(Unit)
    }

    validateOpt(schema, parser, rootNS) {
      case (None, None, _) => Left("One of --schema or --parser must be defined")
      case (Some(_), Some(_), _) => Left("Only one of --parser and --schema may be defined")
      case (None, Some(_), Some(_)) => Left("--root cannot be defined with --parser")
      case _ => Right(Unit)
    }

    validateOpt(parser, validate) {
      case (Some(_), Some(v)) if v == ValidationMode.Full => Left("The validation mode must be 'limited' or 'off' when using a saved parser.")
      case _ => Right(Unit)
    }

    validateOpt(infosetType) {
      case (Some("xml")) => Right(Unit)
      case (Some("scala-xml")) => Right(Unit)
      case (Some("json")) => Right(Unit)
      case (Some("jdom")) => Right(Unit)
      case (Some("w3cdom")) => Right(Unit)
      case (Some("null")) => Right(Unit)
      case (Some(t)) => Left("Unknown infoset type: " + t)
      case _ => Assert.impossible() // not possible due to default value
    }

  }

  // Performance Subcommand Options
  val performance = new scallop.Subcommand("performance") {
    banner("""|Usage: daffodil performance (-s <schema> [-r [{namespace}]<root>] [-p <path>] |
              |                       -P <parser>)
              |                      [--unparse]
              |                      [--validate [mode]]
              |                      [-N <number of files to process>]
              |                      [-t <threadcount>]
              |                      [-D[{namespace}]<variable>=<value>...]
              |                      [-I <infoset_type>]
              |                      [-c <file>] <infile>
              |
              |Run a performance test (parse or unparse), using either a DFDL schema or a saved parser
              |
              |Performance Options:""".stripMargin)

    descr("run performance test")
    helpWidth(76)

    val schema = opt[URI]("schema", argName = "file", descr = "the annotated DFDL schema to use to create the parser.")(fileResourceURIConverter)
    val unparse = opt[Boolean](default = Some(false), descr = "perform unparse instead of parse for performance.")
    val rootNS = opt[RefQName]("root", argName = "node", descr = "the root element of the XML file to use.  An optional namespace may be provided. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the schema")
    val number = opt[Int](short = 'N', argName = "number", default = Some(1), descr = "The total number of files to process.")
    val threads = opt[Int](short = 't', argName = "threads", default = Some(1), descr = "The number of threads to use.")
    val path = opt[String](argName = "path", descr = "path to the node to create parser.")
    val parser = opt[File](short = 'P', argName = "file", descr = "use a previously saved parser.")
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'.")
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when processing. An optional namespace may be provided.")
    val tunables = props[String]('T', keyName = "tunable", valueName = "value", descr = "daffodil tunable to be used when processing.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")
    val infosetType = opt[String](short = 'I', argName = "infoset_type", descr = "infoset type to parse/unparse. Must be one of 'xml', 'scala-xml', 'json', 'jdom', 'w3cdom', or 'null'.", default = Some("xml")).map { _.toLowerCase }
    val infile = trailArg[String](required = true, descr = "input file or directory containing files to process.")

    validateOpt(schema, parser, rootNS) {
      case (None, None, _) => Left("One of --schema or --parser must be defined")
      case (Some(_), Some(_), _) => Left("Only one of --parser and --schema may be defined")
      case (None, Some(_), Some(_)) => Left("--root cannot be defined with --parser")
      case _ => Right(Unit)
    }

    validateOpt(infosetType, unparse) {
      case (Some("xml"), _) => Right(Unit)
      case (Some("scala-xml"), _) => Right(Unit)
      case (Some("json"), _) => Right(Unit)
      case (Some("jdom"), _) => Right(Unit)
      case (Some("w3cdom"), _) => Right(Unit)
      case (Some("null"), Some(true)) => Left("infoset type null not valid with performance --unparse")
      case (Some("null"), _) => Right(Unit)
      case (Some(t), _) => Left("Unknown infoset type: " + t)
      case _ => Assert.impossible() // not possible due to default value
    }
  }

  // Unparse Subcommand Options
  val unparse = new scallop.Subcommand("unparse") {
    banner("""|Usage: daffodil unparse (-s <schema> [-r [{namespace}]<root>] [-p <path>] |
              |                         -P <parser>)
              |                        [--validate [mode]]
              |                        [-D[{namespace}]<variable>=<value>...] [-c <file>]
              |                        [-I <infoset_type>]
              |                        [--stream]
              |                        [-o <output>] [infile]
              |
              |Unparse an infoset file, using either a DFDL schema or a saved parser
              |
              |Unparse Options:""".stripMargin)

    descr("unparse a DFDL infoset")
    helpWidth(76)

    val schema = opt[URI]("schema", argName = "file", descr = "the annotated DFDL schema to use to create the parser.")(fileResourceURIConverter)
    val rootNS = opt[RefQName]("root", argName = "node", descr = "the root element of the XML file to use.  An optional namespace may be provided. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the schema")
    val path = opt[String](argName = "path", descr = "path to the node to create parser.")
    val parser = opt[File](short = 'P', argName = "file", descr = "use a previously saved parser.")
    val output = opt[String](argName = "file", descr = "write output to file. If not given or is -, output is written to standard output.")
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'.")
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when unparsing. An optional namespace may be provided.")
    val tunables = props[String]('T', keyName = "tunable", valueName = "value", descr = "daffodil tunable to be used when parsing.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")
    val infosetType = opt[String](short = 'I', argName = "infoset_type", descr = "infoset type to unparse. Must be one of 'xml', 'scala-xml', 'json', 'jdom', or 'w3cdom'.", default = Some("xml")).map { _.toLowerCase }
    val stream = toggle(noshort = true, default = Some(false), descrYes = "split the input data on the NUL character, and unparse each chuck separately", descrNo = "treat the entire input data as one infoset")
    val infile = trailArg[String](required = false, descr = "input file to unparse. If not specified, or a value of -, reads from stdin.")

    validateOpt(debug, infile) {
      case (Some(None), Some("-")) | (Some(None), None) => Left("Input must not be stdin during interactive debugging")
      case _ => Right(Unit)
    }

    validateOpt(schema, parser, rootNS) {
      case (None, None, _) => Left("One of --schema or --parser must be defined")
      case (Some(_), Some(_), _) => Left("Only one of --parser and --schema may be defined")
      case (None, Some(_), Some(_)) => Left("--root cannot be defined with --parser")
      case _ => Right(Unit)
    }

    validateOpt(config) {
      case Some(path) => {
        val fin = new File(path)
        if (!fin.exists) Left("--config file does not exist.")
        else if (!fin.canRead) Left("--config file could not be read.")
        else Right(Unit)
      }
      case _ => Right(Unit)
    }

    validateOpt(infosetType) {
      case (Some("xml")) => Right(Unit)
      case (Some("scala-xml")) => Right(Unit)
      case (Some("json")) => Right(Unit)
      case (Some("jdom")) => Right(Unit)
      case (Some("w3cdom")) => Right(Unit)
      //case (Some("null")) => Right(Unit) // null is not valid for unparsing
      case (Some(t)) => Left("Unknown infoset type: " + t)
      case _ => Assert.impossible() // not possible due to default value
    }
  }

  // Save Subcommand Options
  val save = new scallop.Subcommand("save-parser") {
    banner("""|Usage: daffodil save-parser -s <schema> [-r [{namespace}]<root>]
              |                            [-p <path>]
              |                            [-D[{namespace}]<variable>=<value>...]
              |                            [-c <file>] [outfile]
              |
              |Create and save a parser using a DFDL schema
              |
              |Save Parser Options:""".stripMargin)

    descr("save a daffodil parser for reuse")
    helpWidth(76)

    val schema = opt[URI]("schema", argName = "file", required = true, descr = "the annotated DFDL schema to use to create the parser.")(fileResourceURIConverter)
    val rootNS = opt[RefQName]("root", argName = "node", descr = "the root element of the XML file to use.  An optional namespace may be provided. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema")
    val path = opt[String](argName = "path", descr = "path to the node to create parser.")
    val outfile = trailArg[String](required = false, descr = "output file to save parser. If not specified, or a value of -, writes to stdout.")
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used.")
    val tunables = props[String]('T', keyName = "tunable", valueName = "value", descr = "daffodil tunable to be used when parsing.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")

    validateOpt(schema) {
      case (None) => Left("No schemas specified using the --schema option")
      case _ => Right(Unit)
    }
  }

  // Test Subcommand Options
  val test = new scallop.Subcommand("test") {
    banner("""|Usage: daffodil test <tdmlfile> [testname...]
              |
              |List or execute tests in a TDML file
              |
              |Test Options:""".stripMargin)

    descr("list or execute TDML tests")
    helpWidth(76)

    val list = opt[Boolean](descr = "show names and descriptions instead of running test cases.")
    val regex = opt[Boolean](descr = "treat <names> as regular expressions.")
    val tdmlfile = trailArg[String](required = true, descr = "test data markup language (TDML) file.")
    val names = trailArg[List[String]](required = false, descr = "name of test case(s) in tdml file. If not given, all tests in tdmlfile are run.")
    val info = tally(descr = "increment test result information output level, one level for each -i")
  }

  addSubcommand(parse)
  addSubcommand(performance)
  addSubcommand(unparse)
  addSubcommand(save)
  addSubcommand(test)

  validateOpt(trace, debug) {
    case (Some(true), Some(_)) => Left("Only one of --trace and --debug may be defined")
    case _ => Right(Unit)
  }

  validateConf(subcommand) {
    case None => Left("Missing subcommand")
    case _ => Right(Unit)
  }

  verify()
}

object Main extends Logging {

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
   * @param pathName The path to the file.
   * @return The Node representation of the file.
   */
  def loadConfigurationFile(pathName: String) = {
    val loader = new DaffodilXMLLoader()
    val node = ConfigurationLoader.getConfiguration(loader, pathName)
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
      val lvl = if (d.isError) LogLevel.Error else LogLevel.Warning
      log(lvl, d.getMessage())
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
    val compiler = Compiler()
    val processor = Timer.getResult("reloading", compiler.reload(savedParser))
    displayDiagnostics(processor)
    if (!processor.isError) {
      Some(processor.withValidationMode(mode))
    } else None
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
            log(LogLevel.Warning, "Using --debug on a non-interactive console may result in display issues")
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
    mode: ValidationMode.Type) = {
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
    val pf = Timer.getResult("compiling", {
      val processorFactory = compiler.compileSource(schemaSource)
      if (!processorFactory.isError) {
        val processor = processorFactory.onPath(path.getOrElse("/")).withValidationMode(mode)
        displayDiagnostics(processor)
        Some(processor) // note: processor could still be isError == true
        // but we do definitely get a processor.
      } else {
        displayDiagnostics(processorFactory)
        None
      }
    })
    pf
  }

  // write blobs to $PWD/daffodil-blobs/*.bin
  val blobDir = Paths.get(System.getProperty("user.dir"), "daffodil-blobs")
  val blobSuffix = ".bin"

  def getInfosetOutputter(infosetType: String, os: java.io.OutputStream): InfosetOutputter = {
    val outputter = infosetType match {
      case "xml" => new XMLTextInfosetOutputter(os, true)
      case "scala-xml" => new ScalaXMLInfosetOutputter()
      case "json" => new JsonInfosetOutputter(os, true)
      case "jdom" => new JDOMInfosetOutputter()
      case "w3cdom" => new W3CDOMInfosetOutputter()
      case "null" => new NullInfosetOutputter()
    }
    outputter.setBlobAttributes(blobDir, null, blobSuffix)
    outputter
  }

  // Converts the data to whatever form the InfosetInputter will want. Note
  // that this requires that the result of this must be thread safe and not
  // mutated by the InfosetInputters, since it will be shared among
  // InfosetInputters to reduce copies. This means InfosetInputters that expect
  // a Reader (e.g. xml, json) will just return the data here and have the
  // Reader created when the InfosetInputter is created. Note that w3c dom is
  // not thread-safe, even for reading, so we must create a thread local
  // variable so that each thread has its own copy of the dom tree.
  //
  // This should be called outside of a performance loop, with
  // getInfosetInputter called inside the performance loop
  def infosetDataToInputterData(infosetType: String, data: Array[Byte]): AnyRef = {
    infosetType match {
      case "xml" => data
      case "scala-xml" => scala.xml.XML.load(new ByteArrayInputStream(data))
      case "json" => data
      case "jdom" => new org.jdom2.input.SAXBuilder().build(new ByteArrayInputStream(data))
      case "w3cdom" => {
        new ThreadLocal[org.w3c.dom.Document] {
          override def initialValue = {
            val dbf = DocumentBuilderFactory.newInstance()
            dbf.setNamespaceAware(true)
            val db = dbf.newDocumentBuilder()
            db.parse(new ByteArrayInputStream(data))
          }
        }
      }
    }
  }

  def getInfosetInputter(infosetType: String, anyRef: AnyRef): InfosetInputter = {
    infosetType match {
      case "xml" => {
        val is = new ByteArrayInputStream(anyRef.asInstanceOf[Array[Byte]])
        new XMLTextInfosetInputter(is)
      }
      case "scala-xml" => new ScalaXMLInfosetInputter(anyRef.asInstanceOf[scala.xml.Node])
      case "json" => {
        val is = new ByteArrayInputStream(anyRef.asInstanceOf[Array[Byte]])
        new JsonInfosetInputter(is)
      }
      case "jdom" => new JDOMInfosetInputter(anyRef.asInstanceOf[org.jdom2.Document])
      case "w3cdom" => {
        val tl = anyRef.asInstanceOf[ThreadLocal[org.w3c.dom.Document]]
        new W3CDOMInfosetInputter(tl.get)
      }
    }
  }

  def run(arguments: Array[String]): Int = {
    LoggingDefaults.setLogWriter(CLILogWriter)

    val conf = new CLIConf(arguments)

    val verboseLevel = conf.verbose() match {
      case 0 => LogLevel.Warning
      case 1 => LogLevel.Info
      case 2 => LogLevel.Compile
      case 3 => LogLevel.Debug
      case _ => LogLevel.OOLAGDebug
    }
    LoggingDefaults.setLoggingLevel(verboseLevel)

    val ret = conf.subcommand match {

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
          case Some(proc) if (!proc.isError) => {
            var processor = proc
            val input = parseOpts.infile.toOption match {
              case Some("-") | None => System.in
              case Some(file) => {
                val f = new File(parseOpts.infile())
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
            val outputter = getInfosetOutputter(parseOpts.infosetType.toOption.get, output)

            var lastParseBitPosition = 0L
            var keepParsing = true
            var error = false

            while (keepParsing) {

              outputter.reset() // reset in case we are streaming

              val parseResult = Timer.getResult("parsing", processor.parse(inStream, outputter))
              val finfo = parseResult.resultState.asInstanceOf[FormatInfo]
              val loc = parseResult.resultState.currentLocation.asInstanceOf[DataLoc]
              displayDiagnostics(parseResult)

              if (parseResult.isProcessingError || parseResult.isValidationError) {
                keepParsing = false
                error = true
              } else {
                // only XMLTextInfosetOutputter and JsonInfosetOutputter write
                // directly to the output stream. Other InfosetOutputters must manually
                // get the result and write it to the stream
                outputter match {
                  case sxml: ScalaXMLInfosetOutputter => {
                    val writer = new java.io.OutputStreamWriter(output, "UTF-8")
                    scala.xml.XML.write(writer, sxml.getResult, "UTF-8", true, null)
                    writer.flush()
                  }
                  case jdom: JDOMInfosetOutputter => {
                    new org.jdom2.output.XMLOutputter().output(jdom.getResult, output)
                  }
                  case w3cdom: W3CDOMInfosetOutputter => {
                    val tf = TransformerFactory.newInstance()
                    val transformer = tf.newTransformer()
                    val result = new StreamResult(output)
                    val source = new DOMSource(w3cdom.getResult)
                    transformer.transform(source, result)
                  }
                  case _ => // do nothing
                }
                output.flush()

                if (loc.isAtEnd) {
                  // do not try to keep parsing, nothing left to parse
                  keepParsing = false
                  error = false
                } else {
                  // remaining data exists

                  if (parseOpts.stream.toOption.get) {
                    if (lastParseBitPosition == loc.bitPos0b) {
                      // this parse consumed no data, that means this would get
                      // stuck in an infinite loop if we kept trying to stream,
                      // so we need quit
                      val remainingBits =
                        if (loc.bitLimit0b.isDefined) {
                          (loc.bitLimit0b.get - loc.bitPos0b).toString
                        } else {
                          "at least " + (inStream.inputSource.bytesAvailable * 8)
                        }
                      log(LogLevel.Warning, "Left over data after consuming 0 bits while streaming. Stopped after consuming %s bit(s) with %s bit(s) remaining.", loc.bitPos0b, remainingBits)
                      keepParsing = false
                      error = true
                    } else {
                      lastParseBitPosition = loc.bitPos0b
                      keepParsing = true
                      error = false
                      output.write(0) // NUL-byte separates streams
                    }
                  } else {
                    // not streaming, show left over data warning
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
                    val leftOverDataWarning = s"Left over data. Consumed ${loc.bitPos0b} bit(s) with ${remainingBits} bit(s) remaining." + firstByteString + dataHex + dataText
                    log(LogLevel.Warning, leftOverDataWarning)
                    keepParsing = false
                    error = true
                  }
                }
              }
            }
            if (error) 1 else 0
          }
          case Some(processor) => 1
          case None => 1
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

        val rc = processor match {
          case Some(processor: DataProcessor) if (!processor.isError) => {
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
              val input = (new FileInputStream(filePath))
              val dataSize = filePath.length()
              val fileContent = new Array[Byte](dataSize.toInt)
              input.read(fileContent) // For performance testing, we want everything in memory so as to remove I/O from consideration.
              val data = performanceOpts.unparse() match {
                case true => Left(infosetDataToInputterData(infosetType, fileContent))
                case false => Right(fileContent)
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

            val nullChannelForUnparse = java.nio.channels.Channels.newChannel(new NullOutputStream)
            val nullOutputStreamForParse = new NullOutputStream()

            //the following line allows output verification
            //val nullChannelForUnparse = java.nio.channels.Channels.newChannel(System.out)
            val NSConvert = 1000000000.0
            val (totalTime, results) = Timer.getTimeResult({
              val tasks = inputsWithIndex.map {
                case (inData, n) =>
                  val task: Future[(Int, Long, Boolean)] = Future {
                    val (time, result) = inData match {
                      case Left(anyRef) => Timer.getTimeResult({
                        val inputterForUnparse = getInfosetInputter(infosetType, anyRef)
                        processor.unparse(inputterForUnparse, nullChannelForUnparse)
                      })
                      case Right(data) => Timer.getTimeResult({
                        val input = InputSourceDataInputStream(data)
                        val outputterForParse = getInfosetOutputter(infosetType, nullOutputStreamForParse)
                        processor.parse(input, outputterForParse)
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
              log(LogLevel.Info, "run: %d, seconds: %f, rate: %f, status: %s", runNum, nsTime / NSConvert, rate, if (error) "fail" else "pass")
              rate
            }

            val numFailures = results.map { _._3 }.filter { e => e }.length
            if (numFailures > 0) {
              log(LogLevel.Error, "%d failures found\n", numFailures)
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

            if (numFailures == 0) 0 else 1
          }
          case Some(processor) => 1
          case None => 1
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

        val outChannel = java.nio.channels.Channels.newChannel(output)
        //
        // We are not loading a schema here, we're loading the infoset to unparse.
        //
        val is = unparseOpts.infile.toOption match {
          case Some("-") | None => System.in
          case Some(fileName) => new FileInputStream(fileName)
        }

        val rc = processor match {
          case None => 1
          case Some(processor) => {
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
            var error = false

            while (keepUnparsing) {

              val data =
                if (maybeScanner.isDefined) {
                  maybeScanner.get.next().getBytes()
                } else {
                  IOUtils.toByteArray(is)
                }

              val inputterData = infosetDataToInputterData(unparseOpts.infosetType.toOption.get, data)
              val inputter = getInfosetInputter(unparseOpts.infosetType.toOption.get, inputterData)
              val unparseResult = Timer.getResult("unparsing", processor.unparse(inputter, outChannel))
              displayDiagnostics(unparseResult)

              if (unparseResult.isError) {
                keepUnparsing = false
                error = true
              } else {
                keepUnparsing = maybeScanner.isDefined && maybeScanner.get.hasNext
                error = false
              }
            }

            if (error) 1 else 0
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
            Timer.getResult("saving", processor.save(output))
            0
          }
          case None => 1
        }
        rc
      }

      case Some(conf.test) => {
        val testOpts = conf.test

        val tdmlFile = testOpts.tdmlfile()
        val tdmlRunner = new DFDLTestSuite(new java.io.File(tdmlFile))
        setupDebugOrTrace(tdmlRunner, conf)

        val tests = {
          if (testOpts.names.isDefined) {
            testOpts.names().flatMap(testName => {
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
        } else {
          LoggingDefaults.setLogWriter(TDMLLogWriter)
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
                      if (testOpts.info() > 1) {
                        println("  Logs:")
                        if (TDMLLogWriter.logs.size > 0) {
                          TDMLLogWriter.logs.foreach { l => println(indent(l, 4)) }
                        } else {
                          println("    None")
                        }

                        println("  Backtrace:")
                        e.getStackTrace.foreach { st => println(indent(st.toString, 4)) }
                      }
                    }
                  }
                }
                TDMLLogWriter.reset
              }

              case (name, None) => {
                println("[Not Found] %s".format(name))
                notfound += 1
              }
            }
          }
          println("")
          println("Total: %d, Pass: %d, Fail: %d, Not Found: %s".format(pass + fail + notfound, pass, fail, notfound))
        }
        0
      }

      case _ => {
        // This should never happen, this is caught by validation
        Assert.impossible()
        // 1
      }
    }

    ret
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
                          |""".format(e.getMessage()).stripMargin)
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
                          |""".format(e.getMessage()).stripMargin)
    1
  }

  def main(arguments: Array[String]): Unit = {
    val ret = try {
      run(arguments)
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case e: java.io.FileNotFoundException => {
        log(LogLevel.Error, "%s", e.getMessage())
        1
      }
      case e: InvalidParserException => {
        log(LogLevel.Error, "%s", e.getMessage())
        1
      }
      case e: BindingException => {
        log(LogLevel.Error, "%s", e.getMessage())
        1
      }
      case e: NotYetImplementedException => {
        nyiFound(e)
      }
      case e: TDMLException => {
        log(LogLevel.Error, "%s", e.getMessage())
        1
      }
      case e: OutOfMemoryError => {
        oomError(e)
      }
      case e: UserDefinedFunctionFatalErrorException => {
        log(LogLevel.Error, "%s", e.getMessage())
        e.printStackTrace()
        1
      }
      case e: Exception => {
        bugFound(e)
      }
    }

    System.exit(ret)
  }
}
