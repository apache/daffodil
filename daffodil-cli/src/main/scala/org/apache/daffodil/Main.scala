/* Copyright (c) 2013-2015 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil

import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.BufferedReader
import java.io.OutputStreamWriter
import java.io.OutputStream
import java.io.InputStreamReader
import java.io.Writer
import java.io.FileInputStream
import java.io.ByteArrayInputStream
import java.io.File
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.net.URI
import scala.xml.SAXParseException
import org.rogach.scallop
import org.apache.daffodil.debugger.{ InteractiveDebugger, TraceDebuggerRunner, CLIDebuggerRunner }
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Timer
import org.apache.daffodil.xml._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.api.WithDiagnostics
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.LogWriter
import org.apache.daffodil.util.LoggingDefaults
import org.apache.daffodil.exceptions.NotYetImplementedException
import java.io.File
import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.api.ValidationMode
import scala.xml.Node
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.configuration.ConfigurationLoader
import org.apache.daffodil.api.ValidationMode
import scala.language.reflectiveCalls
import scala.concurrent.Future
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe._
import org.rogach.scallop.ScallopOption
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import org.apache.daffodil.xml.QName
import org.apache.daffodil.compiler._
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.compiler.InvalidParserException
import java.net.URI
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.tdml.TDMLException
import org.apache.daffodil.xml.RefQName
import org.rogach.scallop.ArgType
import org.rogach.scallop.ValueConverter
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.HasSetDebugger
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.util.InvalidJavaVersionException
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
import org.apache.daffodil.api.TunableLimitExceededError
import org.apache.daffodil.api.DaffodilTunables

class NullOutputStream extends OutputStream {
  override def close() {}
  override def flush() {}
  override def write(b: Array[Byte]) {}
  override def write(b: Array[Byte], off: Int, len: Int) {}
  override def write(b: Int) {}
}

class NullWriter extends Writer {
  override def append(c: Char): Writer = { this }
  override def append(csq: CharSequence): Writer = { this }
  override def append(csq: CharSequence, start: Int, end: Int): Writer = { this }
  override def close(): Unit = {}
  override def flush(): Unit = {}
  override def write(chr: Array[Char]): Unit = {}
  override def write(chr: Array[Char], st: Int, end: Int): Unit = {}
  override def write(idx: Int): Unit = {}
  override def write(str: String): Unit = {}
  override def write(str: String, st: Int, end: Int): Unit = {}
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
    log(LogLevel.Error, "loading schema: " + exception.getMessage())
    System.exit(1)
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

  def write(msg: String) {
    Console.err.println(msg)
    Console.flush
  }
}

object TDMLLogWriter extends CLILogPrefix {
  var logs: scala.collection.mutable.Queue[String] = scala.collection.mutable.Queue.empty

  def write(msg: String) {
    logs += msg
  }

  def reset() {
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
  def optionalValueConverter[A](conv: String => A)(implicit tt: TypeTag[Option[A]]): scallop.ValueConverter[Option[A]] =
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
          case Nil => Right(None) // --validate flag was not present
          case (_, Nil) :: Nil => Right(Some(None)) // --validate flag was present but 'mode' wasn't
          case (_, v :: Nil) :: Nil => { // --validate [mode] was present, perform the conversion
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
          case _ => Left("you should provide no more than one argument for this option") // Error because we expect there to be at most one --validate flag
        }
      }
      val tag = tt
      val argType = scallop.ArgType.LIST
      override def argFormat(name: String): String = "[" + name + "]"
    }

  def validateConf(c1: => Option[scallop.ScallopConf])(fn: (Option[scallop.ScallopConf]) => Either[String, Unit]) {
    validations :+= new Function0[Either[String, Unit]] {
      def apply = {
        fn(c1)
      }
    }
  }

  def validateConverter(s: String): ValidationMode.Type = {
    s.toLowerCase match {
      case "on" => ValidationMode.Full
      case "limited" => ValidationMode.Limited
      case "off" => ValidationMode.Off
      case "" => ValidationMode.Full // Is this even possible?
      case _ => throw new Exception("Unrecognized ValidationMode %s.  Must be 'on', 'limited' or 'off'.".format(s))
    }
  }

  def qnameConvert(s: String): RefQName = {
    val eQN = QName.refQNameFromExtendedSyntax(s)
    eQN.get
  }

  def singleArgConverter[A](conv: String => A)(implicit tt: TypeTag[A]) = new ValueConverter[A] {
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
    val tag = tt
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

  // Parse Subcommand Options
  val parse = new scallop.Subcommand("parse") {
    banner("""|Usage: daffodil parse (-s <schema> [-r [{namespace}]<root>] [-p <path>] |
              |                       -P <parser>)
              |                      [--validate [mode]]
              |                      [-D[{namespace}]<variable>=<value>...] [-o <output>]
              |                      [-I <infoset_type>]
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
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'. Defaults to 'on' if mode is not supplied.")(optionalValueConverter[ValidationMode.Type](a => validateConverter(a)).map {
      case None => ValidationMode.Full
      case Some(mode) => mode
    })
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when parsing. An optional namespace may be provided.")
    val tunables = props[String]('T', keyName = "tunable", valueName = "value", descr = "daffodil tunable to be used when parsing.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")
    val infosetType = opt[String](short = 'I', argName = "infoset_type", descr = "infoset type to output. Must be one of 'xml', 'scala-xml', 'json', 'jdom', 'w3cdom', or 'null'.", default = Some("xml")).map { _.toLowerCase }
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
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'. Defaults to 'on' if mode is not supplied.")(optionalValueConverter[ValidationMode.Type](a => validateConverter(a)).map {
      case None => ValidationMode.Full
      case Some(mode) => mode
    })
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
    val validate: ScallopOption[ValidationMode.Type] = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'. Defaults to 'on' if mode is not supplied.")(optionalValueConverter[ValidationMode.Type](a => validateConverter(a)).map {
      case None => ValidationMode.Full
      case Some(mode) => mode
    })
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when unparsing. An optional namespace may be provided.")
    val tunables = props[String]('T', keyName = "tunable", valueName = "value", descr = "daffodil tunable to be used when parsing.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")
    val infosetType = opt[String](short = 'I', argName = "infoset_type", descr = "infoset type to unparse. Must be one of 'xml', 'scala-xml', 'json', 'jdom', or 'w3cdom'.", default = Some("xml")).map { _.toLowerCase }
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

  validateOpt(trace, debug) {
    case (Some(true), Some(_)) => Left("Only one of --trace and --debug may be defined")
    case _ => Right(Unit)
  }

  validateConf(subcommand) {
    case None => Left("Missing subcommand")
    case _ => Right(Unit)
  }
}

object Main extends Logging {

  val traceCommands = Seq("display info parser",
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

  def displayDiagnostics(pr: WithDiagnostics) {
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
  def retrieveExternalVariables(vars: Map[String, String], configFileNode: Option[Node], tunables: DaffodilTunables): Seq[Binding] = {
    val configFileVars: Seq[Binding] = configFileNode match {
      case None => Seq.empty
      case Some(configNode) => {
        // We have a configuration file node, we now need to grab
        // the externalVariableBindings node.
        val extVarBindingNodeOpt = (configNode \ "externalVariableBindings").headOption
        extVarBindingNodeOpt match {
          case None => Seq.empty
          case Some(extVarBindingsNode) => ExternalVariablesLoader.getVariables(extVarBindingsNode, tunables)
        }
      }
    }

    val individualVars = ExternalVariablesLoader.getVariables(vars)

    val bindings = overrideBindings(individualVars, configFileVars)
    bindings
  }

  def createProcessorFromParser(savedParser: File, path: Option[String], mode: ValidationMode.Type) = {
    val compiler = Compiler()
    val processor = Timer.getResult("reloading", compiler.reload(savedParser))
    displayDiagnostics(processor)
    if (!processor.isError) {
      processor.setValidationMode(mode)
      Some(processor)
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
      // runner.init(id)
      proc.setDebugging(true)
      proc.setDebugger(id)
    }
  }

  def createProcessorFromSchema(schema: URI, rootNS: Option[RefQName], path: Option[String],
    extVars: Seq[Binding],
    tunables: Map[String, String],
    mode: ValidationMode.Type) = {
    val compiler = Compiler()

    compiler.setExternalDFDLVariables(extVars)
    compiler.setTunables(tunables)

    rootNS match {
      case None => // nothing
      case Some(RefQName(_, root, ns)) => compiler.setDistinguishedRootNode(root, ns.toStringOrNullIfNoNS)
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
        val processor = processorFactory.onPath(path.getOrElse("/"))
        displayDiagnostics(processor)
        processor.setValidationMode(mode)
        Some(processor) // note: processor could still be isError == true
        // but we do definitely get a processor.
      } else {
        displayDiagnostics(processorFactory)
        None
      }
    })
    pf
  }

  def getInfosetOutputter(infosetType: String, writer: java.io.Writer): InfosetOutputter = {
    infosetType match {
      case "xml" => new XMLTextInfosetOutputter(writer)
      case "scala-xml" => new ScalaXMLInfosetOutputter()
      case "json" => new JsonInfosetOutputter(writer)
      case "jdom" => new JDOMInfosetOutputter()
      case "w3cdom" => new W3CDOMInfosetOutputter()
      case "null" => new NullInfosetOutputter()
    }
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
        val rdr = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(anyRef.asInstanceOf[Array[Byte]])))
        new XMLTextInfosetInputter(rdr)
      }
      case "scala-xml" => new ScalaXMLInfosetInputter(anyRef.asInstanceOf[scala.xml.Node])
      case "json" => {
        val rdr = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(anyRef.asInstanceOf[Array[Byte]])))
        new JsonInfosetInputter(rdr)
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

        val validate = parseOpts.validate.get.get

        val cfgFileNode = parseOpts.config.get match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }

        val processor = {
          if (parseOpts.parser.isDefined) {
            val p = createProcessorFromParser(parseOpts.parser(), parseOpts.path.get, validate)
            p.get.setExternalVariables(retrieveExternalVariables(parseOpts.vars, cfgFileNode, p.get.getTunables()))
            p
          } else {
            val tunables = retrieveTunables(parseOpts.tunables, cfgFileNode)
            val tunablesObj = DaffodilTunables(tunables)
            val extVarsBindings = retrieveExternalVariables(parseOpts.vars, cfgFileNode, tunablesObj)

            //val schema = new URI(parseOpts.schemaString())
            createProcessorFromSchema(parseOpts.schema(), parseOpts.rootNS.get, parseOpts.path.get, extVarsBindings, tunables, validate)
          }
        }

        val rc = processor match {
          case Some(processor) if (!processor.isError) => {
            val (input, optDataSize) = parseOpts.infile.get match {
              case Some("-") | None => (System.in, None)
              case Some(file) => {
                val f = new File(parseOpts.infile())
                (new FileInputStream(f), Some(f.length()))
              }
            }
            val inChannel = java.nio.channels.Channels.newChannel(input);

            processor.setValidationMode(validate)
            setupDebugOrTrace(processor.asInstanceOf[DataProcessor], conf)

            val output = parseOpts.output.get match {
              case Some("-") | None => System.out
              case Some(file) => new FileOutputStream(file)
            }
            val writer = new BufferedWriter(new OutputStreamWriter(output))
            val outputter = getInfosetOutputter(parseOpts.infosetType.get.get, writer)

            val parseResult = Timer.getResult("parsing",
              optDataSize match {
                case None => processor.parse(inChannel, outputter)
                case Some(szInBytes) => processor.parse(inChannel, outputter, szInBytes * 8)
              })
            val loc = parseResult.resultState.currentLocation
            displayDiagnostics(parseResult)
            if (parseResult.isProcessingError) {
              1
            } else {
              // only XMLTextInfosetOutputter and JsonInfosetOutputter write
              // directly to the writer. Other InfosetOutputters must manually
              // be converted to a string and written to the output
              outputter match {
                case sxml: ScalaXMLInfosetOutputter => writer.write(sxml.getResult.toString)
                case jdom: JDOMInfosetOutputter => writer.write(
                    new org.jdom2.output.XMLOutputter().outputString(jdom.getResult)
                  )
                case w3cdom: W3CDOMInfosetOutputter => {
                  val tf = TransformerFactory.newInstance()
                  val transformer = tf.newTransformer()
                  val result  = new StreamResult(writer)
                  val source  = new DOMSource(w3cdom.getResult)
                  transformer.transform(source, result)
                }
                case _ => // do nothing
              }

              writer.flush()

              // check for left over data (if we know the size up front, like for a file)
              optDataSize match {
                case Some(sz) => {
                  if (!loc.isAtEnd) {
                    log(LogLevel.Warning, "Left over data. Consumed %s bit(s) with %s bit(s) remaining.", loc.bitPos1b - 1, (sz * 8) - (loc.bitPos1b - 1))
                    true
                  } else false
                }
                case None => {
                  // we need to look at the internal state of the parser inStream to
                  // see how big it is. We do this after execution so as
                  // not to traverse the data twice.
                  val ps = parseResult.resultState.asInstanceOf[PState]
                  val dis = ps.dataInputStream
                  val hasMoreData = dis.isDefinedForLength(1) // do we have even 1 more bit?
                  if (hasMoreData) {
                    dis.setDecoder(StandardCharsets.ISO_8859_1.newDecoder())
                    val maybeString = dis.getSomeString(processor.getTunables.maxFieldContentLengthInBytes)
                    val lengthInBytes = if (maybeString.isEmpty) 0 else maybeString.get.length
                    if (lengthInBytes > 0)
                      log(LogLevel.Warning, "Left over data. Consumed %s bit(s) with %s bit(s) remaining.", loc.bitPos1b - 1, (lengthInBytes * 8))
                    else {
                      // less than 1 byte is available
                      log(LogLevel.Warning, "Left over data. Consumed %s bit(s) with less than one byte remaining.", loc.bitPos1b - 1)
                    }
                    true
                  } else false
                }
              }

              if (parseResult.isValidationError) 1 else 0
            }
          }
          case Some(processor) => 1
          case None => 1
        }
        rc
      }

      case Some(conf.performance) => {
        val performanceOpts = conf.performance

        val validate = performanceOpts.validate.get.get

        val cfgFileNode = performanceOpts.config.get match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }

        val processor = {
          if (performanceOpts.parser.isDefined) {
            val p = createProcessorFromParser(performanceOpts.parser(), performanceOpts.path.get, validate)
            p.get.setExternalVariables(retrieveExternalVariables(performanceOpts.vars, cfgFileNode, p.get.getTunables()))
            p
          } else {
            val tunables = retrieveTunables(performanceOpts.tunables, cfgFileNode)
            val tunablesObj = DaffodilTunables(tunables)
            val extVarsBindings = retrieveExternalVariables(performanceOpts.vars, cfgFileNode, tunablesObj)

            createProcessorFromSchema(performanceOpts.schema(), performanceOpts.rootNS.get, performanceOpts.path.get, extVarsBindings, tunables, validate)
          }
        }

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

            val infosetType = performanceOpts.infosetType.get.get

            val dataSeq = files.map { filePath =>
              val input = (new FileInputStream(filePath))
              val dataSize = filePath.length()
              val fileContent = new Array[Byte](dataSize.toInt)
              input.read(fileContent) // For performance testing, we want everything in memory so as to remove I/O from consideration.
              val data = performanceOpts.unparse() match {
                case true => infosetDataToInputterData(infosetType, fileContent)
                case false => fileContent
              }
              (filePath, data, dataSize * 8)
            }

            val inputs = (0 until performanceOpts.number()).map { n =>
              val index = n % dataSeq.length
              val (path, data, dataLen) = dataSeq(index)
              val inData = performanceOpts.unparse() match {
                case true => {
                  Left(data)
                }
                case false => {
                  val bais = new ByteArrayInputStream(data.asInstanceOf[Array[Byte]])
                  val channel = java.nio.channels.Channels.newChannel(bais);
                  Right(channel)
                }
              }
              (path, inData, dataLen)
            }
            val inputsWithIndex = inputs.zipWithIndex

            processor.setValidationMode(validate)

            implicit val executionContext = new ExecutionContext {
              val threadPool = Executors.newFixedThreadPool(performanceOpts.threads())

              def execute(runnable: Runnable) {
                threadPool.submit(runnable)
              }

              def reportFailure(t: Throwable) {}
            }

            val nullChannelForUnparse = java.nio.channels.Channels.newChannel(new NullOutputStream)
            val nullWriterForParse = new NullWriter()

            //the following line allows output verification
            //val nullChannelForUnparse = java.nio.channels.Channels.newChannel(System.out)
            val NSConvert = 1000000000.0
            val (totalTime, results) = Timer.getTimeResult({
              val tasks = inputsWithIndex.map {
                case (c, n) =>
                  val task: Future[(Int, Long, Boolean)] = Future {
                    val (_ /* path */ , inData, len) = c
                    val (time, result) = inData match {
                      case Left(anyRef) => Timer.getTimeResult({
                        val inputterForUnparse = getInfosetInputter(infosetType, anyRef)
                        processor.unparse(inputterForUnparse, nullChannelForUnparse)
                      })
                      case Right(channel) => Timer.getTimeResult({
                        val outputterForParse = getInfosetOutputter(infosetType, nullWriterForParse)
                        processor.parse(channel, outputterForParse, len)
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

        val validate = unparseOpts.validate.get.get

        val cfgFileNode = unparseOpts.config.get match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }

        val processor = {
          if (unparseOpts.parser.isDefined) {
            val p = createProcessorFromParser(unparseOpts.parser(), unparseOpts.path.get, validate)
            p.get.setExternalVariables(retrieveExternalVariables(unparseOpts.vars, cfgFileNode, p.get.getTunables()))
            p
          } else {
            val tunables = retrieveTunables(unparseOpts.tunables, cfgFileNode)
            val tunablesObj = DaffodilTunables(tunables)
            val extVarsBindings = retrieveExternalVariables(unparseOpts.vars, cfgFileNode, tunablesObj)

            createProcessorFromSchema(unparseOpts.schema(), unparseOpts.rootNS.get, unparseOpts.path.get, extVarsBindings, tunables, validate)
          }
        }

        val output = unparseOpts.output.get match {
          case Some("-") | None => System.out
          case Some(file) => new FileOutputStream(file)
        }

        val outChannel = java.nio.channels.Channels.newChannel(output)
        //
        // We are not loading a schema here, we're loading the infoset to unparse.
        //
        val is = unparseOpts.infile.get match {
          case Some("-") | None => System.in
          case Some(fileName) => new FileInputStream(fileName)
        }

        val rc = processor match {
          case None => 1
          case Some(processor) => {
            setupDebugOrTrace(processor.asInstanceOf[DataProcessor], conf)
            val data = IOUtils.toByteArray(is)
            val inputterData = infosetDataToInputterData(unparseOpts.infosetType.get.get, data)
            val inputter = getInfosetInputter(unparseOpts.infosetType.get.get, inputterData)
            val unparseResult = Timer.getResult("unparsing", processor.unparse(inputter, outChannel))
            output.close()
            displayDiagnostics(unparseResult)
            if (unparseResult.isError) 1 else 0
          }
        }
        rc
      }

      case Some(conf.save) => {
        val saveOpts = conf.save

        val validate = ValidationMode.Off

        val cfgFileNode = saveOpts.config.get match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }
        val tunables = retrieveTunables(saveOpts.tunables, cfgFileNode)
        val tunablesObj = DaffodilTunables(tunables)
        val extVarsBindings = retrieveExternalVariables(saveOpts.vars, cfgFileNode, tunablesObj)

        val processor = createProcessorFromSchema(saveOpts.schema(), saveOpts.rootNS.get, saveOpts.path.get, extVarsBindings, tunables, validate)

        val output = saveOpts.outfile.get match {
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
                val matches = tdmlRunner.testCases.filter(testCase => regex.pattern.matcher(testCase.name).matches)
                matches.map(testCase => (testCase.name, Some(testCase)))
              } else {
                List((testName, tdmlRunner.testCases.find(_.name == testName)))
              }
            })
          } else {
            tdmlRunner.testCases.map(test => (test.name, Some(test)))
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
                      maxVals(2).max(test.root.length),
                      maxVals(3).max(test.description.length))
                  }
                }
            }
            val formatStr = maxCols.map(max => "%" + -max + "s").mkString("  ")
            println(formatStr.format(headers: _*))
            tests.foreach { testPair =>
              testPair match {
                case (name, Some(test)) => println(formatStr.format(name, test.model, test.root, test.description))
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
                  case e: Throwable =>
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
                          |  https://cwiki.apache.org/confluence/display/DAFFODIL/How+to+Report+a+Bug
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

  def main(arguments: Array[String]): Unit = {
    val ret = try {
      run(arguments)
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case e: java.io.FileNotFoundException => {
        log(LogLevel.Error, "%s", e.getMessage())
        1
      }
      case e: TunableLimitExceededError => {
        log(LogLevel.Error, "%s", e.getMessage())
        1
      }
      case e: InvalidParserException => {
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
      case e: InvalidJavaVersionException => {
        log(LogLevel.Error, "%s", e.getMessage())
        1
      }
      case e: Exception => {
        bugFound(e)
      }
    }

    System.exit(ret)
  }
}
