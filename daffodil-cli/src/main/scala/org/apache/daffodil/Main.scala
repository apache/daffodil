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

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.io.PrintStream
import java.net.URI
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.file.Paths
import java.util.Scanner
import java.util.concurrent.Executors
import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.StreamResult

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.matching.Regex

import com.typesafe.config.ConfigFactory

import org.apache.commons.io.output.NullOutputStream

import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator

import org.rogach.scallop
import org.rogach.scallop.ArgType
import org.rogach.scallop.ScallopOption
import org.rogach.scallop.ValueConverter
import org.rogach.scallop.exceptions.GenericScallopException

import org.xml.sax.InputSource
import org.xml.sax.helpers.XMLReaderFactory

import com.siemens.ct.exi.core.EXIFactory
import com.siemens.ct.exi.core.exceptions.EXIException
import com.siemens.ct.exi.main.api.sax.EXIResult
import com.siemens.ct.exi.main.api.sax.EXISource

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilConfig
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.TDMLImplementation
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
import org.apache.daffodil.io.DataDumper
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.processors.DataLoc
import org.apache.daffodil.processors.ExternalVariableException
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.TDMLException
import org.apache.daffodil.tdml.TDMLTestNotCompatibleException
import org.apache.daffodil.udf.UserDefinedFunctionFatalErrorException
import org.apache.daffodil.util.Logger
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Timer
import org.apache.daffodil.validation.Validators
import org.apache.daffodil.xml.QName
import org.apache.daffodil.xml.RefQName


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

  implicit def implementationConverter = singleArgConverter[TDMLImplementation]((s: String) => {
    val optImplementation = TDMLImplementation.optionStringToEnum("implementation", s)
    if (!optImplementation.isDefined) {
      throw new Exception("Unrecognized TDML implementation '%s'.  Must be one of %s"
        .format(s, TDMLImplementation.values.mkString(", ")))
    }
    optImplementation.get
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
    throw new GenericScallopException(msg)
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
    val infosetType = opt[InfosetType.Type](short = 'I', argName = "infoset_type", descr = "Infoset type to output. Type can be: " + InfosetType.values.mkString(", ") + ". Defaults to 'xml'.", default = Some(InfosetType.XML))
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

    validateOpt(infosetType, stream, schema) {
      case (Some(InfosetType.EXI), Some(true), _) => Left("Streaming mode is not currently supported with EXI infosets.")
      case (Some(InfosetType.EXISA), Some(true), _) => Left("Streaming mode is not currently supported with EXI infosets.")
      case (Some(InfosetType.EXISA), _, None) => Left("A schema must be specified to use schema-aware compression with EXI")
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
    val infosetType = opt[InfosetType.Type](short = 'I', argName = "infoset_type", descr = "Infoset type to output. Type can be: " + InfosetType.values.mkString(", ") + ". Defaults to 'xml'.", default = Some(InfosetType.XML))
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

    validateOpt(infosetType, stream, schema) {
      case (Some(InfosetType.EXI), Some(true), _) => Left("Streaming mode is not currently supported with EXI infosets.")
      case (Some(InfosetType.EXISA), Some(true), _) => Left("Streaming mode is not currently supported with EXI infosets.")
      case (Some(InfosetType.EXISA), _, None) => Left("A schema must be specified to use schema-aware compression with EXI")
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
    banner("""|Usage: daffodil test [-I <implementation>] [-l] [-r] [-i] <tdmlfile> [testnames...]
              |
              |List or execute tests in a TDML file
              |
              |Test Options:""".stripMargin)

    descr("List or execute TDML tests")
    helpWidth(width)

    val implementation = opt[TDMLImplementation](short = 'I', argName = "implementation",
      descr = "Implementation to run TDML tests. Choose one of %s. Defaults to %s."
        .format(TDMLImplementation.values.mkString(", "), TDMLImplementation.Daffodil.toString),
      default = None)
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
    val infosetType = opt[InfosetType.Type](short = 'I', argName = "infoset_type", descr = "Infoset type to output. Type can be: " + InfosetType.values.mkString(", ") + ". Defaults to 'xml'.", default = Some(InfosetType.XML))
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

    validateOpt(infosetType, schema) {
      case (Some(InfosetType.EXISA), None) => Left("A schema must be specified to use schema-aware compression with EXI")
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

  // Encode or decode EXI Subcommand Options
  object exi extends scallop.Subcommand("exi") {
    banner("""|Usage: daffodil exi [-d] [-s <schema>] [-o <output>] [infile]
              |
              |Encode/decode an XML file with EXI. If a schema is specified, it will use schema aware encoding/decoding.
              |
              |EncodeEXI Options:""".stripMargin)

    descr("Encode an XML file with EXI")
    helpWidth(width)

    val output = opt[String](argName = "file", descr = "Output file to write the encoded/decoded file to. If not given or is -, data is written to stdout.")
    val schema = opt[URI]("schema", argName = "file", descr = "DFDL schema to use for schema aware encoding/decoding.")(fileResourceURIConverter)
    val decode = opt[Boolean](default = Some(false), descr = "Decode input file from EXI to XML.")
    val infile = trailArg[String](required = false, descr = "Input XML file to encode. If not specified, or a value of -, reads from stdin.")
  }

  addSubcommand(parse)
  addSubcommand(unparse)
  addSubcommand(save)
  addSubcommand(test)
  addSubcommand(performance)
  addSubcommand(generate)
  addSubcommand(exi)

  mutuallyExclusive(trace, debug) // cannot provide both --trace and --debug
  requireSubcommand()

  verify()
}

object ValidatorPatterns {
  val NoArgsPattern: Regex = "(.+?)".r.anchored
  val DefaultArgPattern: Regex = "(.+?)=(.+)".r.anchored
}

object Main {

  var STDIN: InputStream = System.in
  var STDOUT: PrintStream = System.out
  var STDERR: PrintStream = System.err

  /**
   * Allows changing where the input/output of the CLI goes to. This defaults
   * to the normal System.in/out/err, but can be changed to support easier
   * testing. This is not thread safe, but this Main object is not really
   * thread safe anyways if it is configured to read/write from stdin/stout.
   */
  def setInputOutput(in: InputStream, out: PrintStream, err: PrintStream): Unit = {
    STDIN = in
    STDOUT = out
    STDERR = err
  }

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
   * @param optDafConfig DaffodilConfig object from config file (if any)
   */
  def combineExternalVariables(vars: Map[String, String], optDafConfig: Option[DaffodilConfig]): Seq[Binding] = {
    val configFileVars: Seq[Binding] = optDafConfig.map{ _.externalVariableBindings }.getOrElse(Seq())

    val individualVars = ExternalVariablesLoader.mapToBindings(vars)

    val bindings = overrideBindings(individualVars, configFileVars)
    bindings
  }

  def createProcessorFromParser(savedParser: File, path: Option[String], mode: ValidationMode.Type) = {
    try {
      val compiler = Compiler()
      val processor = Timer.getResult("reloading", compiler.reload(savedParser))
      // note that we do not display the diagnostics that were saved as part of the
      // saved processor. Those are from compile time, are all warnings, and
      // are just noise in a production system where we're reloading a parser.
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

  def withDebugOrTrace(proc: DFDL.DataProcessor, conf: CLIConf): DFDL.DataProcessor = {
    (conf.trace() || conf.debug.isDefined) match {
      case true => {
        val runner =
          if (conf.trace()) {
            new TraceDebuggerRunner(STDOUT)
          } else {
            if (System.console == null) {
              Logger.log.warn(s"Using --debug on a non-interactive console may result in display issues")
            }
            conf.debug() match {
              case Some(f) => new CLIDebuggerRunner(new File(f), STDIN, STDOUT)
              case None => new CLIDebuggerRunner(STDIN, STDOUT)
            }
          }
        val id = new InteractiveDebugger(runner, ExpressionCompilers)
        proc.withDebugger(id).withDebugging(true)
      }
      case false => proc
    }
  }

  def createProcessorFromSchema(schema: URI, rootNS: Option[RefQName], path: Option[String],
    tunablesMap: Map[String, String],
    mode: ValidationMode.Type): Option[DFDL.DataProcessor] = {
    val compiler = {
      val c = Compiler().withTunables(tunablesMap)
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


  def setLogLevel(verbose: Int): Unit = {
    val verboseLevel = verbose match {
      case 0 => Level.WARN
      case 1 => Level.INFO
      case 2 => Level.DEBUG
      case _ => Level.TRACE
    }
    Configurator.setLevel("org.apache.daffodil", verboseLevel)
  }

  def runIgnoreExceptions(arguments: Array[String]): ExitCode.Value = {
    val conf = new CLIConf(arguments)

    setLogLevel(conf.verbose())

    val ret: ExitCode.Value = conf.subcommand match {

      case Some(conf.parse) => {
        val parseOpts = conf.parse

        val validate = parseOpts.validate.toOption.get

        val optDafConfig = parseOpts.config.toOption.map{ DaffodilConfig.fromFile(_) }

        val processor: Option[DFDL.DataProcessor] = {
          if (parseOpts.parser.isDefined) {
            createProcessorFromParser(parseOpts.parser(), parseOpts.path.toOption, validate)
          } else {
            val tunables = DaffodilTunables.configPlusMoreTunablesMap(parseOpts.tunables, optDafConfig)
            createProcessorFromSchema(parseOpts.schema(), parseOpts.rootNS.toOption, parseOpts.path.toOption, tunables, validate)
          }
        }.map{ _.withExternalVariables(combineExternalVariables(parseOpts.vars, optDafConfig)) }
         .map{ _.withValidationMode(validate) }
         .map{ withDebugOrTrace(_, conf) }

        val rc = processor match {
          case None => ExitCode.UnableToCreateProcessor
          case Some(processor) => {
            Assert.invariant(!processor.isError)
            val input = parseOpts.infile.toOption match {
              case Some("-") | None => STDIN
              case Some(file) => {
                val f = new File(file)
                new FileInputStream(f)
              }
            }
            val inStream = InputSourceDataInputStream(input)

            val output = parseOpts.output.toOption match {
              case Some("-") | None => STDOUT
              case Some(file) => new FileOutputStream(file)
            }

            val infosetType = parseOpts.infosetType.toOption.get
            val infosetHandler = InfosetType.getInfosetHandler(
              parseOpts.infosetType.toOption.get,
              processor,
              parseOpts.schema.toOption,
              forPerformance = false)

            var lastParseBitPosition = 0L
            var keepParsing = true
            var exitCode = ExitCode.Success

            while (keepParsing) {
              val infosetResult = Timer.getResult("parsing", infosetHandler.parse(inStream, output))
              val parseResult = infosetResult.parseResult

              val finfo = parseResult.resultState.asInstanceOf[FormatInfo]
              val loc = parseResult.resultState.currentLocation.asInstanceOf[DataLoc]
              displayDiagnostics(parseResult)

              if (parseResult.isProcessingError || parseResult.isValidationError) {
                keepParsing = false
                exitCode = ExitCode.ParseError
              } else {
                // Success. Some InfosetHandlers do not write the result to the output
                // stream when parsing (e.g. they just create Scala objects). Since we are
                // parsing, ask the InfosetHandler to serialize the result if it has an
                // implementation so that the user can see an XML represenation regardless
                // of the infoset type.
                infosetResult.write(output)
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

        val optDafConfig = performanceOpts.config.toOption.map{ DaffodilConfig.fromFile(_) }

        val processor = {
          if (performanceOpts.parser.isDefined) {
            createProcessorFromParser(performanceOpts.parser(), performanceOpts.path.toOption, validate)
          } else {
            val tunables = DaffodilTunables.configPlusMoreTunablesMap(performanceOpts.tunables, optDafConfig)
            createProcessorFromSchema(performanceOpts.schema(), performanceOpts.rootNS.toOption, performanceOpts.path.toOption, tunables, validate)
          }
        }.map{ _.withExternalVariables(combineExternalVariables(performanceOpts.vars, optDafConfig)) }
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
            val infosetHandler = InfosetType.getInfosetHandler(
              infosetType,
              processor,
              performanceOpts.schema.toOption,
              forPerformance = true)

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
                case true => Left(infosetHandler.dataToInfoset(bytes))
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

            val NSConvert = 1000000000.0
            val (totalTime, results) = Timer.getTimeResult({
              val tasks = inputsWithIndex.map {
                case (inData, n) =>
                  val task: Future[(Int, Long, Boolean)] = Future {
                    val (time, result) = inData match {
                      case Left(anyRef) => Timer.getTimeResult({
                        val unparseResult = infosetHandler.unparse(anyRef, nullChannelForUnparse)
                        unparseResult
                      })
                      case Right(bytes) => Timer.getTimeResult({
                        val input = InputSourceDataInputStream(bytes)
                        val infosetResult = infosetHandler.parse(input, nullOutputStreamForParse)
                        val parseResult = infosetResult.parseResult
                        parseResult
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
            STDOUT.println(s"total $action time (sec): $sec")
            STDOUT.println(s"min rate (files/sec): ${rates.min.toFloat}")
            STDOUT.println(s"max rate (files/sec): ${rates.max.toFloat}")
            STDOUT.println(s"avg rate (files/sec): ${(performanceOpts.number() / sec).toFloat}")

            if (numFailures == 0) ExitCode.Success else ExitCode.PerformanceTestError
          }

        }
        rc
      }

      case Some(conf.unparse) => {
        val unparseOpts = conf.unparse

        val validate = unparseOpts.validate.toOption.get

        val optDafConfig = unparseOpts.config.toOption.map{ DaffodilConfig.fromFile(_) }

        val processor = {
          if (unparseOpts.parser.isDefined) {
            createProcessorFromParser(unparseOpts.parser(), unparseOpts.path.toOption, validate)
          } else {
            val tunables = DaffodilTunables.configPlusMoreTunablesMap(unparseOpts.tunables, optDafConfig)
            createProcessorFromSchema(unparseOpts.schema(), unparseOpts.rootNS.toOption, unparseOpts.path.toOption, tunables, validate)
          }
        }.map{ _.withExternalVariables(combineExternalVariables(unparseOpts.vars, optDafConfig)) }
         .map{ _.withValidationMode(validate) }
         .map{ withDebugOrTrace(_, conf) }

        val output = unparseOpts.output.toOption match {
          case Some("-") | None => STDOUT
          case Some(file) => new FileOutputStream(file)
        }

        val outChannel = Channels.newChannel(output)
        //
        // We are not loading a schema here, we're loading the infoset to unparse.
        //
        val is = unparseOpts.infile.toOption match {
          case Some("-") | None => STDIN
          case Some(fileName) => new FileInputStream(fileName)
        }

        val rc = processor match {
          case None => ExitCode.UnableToCreateProcessor
          case Some(processor) => {
            Assert.invariant(processor.isError == false)

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

            val infosetType = unparseOpts.infosetType.toOption.get
            val infosetHandler = InfosetType.getInfosetHandler(
              unparseOpts.infosetType.toOption.get,
              processor,
              unparseOpts.schema.toOption,
              forPerformance = false)

            while (keepUnparsing) {

              val inputterData =
                if (maybeScanner.isDefined) {
                  // The scanner reads the entire infoset up unto the delimiter
                  // into memory. No way around that with the --stream option.
                  val bytes = maybeScanner.get.next().getBytes()
                  infosetHandler.dataToInfoset(bytes)
                } else {
                  // We are not using the --stream option and won't need to
                  // unparse the infoset more than once. So pass the
                  // InputStream into dataToInfoset. For some cases, such as
                  // "xml" or "json", we can create an InfosetInputter directly
                  // on this stream so that we can avoid reading the entire
                  // InputStream into memory
                  infosetHandler.dataToInfoset(is)
                }
              val unparseResult = Timer.getResult("unparsing", infosetHandler.unparse(inputterData, outChannel))

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
        val optDafConfig = saveOpts.config.toOption.map{ DaffodilConfig.fromFile(_) }

        val tunables = DaffodilTunables.configPlusMoreTunablesMap(saveOpts.tunables, optDafConfig)
        val tunablesObj = DaffodilTunables(tunables)

        val processor = createProcessorFromSchema(saveOpts.schema(), saveOpts.rootNS.toOption, saveOpts.path.toOption, tunables, validate)

        val output = saveOpts.outfile.toOption match {
          case Some("-") | None => Channels.newChannel(STDOUT)
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
        val optTDMLImplementation = testOpts.implementation.toOption
        val tdmlRunner = Runner(tdmlFile, optTDMLImplementation)

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
            // determine the max lengths of the various pieces of a test
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
            STDOUT.println(formatStr.format(headers: _*))
            tests.foreach { testPair =>
              testPair match {
                case (name, Some(test)) => STDOUT.println(formatStr.format(name, test.model, test.rootName, test.description))
                case (name, None) => STDOUT.println(formatStr.format(name, "[Not Found]", "", ""))
              }
            }
          } else {
            tests.foreach { testPair =>
              testPair match {
                case (name, Some(test)) => STDOUT.println(name)
                case (name, None) => STDOUT.println("%s  [Not Found]".format(name))
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
                  STDOUT.println("[Pass] %s".format(name))
                  pass += 1
                } catch {
                  case s: scala.util.control.ControlThrowable => throw s
                  case u: UnsuppressableException => throw u
                  case e: TDMLTestNotCompatibleException => {
                    STDOUT.println("[Skipped] %s (not compatible with implementation: %s)"
                      .format(name, e.implementation.getOrElse("<none>")))
                  }
                  case e: Throwable => {
                    e.getCause match {
                      case e: TDMLTestNotCompatibleException => {
                        // if JUnit is on the classpath (e.g. possible in integration tests), then
                        // the TDML runner throws an AssumptionViolatedException where the cause is
                        // a TDMLTestNotCompatibleException. In that case we should output the test
                        // skipped message. The way we match here avoids having the CLI to require
                        // JUnit as a dependency
                        STDOUT.println("[Skipped] %s (not compatible with implementation: %s)"
                          .format(name, e.implementation.getOrElse("<none>")))
                      }
                      case _ => {
                        STDOUT.println("[Fail] %s".format(name))
                        fail += 1
                        if (testOpts.info() > 0) {
                          STDOUT.println("  Failure Information:")
                          STDOUT.println(indent(e.getMessage(), 4))
                        }
                        if (testOpts.info() > 1) {
                          STDOUT.println("  Backtrace:")
                          e.getStackTrace.foreach { st => STDOUT.println(indent(st.toString, 4)) }
                        }
                      }
                    }
                  }
                }
              }

              case (name, None) => {
                STDOUT.println("[Not Found] %s".format(name))
                notfound += 1
              }
            }
          }
          STDOUT.println("")
          STDOUT.println("Total: %d, Pass: %d, Fail: %d, Not Found: %s".format(pass + fail + notfound, pass, fail, notfound))

          if (fail == 0) ExitCode.Success else ExitCode.TestError
        }
      }

      case Some(conf.generate) => {
        conf.subcommands match {
          case List(conf.generate, conf.generate.c) => {
            val generateOpts = conf.generate.c

            // Read any config file and any tunables given as arguments
            val optDafConfig = generateOpts.config.toOption.map{ DaffodilConfig.fromFile(_) }

            val tunables = DaffodilTunables.configPlusMoreTunablesMap(generateOpts.tunables, optDafConfig)

            // Create a CodeGenerator from the DFDL schema
            val generator = createGeneratorFromSchema(generateOpts.schema(), generateOpts.rootNS.toOption,
              tunables, generateOpts.language)

            // Ask the CodeGenerator to generate source code from the DFDL schema
            val outputDir = generateOpts.outdir.toOption.getOrElse(".")
            val rc = generator match {
              case Some(generator) => {
                Timer.getResult("generating", generator.generateCode(outputDir))
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

      case Some(conf.exi) => {
        var rc = ExitCode.Success
        val exiOpts = conf.exi
        val channel = exiOpts.output.toOption match {
          case Some("-") | None => Channels.newChannel(STDOUT)
          case Some(file) => new FileOutputStream(file).getChannel()
        }
        val output = Channels.newOutputStream(channel)

        val inputStream = exiOpts.infile.toOption match {
          case Some("-") | None => STDIN
          case Some(file) => {
            val f = new File(file)
            new FileInputStream(f)
          }
        }
        val input = new InputSource(inputStream)

        val exiFactory: Option[EXIFactory] = try {
          Some(EXIInfosetHandler.createEXIFactory(exiOpts.schema.toOption))
        } catch {
          case e: EXIException => {
            Logger.log.error(s"Error creating EXI grammar for the supplied schema: ${ Misc.getSomeMessage(e).get }")
            rc = ExitCode.Failure
            None
          }
        }

        (exiOpts.decode.toOption.get, exiFactory.isDefined) match {
          case (true, true) => { // Decoding
            val exiSource = new EXISource(exiFactory.get)
            exiSource.setInputSource(input)

            val result = new StreamResult(output)
            val tf = TransformerFactory.newInstance()
            val transformer = tf.newTransformer
            try {
              transformer.transform(exiSource, result)
            } catch {
              /* We catch a generic Exception here as Exificient will attempt
               * to decode anything and will throw very generic errors, such as
               * an IllegalArgumentException when it runs into a series of bytes
               * that aren't a Unicode codepoint. */
              case e: Exception => {
                Logger.log.error(s"Error decoding EXI input: ${ Misc.getSomeMessage(e).get }")
                rc = ExitCode.Failure
              }
            }
          }
          case (false, true) => { // Encoding
            val exiResult = new EXIResult(exiFactory.get)
            exiResult.setOutputStream(output)

            val reader = XMLReaderFactory.createXMLReader()
            reader.setContentHandler(exiResult.getHandler)
            try {
              reader.parse(input)
            } catch {
              case s: org.xml.sax.SAXException => {
                Logger.log.error(s"Error parsing input XML: ${ Misc.getSomeMessage(s).get }")
                rc = ExitCode.Failure
              }
            }
          }
          case (_, false) => // Hit an exception creating exiFactory, rc already set
        }

        inputStream.close
        output.close
        rc
      }

      // Required to avoid "match may not be exhaustive", but should never happen
      case _ => Assert.impossible()
    }

    ret
  }

  def bugFound(e: Exception): Int = {
    STDERR.println("""|
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
    STDERR.println("""|
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
    STDERR.println("""|
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


  def run(arguments: Array[String]): ExitCode.Value = {
    val ret = try {
      runIgnoreExceptions(arguments)
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
      case e: GenericScallopException => {
        Logger.log.error(e.message)
        ExitCode.Usage
      }
      case e: Exception => {
        bugFound(e)
        ExitCode.BugFound
      }
    }
    ret
  }

  def main(arguments: Array[String]): Unit = {
    val exitCode = run(arguments)
    System.exit(exitCode.id)
  }
}
