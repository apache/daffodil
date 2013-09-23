package edu.illinois.ncsa.daffodil

/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileInputStream
import java.io.File
import scala.xml.SAXParseException
import org.rogach.scallop
import edu.illinois.ncsa.daffodil.debugger.{ Debugger, InteractiveDebugger, TraceDebuggerRunner, CLIDebuggerRunner }
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Timer
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.util.Glob
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.LogWriter
import edu.illinois.ncsa.daffodil.util.LoggingDefaults
import edu.illinois.ncsa.daffodil.exceptions.NotYetImplementedException
import java.io.File
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.api.ValidationMode
import scala.xml.Node
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.configuration.ConfigurationLoader
import edu.illinois.ncsa.daffodil.api.ValidationMode

class CommandLineXMLLoaderErrorHandler() extends org.xml.sax.ErrorHandler with Logging {

  def warning(exception: SAXParseException) = {
    log(LogLevel.Warning, "loading schema: " + exception.getMessage)
  }

  def error(exception: SAXParseException) = {
    log(LogLevel.Error, "loading schema: " + exception.getMessage)
    System.exit(1)
  }

  def fatalError(exception: SAXParseException) = {
    log(LogLevel.Error, "loading schema: " + exception.getMessage)
    System.exit(1)
  }
}

trait CLILogPrefix extends LogWriter {
  override def prefix(logID: String, glob: Glob): String = {
    "[" + glob.lvl.toString.toLowerCase + "] "
  }

  override def suffix(logID: String, glob: Glob): String = {
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

  // This essentially reimplements the listArgConverter in scallop to not
  // allow a list of options to follow the option. For example, the
  // following is illegal: --schema foo bar. Instead, it must be --schema
  // foo --schema bar. It does this by copying listArgConverter, but
  // setting the argType to SINGLE.
  def singleListArgConverter[A](conv: String => A)(implicit m: Manifest[List[A]]) = new scallop.ValueConverter[List[A]] {
    def parse(s: List[(String, List[String])]) = {
      try {
        // this happens when options are provided after a trailing arg
        assert(s.forall(_._2.size == 1))

        val l = s.map(_._2).flatten.map(i => conv(i))
        if (l.isEmpty) Right(Some(Nil))
        else Right(Some(l))
      } catch {
        case _: Throwable => Left(Unit)
      }
    }
    val manifest = m
    val argType = scallop.ArgType.SINGLE
  }

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
  def optionalValueConverter[A](conv: String => A)(implicit m: Manifest[Option[A]]) = new scallop.ValueConverter[Option[A]] {

    // From the Scallop wiki:
    //
    // parse is a method, that takes a list of arguments to all option invocations:
    // for example, "-a 1 2 -a 3 4 5" would produce List(List(1,2),List(3,4,5)).
    // parse returns Left with error message, if there was an error while parsing
    // if no option was found, it returns Right(None)
    // and if option was found, it returns Right(...)
    def parse(s: List[(String, List[String])]): Either[Unit, Option[Option[A]]] = {
      s match {
        case Nil => Right(None) // --validate flag was not present
        case (_, Nil) :: Nil => Right(Some(None)) // --validate flag was present but 'mode' wasn't
        case (_, v :: Nil) :: Nil => { // --validate [mode] was present, perform the conversion
          try {
            Right(Some(Some(conv(v))))
          } catch {
            case e: Exception => {
              Left(e.getMessage())
            }
          }
        }
        case _ => Left(Unit) // Error because we expect there to be at most one --validate flag
      }
    }
    val manifest = m
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

  def validateConverter(s: String) = {
    s.toLowerCase match {
      case "on" => ValidationMode.Full
      case "limited" => ValidationMode.Limited
      case "off" => ValidationMode.Off
      case "" => ValidationMode.Full // Is this even possible?
      case _ => throw new Exception("Unrecognized ValidationMode %s.  Must be 'on', 'limited' or 'off'.".format(s))
    }
  }

  printedName = "daffodil"

  helpWidth(76)

  def error(msg: String) = errorMessageHandler(msg)

  errorMessageHandler = { message =>
    val msg =
      if (message.indexOf("Wrong format for option 'schema'") >= 0) {
        // the 'wrong format' error only occurs on --schema when options are
        // provided after the trailing arg, so let's give a more helpful error
        // message
        "Options are not allow after a trailing argument"
      } else {
        message
      }

    log(LogLevel.Error, "%s", msg)
    sys.exit(1)
  }

  banner("""|Usage: %s [GLOBAL_OPTS] <subcommand> [SUBCOMMAND_OPTS]
            |
            |Global Options:""".format(printedName).stripMargin)

  footer("""|
            |Run '%s <subcommand> --help' for subcommand specific options""".format(printedName).stripMargin)

  version({
    val versions = Misc.getDaffodilVersion
    val strVers = "%s %s (build %s)".format(printedName, versions._1, versions._2)
    strVers
  })

  shortSubcommandsHelp()

  // Global Options
  val debug = opt[Option[String]](argName = "file", descr = "enable debugging. Optionally, read initial debugger commands from [file] if provided.")(optionalValueConverter[String](a => a))
  val trace = opt[Boolean](descr = "run the debugger with verbose trace output")
  val verbose = tally(descr = "increment verbosity level, one level for each -v")

  // Parse Subcommand Options
  val parse = new scallop.Subcommand("parse") {
    banner("""|Usage: daffodil parse (-s <schema>... [-r <root> [-n <namespace>]] [-p <path>] |
              |                       -P <parser>)
              |                      [--validate [mode]]
              |                      [-D[{namespace}]<variable>=<value>...] [-o <output>]
              |                      [-c <file>] [infile]
              |
              |Parse a file, using either a DFDL schema or a saved parser
              |
              |Parse Options:""".stripMargin)

    descr("parse data to a DFDL infoset")
    helpWidth(76)

    val schemas = opt[List[String]]("schema", argName = "file", descr = "the annotated DFDL schema to use to create the parser. May be supplied multiple times for multi-schema support.")(singleListArgConverter[String](a => a))
    val root = opt[String](argName = "node", descr = "the root element of the XML file to use. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema")
    val namespace = opt[String](argName = "ns", descr = "the namespace of the root element. Requires --root.")
    val path = opt[String](argName = "path", descr = "path to the node to create parser.")
    val parser = opt[String](short = 'P', argName = "file", descr = "use a previously saved parser.")
    val output = opt[String](argName = "file", descr = "write output to a given file. If not given or is -, output is written to stdout.")
    val validate = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'. Defaults to 'on' if mode is not supplied.")(optionalValueConverter[ValidationMode.Type](a => validateConverter(a)).map {
      case None => ValidationMode.Full
      case Some(mode) => mode
    })
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when parsing. An option namespace may be provided.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")
    val infile = trailArg[String](required = false, descr = "input file to parse. If not specified, or a value of -, reads from stdin.")

    validateOpt(debug, infile) {
      case (Some(None), Some("-")) | (Some(None), None) => Left("Input must not be stdin during interactive debugging")
      case _ => Right(Unit)
    }

    validateOpt(schemas, parser, root, namespace) {
      case (Some(Nil), None, _, _) => Left("One of --schema or --parser must be defined")
      case (Some(_ :: _), Some(_), _, _) => Left("Only one of --parser and --schema may be defined")
      case (Some(_ :: _), None, None, Some(_)) => Left("--root must be defined if --namespace is defined")
      case (None, Some(_), Some(_), _) => Left("--root cannot be defined with --parser")
      case (None, Some(_), _, Some(_)) => Left("--namespace cannot be defined with --parser")
      case _ => Right(Unit)
    }

  }

  // Unparse Subcommand Options
  val unparse = new scallop.Subcommand("unparse") {
    banner("""|Usage: daffodil unparse (-s <schema>... [-r <root> [-n <namespace>]] [-p <path>] |
              |                         -P <parser>)
              |                        [--validate [mode]]
              |                        [-D[{namespace}]<variable>=<value>...] [-c <file>]
              |                        [-o <output>] [infile]
              |
              |Unparse an infoset file, using either a DFDL schema or a saved paser
              |
              |Unparse Options:""".stripMargin)

    descr("unparse a DFDL infoset")
    helpWidth(76)

    val schemas = opt[List[String]]("schema", argName = "file", descr = "the annotated DFDL schema to use to create the parser. May be supplied multiple times for multi-schema support.")(singleListArgConverter[String](a => a))
    val root = opt[String](argName = "node", descr = "the root element of the XML file to use. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema")
    val namespace = opt[String](argName = "ns", descr = "the namespace of the root element. Requires --root.")
    val path = opt[String](argName = "path", descr = "path to the node to create parser.")
    val parser = opt[String](short = 'P', argName = "file", descr = "use a previously saved parser.")
    val output = opt[String](argName = "file", descr = "write output to file. If not given or is -, output is written to standard output.")
    val validate = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'. Defaults to 'on' if mode is not supplied.")(optionalValueConverter[ValidationMode.Type](a => validateConverter(a)).map {
      case None => ValidationMode.Full
      case Some(mode) => mode
    })
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when unparsing. An optional namespace may be provided.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")
    val infile = trailArg[String](required = false, descr = "input file to unparse. If not specified, or a value of -, reads from stdin.")

    validateOpt(debug, infile) {
      case (Some(None), Some("-")) | (Some(None), None) => Left("Input must not be stdin during interactive debugging")
      case _ => Right(Unit)
    }

    validateOpt(schemas, parser, root, namespace) {
      case (Some(Nil), None, _, _) => Left("One of --schema or --parser must be defined")
      case (Some(_ :: _), Some(_), _, _) => Left("Only one of --parser and --schema may be defined")
      case (Some(_ :: _), None, None, Some(_)) => Left("--root must be defined if --namespace is defined")
      case (None, Some(_), Some(_), _) => Left("--root cannot be defined with --parser")
      case (None, Some(_), _, Some(_)) => Left("--namespace cannot be defined with --parser")
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
  }

  // Save Subcommand Options
  val save = new scallop.Subcommand("save-parser") {
    banner("""|Usage: daffodil save-parser -s <schema>... [-r <root> [-n <namespace>]]
              |                            [-p <path>] [outfile]
              |                            [--validate [mode]] 
              |                            [-D[{namespace}]<variable>=<value>...]
              |                            [-c <file>]
              |
              |Create and save a parser using a DFDL schema
              |
              |Save Parser Options:""".stripMargin)

    descr("save a daffodil parser for reuse")
    helpWidth(76)

    val schemas = opt[List[String]]("schema", argName = "file", required = true, descr = "the annotated DFDL schema to use to create the parser. May be supplied multiple times for multi-schema support.")(singleListArgConverter[String](a => a))
    val root = opt[String](argName = "node", descr = "the root element of the XML file to use. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema.")
    val namespace = opt[String](argName = "ns", descr = "the namespace of the root element. Requires --root.")
    val path = opt[String](argName = "path", descr = "path to the node to create parser.")
    val outfile = trailArg[String](required = false, descr = "output file to save parser. If not specified, or a value of -, writes to stdout.")
    val validate = opt[ValidationMode.Type](short = 'V', default = Some(ValidationMode.Off), argName = "mode", descr = "the validation mode. 'on', 'limited' or 'off'. Defaults to 'on' if mode is not supplied.")(optionalValueConverter[ValidationMode.Type](a => validateConverter(a)).map {
      case None => ValidationMode.Full
      case Some(mode) => mode
    })
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used.")
    val config = opt[String](short = 'c', argName = "file", descr = "path to file containing configuration items.")

    validateOpt(schemas, root, namespace, outfile) {
      case (Some(Nil), _, _, _) => Left("No schemas specified using the --schema option")
      case (_, None, Some(_), _) => Left("--root must be defined if --namespace is defined")
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

  def createProcessorFromParser(parseFile: String, path: Option[String], mode: ValidationMode.Type) = {
    val compiler = Compiler()
    val processorFactory = Timer.getResult("reloading", compiler.reload(parseFile))
    if (processorFactory.canProceed) {
      val processor = processorFactory.onPath(path.getOrElse("/"))
      processor.setValidationMode(mode)
      Some(processor)
    } else None
  }

  /**
   * Loads and validates the configuration file.
   *
   * @param pathName The path to the file.
   * @return The Node representation of the file.
   */
  def loadConfigurationFile(pathName: String) = {
    val node = ConfigurationLoader.getConfiguration(pathName)
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
          case Some(extVarBindingsNode) => ExternalVariablesLoader.getVariables(extVarBindingsNode)
        }
      }
    }

    val individualVars = ExternalVariablesLoader.getVariables(vars)

    val bindings = overrideBindings(individualVars, configFileVars)
    bindings
  }

  def createProcessorFromSchemas(schemaFiles: List[File], root: Option[String],
    namespace: Option[String], path: Option[String],
    extVars: Seq[Binding],
    mode: ValidationMode.Type) = {
    val compiler = Compiler()

    val ns = namespace.getOrElse(null)

    compiler.setExternalDFDLVariables(extVars)

    root match {
      case Some(r) => {
        compiler.setDistinguishedRootNode(r, ns)
      }
      case None => //ignore 
    }

    // Wrap timing around the whole of compilation
    //
    // compilation extends from the call to compile
    // to also include the call to pf.onPath. (which is the last phase 
    // of compilation, where it asks for the parser)
    //
    val pf = Timer.getResult("compiling", {
      val processorFactory = compiler.compile(schemaFiles: _*)
      if (processorFactory.canProceed) {
        val processor = processorFactory.onPath(path.getOrElse("/"))
        processor.setValidationMode(mode)
        Some(processor) // note: processor could still be isError == true
        // but we do definitely get a processor.
      } else
        None
    })
    pf
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
      Debugger.setDebugging(true)
      Debugger.setDebugger(new InteractiveDebugger(runner))
    }

    val ret = conf.subcommand match {

      case Some(conf.parse) => {
        val parseOpts = conf.parse

        val validate = parseOpts.validate.get.get

        val cfgFileNode = parseOpts.config.get match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }
        val extVarsBindings = retrieveExternalVariables(parseOpts.vars, cfgFileNode)

        val processor = {
          if (parseOpts.parser.isDefined) {
            createProcessorFromParser(parseOpts.parser(), parseOpts.path.get, validate)
          } else {
            val files: List[File] = parseOpts.schemas().map(s => new File(s))
            createProcessorFromSchemas(files, parseOpts.root.get, parseOpts.namespace.get, parseOpts.path.get, extVarsBindings, validate)
          }
        }

        def displayDiagnostics(lvl: LogLevel.Type, pr: WithDiagnostics) {
          pr.getDiagnostics.foreach { d =>
            log(lvl, "%s", d.getMessage())
          }
        }

        val rc = processor match {
          case Some(processor) if (processor.canProceed) => {
            val (input, optDataSize) = parseOpts.infile.get match {
              case Some("-") | None => (System.in, None)
              case Some(file) => {
                val f = new File(parseOpts.infile())
                (new FileInputStream(f), Some(f.length()))
              }
            }
            val inChannel = java.nio.channels.Channels.newChannel(input);

            processor.setValidationMode(validate)

            val parseResult = Timer.getResult("parsing",
              optDataSize match {
                case None => processor.parse(inChannel)
                case Some(szInBytes) => processor.parse(inChannel, szInBytes * 8)
              })
            val loc = parseResult.resultState.currentLocation
            if (parseResult.isError) {
              displayDiagnostics(LogLevel.Error, parseResult)
              1
            } else {
              displayDiagnostics(LogLevel.Warning, parseResult) // displays any warnings (should be no errors)
              val output = parseOpts.output.get match {
                case Some("-") | None => System.out
                case Some(file) => new FileOutputStream(file)
              }
              // check for left over data (if we know the size up front, like for a file)
              val hasLeftOverData = optDataSize match {
                case Some(sz) => {
                  if (!loc.isAtEnd) {
                    log(LogLevel.Error, "Left over data. %s bytes available. Location: %s", sz, loc)
                    true
                  } else false
                }
                case None => {
                  // we need to look at the internal state of the parser inStream to 
                  // see how big it is. We do this after execution so as
                  // not to traverse the data twice.
                  val lengthInBytes = parseResult.resultState.lengthInBytes
                  val positionInBytes = loc.bytePos
                  if (positionInBytes != lengthInBytes) {
                    log(LogLevel.Error, "Left over data. %s bytes available. Location: %s", lengthInBytes, loc)
                    true
                  } else false
                }
              }
              val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(output));

              val pp = new scala.xml.PrettyPrinter(80, 2)
              Timer.getResult("writing", writer.write(pp.format(parseResult.result) + "\n"))
              writer.flush()
              if (hasLeftOverData) 1 else 0
            }
          }
          case Some(processor) => 1
          case None => 1
        }
        rc
      }

      case Some(conf.unparse) => {
        val unparseOpts = conf.unparse

        val validate = unparseOpts.validate.get match {
          case None => ValidationMode.Off
          case Some(vMode) => vMode
        }

        val cfgFileNode = unparseOpts.config.get match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }
        val extVarsBindings = retrieveExternalVariables(unparseOpts.vars, cfgFileNode)

        val processor = {
          if (unparseOpts.parser.isDefined) {
            createProcessorFromParser(unparseOpts.parser(), unparseOpts.path.get, validate)
          } else {
            val files: List[File] = unparseOpts.schemas().map(s => new File(s))
            createProcessorFromSchemas(files, unparseOpts.root.get, unparseOpts.namespace.get, unparseOpts.path.get, extVarsBindings, validate)
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
        val dataLoader = new DaffodilXMLLoader(new CommandLineXMLLoaderErrorHandler)
        dataLoader.setValidation(true) //TODO: make this flag an option. 
        val document = unparseOpts.infile.get match {
          case Some("-") | None => dataLoader.load(System.in)
          case Some(file) => dataLoader.loadFile(file)
        }
        val rc = processor match {
          case None => 1
          case Some(processor) => {
            val unparseResult = Timer.getResult("unparsing", processor.unparse(outChannel, document))
            output.close()
            if (unparseResult.isError) 1 else 0
          }
        }
        rc
      }

      case Some(conf.save) => {
        val saveOpts = conf.save

        val files: List[File] = saveOpts.schemas().map(s => new File(s))

        val validate = saveOpts.validate.get match {
          case None => ValidationMode.Off
          case Some(vMode) => vMode
        }

        val cfgFileNode = saveOpts.config.get match {
          case None => None
          case Some(pathToConfig) => Some(this.loadConfigurationFile(pathToConfig))
        }
        val extVarsBindings = retrieveExternalVariables(saveOpts.vars, cfgFileNode)

        val processor = createProcessorFromSchemas(files, saveOpts.root.get, saveOpts.namespace.get, saveOpts.path.get, extVarsBindings, validate)

        val output = saveOpts.outfile.get match {
          case Some("-") | None => System.out
          case Some(file) => new FileOutputStream(file)
        }

        val outChannel = java.nio.channels.Channels.newChannel(output)
        val rc = processor match {
          case Some(processor) => {
            Timer.getResult("saving", processor.save(outChannel))
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
                  case e: Throwable =>
                    println("[Fail] %s".format(name))
                    fail += 1
                    if (testOpts.info() > 0) {
                      println("  Failure Information:")
                      println(indent(e.getMessage, 4))
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
        1
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
                          |  https://opensource.ncsa.illinois.edu/confluence/display/DFDL/How+to+Report+a+Bug
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
                          |  https://opensource.ncsa.illinois.edu/jira/browse/DFDL
                          |
                          |""".format(e.getMessage).stripMargin)
    1
  }

  def main(arguments: Array[String]): Unit = {
    val ret = try {
      run(arguments)
    } catch {
      case e: java.io.FileNotFoundException => {
        log(LogLevel.Error, "%s", e.getMessage)
        1
      }
      case e: NotYetImplementedException => {
        nyiFound(e)
      }
      case e: Exception => {
        bugFound(e)
      }
    }

    System.exit(ret)
  }
}
