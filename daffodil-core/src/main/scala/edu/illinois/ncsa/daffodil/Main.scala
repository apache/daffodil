package edu.illinois.ncsa.daffodil

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

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

import java.io.ByteArrayInputStream
import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileInputStream
import java.io.File
import scala.xml.SAXParseException
import org.rogach.scallop
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Timer
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.debugger.InteractiveDebugger
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.util.Glob
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.LogWriter
import edu.illinois.ncsa.daffodil.util.LoggingDefaults
import edu.illinois.ncsa.daffodil.exceptions.NotYetImplementedException
import java.io.File

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

object CLILogWriter extends LogWriter {
  override def prefix(logID: String, glob: Glob): String = {
    "[" + glob.lvl.toString.toLowerCase + "] "
  }

  override def suffix(logID: String, glob: Glob): String = {
    ""
  }

  def write(msg: String) {
    Console.err.println(msg)
    Console.flush
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

  def optionalValueConverter[A](conv: String => A)(implicit m: Manifest[Option[A]]) = new scallop.ValueConverter[Option[A]] {
    def parse(s: List[(String, List[String])]) = {
      s match {
        case Nil => Right(None)
        case (_, Nil) :: Nil => Right(Some(None))
        case (_, v :: Nil) :: Nil => Right(Some(Some(conv(v))))
        case _ => Left(Unit)
      }
    }
    val manifest = m
    val argType = scallop.ArgType.LIST
    override def argFormat(name: String): String = "[" + name + "]"
  }

  def validateConf(c1: => Option[scallop.ScallopConf])
    (fn: (Option[scallop.ScallopConf]) => Either[String,Unit]) {
      validations :+= new Function0[Either[String,Unit]] {
      def apply = {
        fn(c1)
      }
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
  val debug = opt[Option[String]](argName="file", descr = "enable debugging. Optionally, read initial debugger commands from [file] if provided.")(optionalValueConverter[String](a => a))
  val trace = opt[Boolean](descr = "run the debugger with verbose trace output")
  val verbose = tally(descr = "increment verbosity level, one level for each -v")

  // Parse Subcommand Options
  val parse = new scallop.Subcommand("parse") {
    banner("""|Usage: daffodil parse (-s <schema>... [-r <root> [-n <namespace>]] [-p <path>] |
              |                       -P <parser>)
              |                      [-D<variable>=<value>...] [-o <output>] [infile]
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
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when parsing.")
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
              |                        [-D<variable>=<value>...] [-o <output>] [infile]
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
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used when unparsing.")
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
  }

  // Save Subcommand Options
  val save = new scallop.Subcommand("save-parser") {
    banner("""|Usage: daffodil save-parser -s <schema>... [-r <root> [-n <namespace>]]
              |                            [-p <path>] [outfile]
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
    val vars = props[String]('D', keyName = "variable", valueName = "value", descr = "variables to be used.")

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

  def createProcessorFromParser(parseFile: String, path: Option[String]) = {
    val compiler = Compiler()
    val processorFactory = Timer.getResult("reloading", compiler.reload(parseFile))
    if (processorFactory.canProceed) {
      val processor = processorFactory.onPath(path.getOrElse("/"))
      Some(processor)
    } else None
  }

  def createProcessorFromSchemas(schemaFiles: List[File], root: Option[String], namespace: Option[String], path: Option[String], vars: Map[String, String]) = {
    val compiler = Compiler()

    val ns = namespace.getOrElse(null)

    vars foreach { case (key, value) => compiler.setExternalDFDLVariable(key, ns, value) }

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

    if (conf.trace()) {
      Debugger.setDebugging(true)
      Debugger.setDebugger(new InteractiveDebugger(traceCommands))
    } else if (conf.debug.isDefined) {
      Debugger.setDebugging(true)
      val debugger = conf.debug() match {
        case Some(f) => new InteractiveDebugger(new File(f))
        case None => new InteractiveDebugger()
      }
      Debugger.setDebugger(debugger)
    }

    val ret = conf.subcommand match {

      case Some(conf.parse) => {
        val parseOpts = conf.parse

        val processor = {
          if (parseOpts.parser.isDefined) {
            createProcessorFromParser(parseOpts.parser(), parseOpts.path.get)
          } else {
            val files: List[File] = parseOpts.schemas().map(s => new File(s))

            createProcessorFromSchemas(files, parseOpts.root.get, parseOpts.namespace.get, parseOpts.path.get, parseOpts.vars)
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
          case None => 1
        }
        rc
      }

      case Some(conf.unparse) => {
        val unparseOpts = conf.unparse

        val processor = {
          if (unparseOpts.parser.isDefined) {
            createProcessorFromParser(unparseOpts.parser(), unparseOpts.path.get)
          } else {
            val files: List[File] = unparseOpts.schemas().map(s => new File(s))
            createProcessorFromSchemas(files, unparseOpts.root.get, unparseOpts.namespace.get, unparseOpts.path.get, unparseOpts.vars)
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

        val processor = createProcessorFromSchemas(files, saveOpts.root.get, saveOpts.namespace.get, saveOpts.path.get, saveOpts.vars)

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
          if (conf.verbose() > 0) {
            // determine the max lengths of the various pieces of atest
            val headers = List("Name", "Model", "Root", "Description")
            val maxCols = tests.foldLeft(headers.map(_.length)) {
              (maxVals, testPair) =>
                {
                  testPair match {
                    case (name, None) => maxVals
                    case (name, Some(test)) => List(maxVals(0).max(name.length),
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
                case (name, None) => println("%s [Missing]".format(name))
              }
            }
          } else {
            tests.foreach { testPair =>
              testPair match {
                case (name, Some(test)) => println(name)
                case (name, None) => println("%s [Missing]".format(name))
              }
            }
          }
        } else {
          tests.foreach { testPair =>
            testPair match {
              case (name, Some(test)) => {
                var success = true
                try {
                  test.run()
                } catch {
                  case _: Throwable =>
                    success = false
                    if (System.console != null)
                      print("[\033[31mFail\033[0m]")
                    else
                      print("Fail")
                }
                if (success) {
                  if (System.console != null)
                    print("[\033[32mPass\033[0m]")
                  else
                    print("Pass")
                }
                println(" %s".format(name))
              }

              case (name, None) => {
                if (System.console != null)
                  print("[\033[33mMissing\033[0m]")
                else
                  print("Missing")
                println(" %s".format(name))
              }
            }
          }
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
                          | Please help us fix this by opening a bug report at:
                          |
                          |  https://opensource.ncsa.illinois.edu/jira/browse/DFDL
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
