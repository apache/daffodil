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
import scala.xml.SAXParseException
import org.rogach.scallop
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.debugger.DebugUtil
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.compiler.Compiler

class CommandLineXMLLoaderErrorHandler() extends org.xml.sax.ErrorHandler {

  def warning(exception: SAXParseException) = {
    System.err.println("Warning loading schema: " + exception.getMessage)
  }

  def error(exception: SAXParseException) = {
    System.err.println("Error loading schema: " + exception.getMessage)
    System.exit(1)
  }

  def fatalError(exception: SAXParseException) = {
    System.err.println("Error loading schema: " + exception.getMessage)
    System.exit(1) 
  }
}

object Main {

  def createProcessorFromParser(parseFile: String, path: Option[String]) = {
    val compiler = Compiler()
    val processorFactory = DebugUtil.time("Reloading parser", compiler.reload(parseFile))
    val processor = processorFactory.onPath(path.getOrElse("/"))
    processor
  }

  def createProcessorFromSchemas(schemaFiles: List[String], root: Option[String], namespace: Option[String], path: Option[String], vars: Map[String, String]) = {
    val compiler = Compiler()

    val ns = namespace.getOrElse(null)

    vars foreach { case (key, value) => compiler.setExternalDFDLVariable(key, ns, value) }

    root match {
      case Some(r) => {
        compiler.setDistinguishedRootNode(r, ns)
      }
      case None => //ignore 
    }

    val processorFactory = DebugUtil.time("Compiling schema", compiler.compile(schemaFiles:_*))
    val processor = processorFactory.onPath(path.getOrElse("/"))
    processor
  }

  def run(arguments: Array[String]): Int = {
    object Conf extends scallop.ScallopConf(arguments) {

      // This essentially reimplements the listArgConverter in scallop to not
      // allow a list of options to follow the option. For example, the
      // following is illegal: --schema foo bar. Instead, it must be --schema
      // foo --schema bar. It does this by copying listArgConverter, but
      // setting the argType to SINGLE.
      def singleListArgConverter[A](conv: String => A)(implicit m: Manifest[List[A]]) = new scallop.ValueConverter[List[A]] {
        def parse(s:List[(String, List[String])]) = {
          try {
            val l = s.map(_._2).flatten.map(i => conv(i))
            if (l.isEmpty) Right(Some(Nil))
              else Right(Some(l))
          } catch {
              case _ => Left(Unit)
          }
        }
        val manifest = m
        val argType = scallop.ArgType.SINGLE
      }


      printedName = "daffodil"

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
      val debug   = opt[Boolean]("debug", descr="enable debugging.")
      val verbose = opt[Boolean]("verbose", descr="enable verbose output.")

      // Parse Subcommand Options
      val parse = new scallop.Subcommand("parse") {
        banner("""|Usage: daffodil parse (-s <schema> [-r <root> [-n <namespace>]] [-p <path>] | -P <parser>)
                  |                      [-D<variable>=<value>...] [-o <output>] [<infile>]
                  |
                  |Parse a file, using either a DFDL schema or a saved parser
                  |
                  |Parse Options:""".stripMargin)

        descr("parse data to a DFDL infoset")

        val schemas = opt[List[String]]("schema", argName="file", descr="the annotated DFDL schema to use to create the parser. May be supplied multiple times for multi-schema support.")(singleListArgConverter[String](a => a))
        val root    = opt[String]("root", argName="node", descr="the root element of the XML file to use. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema")
        val ns      = opt[String]("namespace", argName="ns", descr="the namespace of the root element. Requires --root.")
        val path    = opt[String]("path", argName="path", descr="path to the node to create parser.")
        val parser  = opt[String]("parser", 'P', argName="file", descr="use a previously saved parser.")
        val output  = opt[String]("output", argName="file", descr="write output to a given file. If not given or is -, output is written to stdout.")
        val vars    = props[String]('D', keyName="variable", valueName="value", descr="variables to be used when parsing.")
        val input   = trailArg[String]("infile", required=false, descr="input file to parse. If not specified, or a value of -, reads from stdin.")
      }

      // Unparse Subcommand Options
      val unparse = new scallop.Subcommand("unparse") {
        banner("""|Usage: daffodil unparse (-s <schema> [-r <root> [-n <namespace>]] [-p <path>] | -P <parser>)
                  |                        [-D<variable>=<value>...] [-o <output>] [<infile>]
                  |
                  |Unparse an infoset file, using either a DFDL schema or a saved paser
                  |
                  |Unparse Options:""".stripMargin)
        
        descr("unparse a DFDL infoset")

        val schemas = opt[List[String]]("schema", argName="file", descr="the annotated DFDL schema to use to create the parser. May be supplied multiple times for multi-schema support.")(singleListArgConverter[String](a => a))
        val root    = opt[String]("root", argName="node", descr="the root element of the XML file to use. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema")
        val ns      = opt[String]("namespace", argName="ns", descr="the namespace of the root element. Requires --root.")
        val path    = opt[String]("path", argName="path", descr="path to the node to create parser.")
        val parser  = opt[String]("parser", 'P', argName="file", descr="use a previously saved parser.")
        val output  = opt[String]("output", argName="file", descr="write output to file. If not given or is -, output is written to standard output.")
        val vars    = props[String]('D', keyName="variable", valueName="value", descr="variables to be used when unparsing.")
        val input   = trailArg[String]("infile", required=false, descr="input file to unparse. If not specified, or a value of -, reads from stdin.")
      }

      // Save Subcommand Options
      val save = new scallop.Subcommand("save-parser") {
        banner("""|Usage: daffodil save-parser -s <schema> [-r <root> [-n <namespace>]] [-p <path>] [<outfile>]
                  |
                  |Create and save a parser using a DFDL schema
                  |
                  |Save Parser Options:""".stripMargin)

        descr("save a daffodil parser for reuse")

        val schemas = opt[List[String]]("schema", argName="file", required=true, descr="the annotated DFDL schema to use to create the parser. May be supplied multiple times for multi-schema support.")(singleListArgConverter[String](a => a))
        val root    = opt[String]("root", argName="node", descr="the root element of the XML file to use. This needs to be one of the top-level elements of the DFDL schema defined with --schema. Requires --schema. If not supplied uses the first element of the first schema.")
        val ns      = opt[String]("namespace", argName="ns", descr="the namespace of the root element. Requires --root.")
        val path    = opt[String]("path", argName="path", descr="path to the node to create parser.")
        val output  = trailArg[String]("outfile", required=false, descr="output file to save parser. If not specified, or a value of -, writes to stdout.")
        val vars    = props[String]('D', keyName="variable", valueName="value", descr="variables to be used.")
      }

      // Test Subcommand Options
      val test = new scallop.Subcommand("test") {
        banner("""|Usage: daffodil test <tdmlfile> [<testname>...]
                  |
                  |List or execute tests in a TDML file
                  |
                  |Test Options:""".stripMargin)

        descr("list or execute TDML tests")

        val list  = opt[Boolean]("list", descr="show names and descriptions instead of running test cases.")
        val regex = opt[Boolean]("regex", descr="treat <names> as regular expressions.")
        val file  = trailArg[String]("tdmlfile", required=true, descr="test data markup language (TDML) file.")
        val names = trailArg[List[String]]("names", required=false, descr="name of test case(s) in tdml file. If not given, all tests in tdmlfile are run.")
      }

      verify

      // Custom verification, scallop isn't quite rich enough
      subcommand match {
        case Some(this.parse) => {
          if (this.parse.parser.isDefined) {
            if (this.parse.schemas().length > 0) { onError(scallop.exceptions.IllegalOptionParameters("only one of --parser and --schema may be defined")) }
            if (this.parse.root.isDefined) { onError(scallop.exceptions.IllegalOptionParameters("--root cannot be defined with --parser")) }
            if (this.parse.ns.isDefined) { onError(scallop.exceptions.IllegalOptionParameters("--namespace cannot be defined with --parser")) }
          } else if (this.parse.schemas().length > 0) {
            if (this.parse.ns.isDefined && !this.parse.root.isDefined) { onError(scallop.exceptions.IllegalOptionParameters("--root must be defined if --namespace is defined")) }
          } else {
            onError(scallop.exceptions.IllegalOptionParameters("one of --schema or --parser must be defined"))
          }
        }

        case Some(this.unparse) => {
          if (this.unparse.parser.isDefined) {
            if (this.unparse.schemas().length > 0) { onError(scallop.exceptions.IllegalOptionParameters("only one of --parser and --schema may be defined")) }
            if (this.unparse.root.isDefined) { onError(scallop.exceptions.IllegalOptionParameters("--root cannot be defined with --parser")) }
            if (this.unparse.ns.isDefined) { onError(scallop.exceptions.IllegalOptionParameters("--namespace cannot be defined with --parser")) }
          } else if (this.unparse.schemas().length > 0) {
            if (this.unparse.ns.isDefined && !this.unparse.root.isDefined) { onError(scallop.exceptions.IllegalOptionParameters("--root must be defined if --namespace is defined")) }
          } else {
            onError(scallop.exceptions.IllegalOptionParameters("one of --schema or --parser must be defined"))
          }
        }

        case Some(this.save) => {
          if (this.save.ns.isDefined && !this.save.root.isDefined) { onError(scallop.exceptions.IllegalOptionParameters("--root must be defined if --namespace is defined")) }
        }

        case Some(this.test) => {
          // no additional validation needed
        }

        case _ => onError(scallop.exceptions.IllegalOptionParameters("missing subcommand"))
      }
    }
      
    if (Conf.verbose())
      DebugUtil.verbose = true
 
    if (Conf.debug())
      Debugger.setDebugging(true)


    val ret = Conf.subcommand match {
      
      case Some(Conf.parse) => {
        val parseOpts = Conf.parse

        val processor = {
          if (parseOpts.parser.isDefined) {
            createProcessorFromParser(parseOpts.parser(), parseOpts.path.get)
          } else {
            createProcessorFromSchemas(parseOpts.schemas(), parseOpts.root.get, parseOpts.ns.get, parseOpts.path.get, parseOpts.vars)
          }
        }

        val input = parseOpts.input.get match {
          case Some("-") | None => System.in
          case Some(file) => new FileInputStream(parseOpts.input())
        }
        val inChannel = java.nio.channels.Channels.newChannel(input);

        val parseResult = DebugUtil.time("Parsing", processor.parse(inChannel))
        
        val output = parseOpts.output.get match {
          case Some("-") | None => System.out
          case Some(file) => new FileOutputStream(file)
        }
        val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(output));

        val pp = new scala.xml.PrettyPrinter(80,2)
        DebugUtil.time("Writing", writer.write(pp.format(parseResult.result) + "\n"))
        writer.flush()

        if (parseResult.isError) 1 else 0
      }


      case Some(Conf.unparse) => {
        val unparseOpts = Conf.unparse

        val processor = {
          if (unparseOpts.parser.isDefined) {
            createProcessorFromParser(unparseOpts.parser(), unparseOpts.path.get)
          } else {
            createProcessorFromSchemas(unparseOpts.schemas(), unparseOpts.root.get, unparseOpts.ns.get, unparseOpts.path.get, unparseOpts.vars)
          }
        }

        val output = unparseOpts.output.get match {
          case Some("-") | None => System.out
          case Some(file) => new FileOutputStream(file)
        }

        val outChannel = java.nio.channels.Channels.newChannel(output)
        val loader = new DaffodilXMLLoader(new CommandLineXMLLoaderErrorHandler)
        loader.setValidation(true)
        val document = unparseOpts.input.get match {
          case Some("-") | None => loader.load(System.in)
          case Some(file) => loader.loadFile(file)
        }
        val unparseResult =  DebugUtil.time("Unparsing infoset", processor.unparse(outChannel, document))
        output.close()

        if (unparseResult.isError) 1 else 0
      }

      case Some(Conf.save) => {
        val saveOpts = Conf.save

        val processor = createProcessorFromSchemas(saveOpts.schemas(), saveOpts.root.get, saveOpts.ns.get, saveOpts.path.get, saveOpts.vars)

        val output = saveOpts.output.get match {
          case Some("-") | None => System.out
          case Some(file) => new FileOutputStream(file)
        }

        val outChannel = java.nio.channels.Channels.newChannel(output)
        val saveResult = DebugUtil.time("Saving parser", processor.save(outChannel))

        0
      }

      case Some(Conf.test) => {
        val testOpts = Conf.test

        val tdmlFile = testOpts.file()
        val tdmlRunner = new DFDLTestSuite(new java.io.File(tdmlFile))
       
        val tests = {
          if (testOpts.names().length > 0) {
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
          if (Conf.verbose()) {
            // determine the max lengths of the various pieces of atest
            val headers = List("Name", "Model", "Root", "Description")
            val maxCols = tests.foldLeft(headers.map(_.length)) {
              (maxVals, testPair) => {
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
            println(formatStr.format(headers:_*))
            tests.foreach{ testPair =>
              testPair match {
                case (name, Some(test)) => println(formatStr.format(name, test.model, test.root, test.description))
                case (name, None) => println("%s [Missing]".format(name))
              }
            }
          } else {
            tests.foreach{ testPair =>
              testPair match {
                case (name, Some(test)) => println(name)
                case (name, None) => println("%s [Missing]".format(name))
              }
            }
          }
        } else {
          tests.foreach{ testPair =>
            testPair match {
              case (name, Some(test)) => {
                var success = true
                try {
                  test.run()
                } catch {
                  case _ =>
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

  def main(arguments: Array[String]): Unit = {
    val ret = try {
      run(arguments)
    } catch {
      case e: Exception => bugFound(e)
    }

    System.exit(ret)
  }
}
