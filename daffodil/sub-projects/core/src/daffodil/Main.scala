package daffodil

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

import daffodil.api.DFDL
import daffodil.arguments.ArgumentDescription
import daffodil.arguments.ArgumentParser
import daffodil.arguments.OptionalSingle
import daffodil.debugger.DebugUtil
import xml.XMLUtil

/**
 * The command line interface to Daffodil
 * @author Alejandro Rodriguez
 */
object Main {

  private val NAME = "Daffodil"
  private val MAJOR_VERSION = 0
  private val MINOR_VERSION = 2
  private val YEAR = 2012

  def main(arguments: Array[String]): Unit = {
    val argumentParser = new ArgumentParser

    argumentParser add (new ArgumentDescription("schema", "schema", "s", true, OptionalSingle))
    argumentParser add (new ArgumentDescription("input", "input", "i", true, OptionalSingle))
    argumentParser add (new ArgumentDescription("root", "root", "r", true, OptionalSingle))
    argumentParser add (new ArgumentDescription("parser-destination", "parser-destination", "D", true, OptionalSingle))
    argumentParser add (new ArgumentDescription("parser", "parser", "p", true, OptionalSingle))
    argumentParser add (new ArgumentDescription("output", "output", "o", true, OptionalSingle))

    //    argumentParser add (new ArgumentDescription("grddl", "grddl", "g", true, OptionalMultiple))
    //    argumentParser add (new ArgumentDescription("grddlOutput", "grddloutput", "G", true, OptionalSingle))

    argumentParser add (new ArgumentDescription("debug", "debug", "d", false, OptionalSingle))
    argumentParser add (new ArgumentDescription("version", "version", "v", false, OptionalSingle))
    argumentParser add (new ArgumentDescription("help", "help", "h", false, OptionalSingle))
    argumentParser add (new ArgumentDescription("verbose", "verbose", "V", false, OptionalSingle))

    argumentParser.add(new ArgumentDescription("unparse", "unparse", "u", true, OptionalSingle))

    try {
      argumentParser parse (arguments)
    } catch {
      case e: IllegalArgumentException => System.err.println(e.getMessage); usage
    }

    if (argumentParser isSet ("help"))
      usage

    if (argumentParser isSet ("version"))
      printVersion

    if (argumentParser isSet ("verbose"))
      DebugUtil.verbose = true

    if (argumentParser.isSet("parser") && argumentParser.isSet("schema")) {
      System.err.println("Both --schema and --parser option specified. Please use only one.")
      usage
    }

    if (argumentParser.isSet("input") && argumentParser.isSet("unparse")) {
      System.err.println("Both --input and --unparse option specified. Please use only one.")
      usage
    }

    val compiler = daffodil.dsom.Compiler()
    var processorFactory: DFDL.ProcessorFactory = null
    var processor: DFDL.DataProcessor = null

    try {

      if (argumentParser isSet "parser") {
        processorFactory = DebugUtil.time("Loading parser", compiler.reload(argumentParser getSingle ("parser")))
      } else if (argumentParser isSet "schema") {
        val schema = argumentParser getSingle ("schema")
        processorFactory = DebugUtil.time("Compiling schema", compiler.compile(schema))
      } else {
        System.err.println("Neither --schema nor --parser option specified. Nothing to do.")
        usage
      }
      processor = processorFactory.onPath("/")
      if (argumentParser isSet ("debug"))
        compiler.setDebugging(true)
      if (argumentParser isSet ("root"))
        compiler.setDistinguishedRootNode(argumentParser getSingle ("root"), "") //TODO: namespace

      if (argumentParser.isSet("unparse")) try {
        val infoset = argumentParser.getSingle("unparse")
        val outputStream =
          if (argumentParser.isSet("output"))
            new FileOutputStream(argumentParser.getSingle("output"));
          else
            System.out
        val out = java.nio.channels.Channels.newChannel(outputStream)

        try {
          val document = scala.xml.XML.loadFile(infoset)
          DebugUtil.time("Unparsing infoset", processor.unparse(out, document))
        } finally {
          out.close()
        }
      } finally {
        // nothing. Just keep going.
      }

      if (argumentParser isSet "input") {
        // not called parse because of single-letter command line options 
        // naming conflicts. I.e., -p is the 'parser' aka processor.
        // -i and -u control parsing versus unparsing.        
        val data = argumentParser getSingle ("input")
        val bytes = data.getBytes()
        val inputStream = new ByteArrayInputStream(bytes);
        val rbc = java.nio.channels.Channels.newChannel(inputStream);
        val result = DebugUtil.time("Parsing document", processor.parse(rbc))

        DebugUtil log ("Total nodes:" + XMLUtil.getTotalNodes)

        val output =
          if (argumentParser isSet ("output"))
            new FileOutputStream(argumentParser getSingle ("output"))
          else
            System.out
        try {
          // DBUtil printingPhase(true)
          DebugUtil.time("Printing", output.write(result.toString.getBytes()))
        } finally {
          output.close()
        }
      }
      //        if (argumentParser isSet("grddl"))
      //          if (argumentParser isSet("output")){
      //            val trans = argumentParser getMultiple("grddl")
      //            val xmlOutput = new File(argumentParser getSingle("output"))
      //            val rdfOutput = argumentParser get("grddlOutput") match {
      //              case SingleValue(o) => o
      //              case _ => xmlOutput.getAbsolutePath + ".rdf"
      //            }
      //            DebugUtil.time("Injecting GRDDL headers",GRDDLUtil inject(root,trans))
      //            DebugUtil.time("Performing GRDDL transformations",
      //              GRDDLUtil glean(new URL(new URL("file:///"),xmlOutput getAbsolutePath),
      //                      trans,new FileOutputStream(rdfOutput)))
      //          }else{
      //            System.err.println("You need to specify an output file in order to use GRDDL")
      //
      //          }

      if (argumentParser isSet "parser-destination") {
        DebugUtil time ("Saving parser", processor.save(argumentParser getSingle ("parser-destination")))
      }

    } catch {
      case e: Exception => DebugUtil.printException(e)
    }
  }

  private def usage = {
    println("""
      Daffodil

    USAGE

      scala -cp $CLASSPATH daffodil.Main [OPTIONS]
    
    EXAMPLES

      scala -cp $CLASSPATH daffodil.Main -s <schema> -i <input>


      scala -cp $CLASSPATH daffodil.Main -s <schema> -r <root> -i <input> \
                                       -o <output> -g <grddlTransformation>

      scala -cp $CLASSPATH daffodil.Main -s <schema> -D <parser>

      scala -cp $CLASSPATH daffodil.Main -p <parser> -i <input> 

      scala -cp $CLASSPATH daffodil.Main --version


    OPTIONS

      -d, --debug            start the parser in debugger mode. This mode helps
                             debug the schema by printing step by step
                             information of the parsing.

      -D,
       --parser-destination <file>
                             saves the produced parser to the specified file.

      -g, --grddl <grddl>    adds a grddl transformation (can be specified
                             multiple times). -o needs to be specified.

      -G,
       -grddlOutput <grddl>  indicates the output for the grddl
                             transformation. If not specified, <ouput>.rdf will
                             be used (as specified by -o).

      -h, --help             prints this help message.

      -i, --input  <input>   the input data file to translate.
            
      -u, --unparse <infoset>
                             the DFDL infoset file (XML) to unparse.

      -o, --output <output>  saves XML output to the specified file. If not
                             specified, ouput is printed to standard output

      -p, --parser <file>    reads a previously saved parser. Use this option
                             instead of -s if you saved the parser.

      -r, --root <root>      the root element of the XML file to produce. This
                             needs to be one of the top-level elements of the
                             DFDL schema.

      -s, --schema <schema>  the annotated DFDL schema to use to produce the
                             parser.

      -v, --version          prints version information and exits.

      -V, --verbose          prints additional information while processing.
      """)

    System exit (1)
  }

  private def printVersion = {
    print(NAME + " " + YEAR + "-" + MAJOR_VERSION + "." + MINOR_VERSION)
    println("   build " + BuildNumber.buildNumber)
    System exit (1)
  }

}
