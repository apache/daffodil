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

import exceptions.DFDLSchemaDefinitionException
import java.net.URL
import java.util.Properties

import daffodil.arguments.ArgumentParser
import daffodil.arguments.ArgumentDescription
import daffodil.arguments.{OptionalMultiple,OptionalSingle}
import daffodil.arguments.SingleValue
import daffodil.debugger.DebugUtil
import daffodil.parser.SchemaParser
import xml.XMLUtil
import java.io._

/**
 * The command line interface to Daffodil
 *
 * @version 1
 * @author Alejandro Rodriguez
 * */
object Main {

  private val NAME = "Daffodil"
  private val MAJOR_VERSION = 0
  private val MINOR_VERSION = 1
  private val YEAR = 2010
  
  def main(arguments:Array[String]):Unit = {
    val argumentParser = new ArgumentParser

    argumentParser add (new ArgumentDescription("schema","schema","s",true,OptionalSingle))
    argumentParser add (new ArgumentDescription("input","input","i",true,OptionalSingle))
    argumentParser add (new ArgumentDescription("root","root","r",true,OptionalSingle))
    argumentParser add (new ArgumentDescription("parser-destination","parser-destination","D",true,OptionalSingle))
    argumentParser add (new ArgumentDescription("parser","parser","p",true,OptionalSingle))

    argumentParser add (new ArgumentDescription("output","output","o",true,OptionalSingle))
    argumentParser add (new ArgumentDescription("grddl","grddl","g",true,OptionalMultiple))
    argumentParser add (new ArgumentDescription("grddlOutput","grddloutput","G",true,OptionalSingle))

    argumentParser add (new ArgumentDescription("debug","debug","d",false,OptionalSingle))
    argumentParser add (new ArgumentDescription("version","version","v",false,OptionalSingle))
    argumentParser add (new ArgumentDescription("help","help","h",false,OptionalSingle))
    argumentParser add (new ArgumentDescription("verbose","verbose","V",false,OptionalSingle))
    
    argumentParser.add(new ArgumentDescription("unparse", "unparse", "u", true, OptionalSingle))

    try {
      argumentParser parse(arguments)
    }catch {
      case e:IllegalArgumentException => System.err.println(e.getMessage); usage
    }

    if (argumentParser isSet("help"))
      usage

    if (argumentParser isSet("version"))
      printVersion

    if (argumentParser isSet("verbose"))
      DebugUtil.verbose = true

    if (argumentParser.isSet("parser") && argumentParser.isSet("schema")){
      System.err.println("Both --schema and --parser option specified. Please use only one.")
      usage
    }

    if (argumentParser.isSet("input") && argumentParser.isSet("unparse")){
      System.err.println("Both --input and --unparse option specified. Please use only one.")
      usage
    }
    
    var schemaParser:SchemaParser = null

    try {

      if (argumentParser isSet "parser")
        schemaParser = DebugUtil.time("Loading parser",readParser(argumentParser getSingle ("parser")))
      else if (argumentParser isSet "schema"){
        val schema = argumentParser getSingle("schema")

        schemaParser = new SchemaParser
        if (argumentParser isSet("debug"))
          schemaParser setDebugging(true)

        DebugUtil.time("Parsing schema",schemaParser parse(schema))
      }else{
         System.err.println("Neither --schema nor --parser option specified. Nothing to do.")
        usage
      }

      if ( argumentParser.isSet("unparse")) {
        val infoset = argumentParser.getSingle("unparse")
        
        if (!argumentParser.isSet("root") && schemaParser.getTopElements.size != 1)
          if (schemaParser.getTopElements.size==0)
            throw new DFDLSchemaDefinitionException("Schema does not contain a top level element")
          else
            throw new DFDLSchemaDefinitionException("Schema contains more than one top level element "+
                    schemaParser.getTopElements+".\n Please specify which " +
                "one to use as root of the document with the option -r ")

        val data = 
          if ( argumentParser.isSet("root"))
            DebugUtil.time("Unparsing infoset",schemaParser.unparse(infoset, argumentParser.getSingle("root")))
          else
            DebugUtil.time("Unparsing infoset",schemaParser.unparse(infoset, schemaParser.getTopElements(0)))
          
        val output =
          if ( argumentParser.isSet("output"))
            new FileOutputStream(argumentParser.getSingle("output"));
          else
            System.out
          
        //TODO Once I decide unparse return type. Figure out how to write it out.
        DebugUtil.time("Saving file", output.write(data.array, 0, data.position))
      }

      if (argumentParser isSet "input"){

        val data = argumentParser getSingle("input")

        if (!argumentParser.isSet("root") && schemaParser.getTopElements.size != 1)
          if (schemaParser.getTopElements.size==0)
            throw new DFDLSchemaDefinitionException("Schema does not contain a top level element")
          else
            throw new DFDLSchemaDefinitionException("Schema contains more than one top level element "+
                    schemaParser.getTopElements+".\n Please specify which " +
                "one to use as root of the document with the option -r ")


        val root =
          if (argumentParser isSet("root"))
            DebugUtil.time("Parsing document",schemaParser eval(data,argumentParser getSingle ("root")))
          else
            DebugUtil.time("Parsing document",schemaParser eval(data,schemaParser.getTopElements(0)))


        DebugUtil log("Total nodes:"+XMLUtil.getTotalNodes)

        val output =
          if (argumentParser isSet("output"))
            new FileOutputStream(argumentParser getSingle("output"))
          else
            System.out


        // DBUtil printingPhase(true)
        DebugUtil.time("Printing",XMLUtil serialize(output,root))

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
      }
      
      if (argumentParser isSet "parser-destination"){
        DebugUtil time("Saving parser",writeParser(schemaParser,argumentParser getSingle ("parser-destination")))
      }


    }catch{
      case e:Exception => DebugUtil.printException(e)
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

    System exit(1)
  }
  
  private def printVersion = {
    print(NAME+" "+YEAR+"-"+MAJOR_VERSION+"."+MINOR_VERSION)
    println("   build "+BuildNumber.buildNumber)
    System exit(1)
  }

  /** Deserializaes a DFDL generated parser from a file */
  private def readParser(fileName:String):SchemaParser = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    is.readObject.asInstanceOf[SchemaParser]
  }

  /** Serializes the DFDL generated parser to a file */
  private def writeParser(schemaParser:SchemaParser,fileName:String) = {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os writeObject(schemaParser)
    os.close    
  }

}
