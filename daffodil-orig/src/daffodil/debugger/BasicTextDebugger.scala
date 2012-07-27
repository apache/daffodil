package daffodil.debugger

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

//import java.nio.charset.Charset
import java.nio.charset.Charset
import java.io.Serializable

import org.jdom.Document
import org.jdom.Element
import org.jdom.Parent

import daffodil.parser.RollbackStream
import daffodil.parser.VariableEncodingStreamReader
import daffodil.processors.VariableMap
import daffodil.processors.xpath.XPathUtil
import daffodil.processors.xpath.NodeResult
import daffodil.processors.xpath.StringResult
import daffodil.schema.annotation.Annotation
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil



/**
 * A debugger listener that provides an interactive, text-based debugging interface
 *
 * @see DebuggingListener
 *
 * @author Alejandro Rodriguez
 * @version 1 
 */
@SerialVersionUID(1)
class BasicTextDebugger extends DebuggingListener with Serializable{

  /** whether the debugger is stepping through and should interrupt execution */
  private var stopping = true

  private var inputBreakpoints:List[Int] = Nil
  private var schemaBreakpoints:List[Element] = Nil
  private var defaultTextEncoding = "UTF-8"

  override def step(schemaElement:Element,annotation:Annotation,documentElement:Parent,variables:VariableMap,
                    namespaces:Namespaces,input:RollbackStream) = {
    if (!stopping) {
      if (schemaBreakpoints.indexOf(schemaElement) != -1)
        stopping = true
      else if (inputBreakpoints.exists { _ <= input.getPosition })
        stopping = true
    }

    if (stopping){
      printSchemaContext(schemaElement)
      printInputPosition(input)
      while(!readCommand(schemaElement,annotation,documentElement,variables,namespaces,input)) {}
    }
  }

  /**
   * Interactively reads commands from from the command line
   */
  private def readCommand(schemaElement:Element,annotation:Annotation,document:Parent,
                          variables:VariableMap,namespaces:Namespaces,input:RollbackStream):Boolean = {
    println("Enter command ('h' for help):")
    try{
      Console.readChar match {
        case 'h' => printHelp
        case 'a' => printAnnotation(annotation)
        case 'n' => return true
        case 'r' => { stopping = false; return true }
        case 'b' => setSchemaBreakpoint(schemaElement,namespaces)
        case 'i' => setInputBreakpoint
        case 'd' => printDocumentContext(document)
        case 'e' => evalXPath(document,variables,namespaces)
        case 'p' => printDocument(document)
        case 's' => printSchemaContext(schemaElement)
        case 't' => printText(input)
        case 'v' => printVariables(variables)
        case 'm' => removeBreakpoints()
        case 'x' => printHex(input)
        case 'q' => System.exit(0)
        case _ =>
      }
    }catch{
      case _:StringIndexOutOfBoundsException =>
    }
    false
  }

  private def setSchemaBreakpoint(context:Element,namespaces:Namespaces) = {
    println("Enter xpath expression indicating an element in the schema (not the document):")
    println("i.e. \"//xsd:group[@name='myGroup']\"")
    val s = Console readLine

    try{
      XPathUtil.evalExpression(s,new VariableMap(),context,namespaces) match {
        case NodeResult(n) => if (n!=null){ println("Breakpoint set"); schemaBreakpoints = n :: schemaBreakpoints }
        else println("Location was not found");
        case StringResult(s) => println("Not an element:"+s)
      }

    }catch {
      case e:Exception => println(e)
    }
  }

  private def evalXPath(context:Parent,variables:VariableMap,namespaces:Namespaces) = {
    println("Enter an xpath expression to evaluate:")
    val s = Console readLine

    try{
      XPathUtil.evalExpression(s,variables,context,namespaces) match {
        case NodeResult(n) => println(n toString)
        case StringResult(s) => println(s)
      }
    }catch {
      case e:Exception => println(e)
    }
  }

  private def setInputBreakpoint = {
    println("Enter byte number to stop at:")
    val s = Console readLine;
    try{
      inputBreakpoints = s.toInt :: inputBreakpoints
      println("Breakpoint set")
    }catch {
      case _:NumberFormatException => "That's not a number"
    }
  }

  private def printDocument(document:Parent) = {
    println("DOCUMENT")
    println("--------")
    document match {
      case e:Element => println(XMLUtil serialize(XMLUtil getRoot(document)))
      case _:Document => println("Empty document")
    }
    println
  }

  private def printHelp = {
    println("Available commands:")
    println("  h   - help")
    println()
    println("  n   - next step")
    println("  r   - run")
    println()
    println("  b   - set breakpoint in schema")
    println("  i   - set breakpoint in input")
    println("  m   - remove a breakpoint")
    println()
    println("  a   - print annotation of current schema node")
    println("  d   - print context in parsed document")
    println("  e   - eval xpath  expression")
    println("  p   - print whole parsed document")
    println("  s   - print context in schema")
    println("  t   - print incoming input as text")
    println("  v   - print variables")
    println("  x   - print incoming input as hexadecimal")
    println()
    println("  q   - quit")
    println
  }

  private def printAnnotation(annotation:Annotation) = {
    println("DFDL ANNOTATION")
    println("---------------")
    println(annotation)
    println;
    true
  }

  private def printSchemaContext(schemaElement:Element)  = {
    println("SCHEMA CONTEXT")
    println("--------------")
    println(XMLUtil printContext(schemaElement,"^^^^^^^",160))
    println
  }

  private def printDocumentContext(documentElement:Parent) = {
    println("DOCUMENT CONTEXT")
    println("----------------")
    documentElement match {
      case e:Element => println(XMLUtil printContext(e,"^^^^^^^",160))
      case _:Document => println("Empty document")
    }
    println
  }

  private def printVariables(variables:VariableMap) = {
    println("VARIABLES")
    println("----------------")
    for( v <- variables.listVariables)
      println(v.name+"\t: "+v.variableType+"\t= "+v.value)
    println
  }

  private def printInputPosition(input:RollbackStream) = {
    if(input==null)
      println("No input in this context")
    else{
      println("INPUT POSITION")
      println("--------------")
      println(input getPosition)
    }
    println
  }

  /** Prints the input stream as text */
  private def printText(input:RollbackStream) = {
    if (input==null)
      println("No input in this context")
    else{
      println("Enter text encoding ["+defaultTextEncoding+"]:")
      var s = Console readLine;
      if (s=="") s = defaultTextEncoding;
      defaultTextEncoding = s
      input checkpoint;
      try{
        val charset = Charset.forName(s)
        val reader = new VariableEncodingStreamReader(input,charset)
        val buffer = new Array[Char](20)
        val read = reader read(buffer,0,20)
        println("INPUT ("+s+")")
        println("-----")
        for(i <- 0 until read)
          print(buffer(i))
        print("-----")
        println
      }catch{
        case e:Exception => println(e)
      }
      input rollback
    }
    println
  }

  /** Prints the input stream as hexadecimal values */
  private def printHex(input:RollbackStream) = {
    if (input==null)
      println("No input in this context")
    else{
      input checkpoint;
      try{
        val buffer = new Array[Byte](20)
        val read = input read(buffer,0,20)
        println("INPUT")
        println("-----")
        for(i <- 0 until read){
          print(Integer toHexString(buffer(i)))
          print(" ")
        }
        println
      }catch{
        case e:Exception => println(e)
      }
      input rollback
    }
    println
  }

  private def removeBreakpoints() = {
    var i = 1
    println("Choose a break point to remove:")
    println("in the schema")
    for(e <- schemaBreakpoints){
      println(i+":"+XMLUtil.printContext(e,"^^^",40))
      i += 1
    }
    println("------------")
    println("in the input")
    for(e <- inputBreakpoints){
      println(i+": byte "+e)
      i += 1
    }
    println("------------")
    println("?")
    val s = Console readLine;
    try{
      val n = s.toInt-1;
      if (n<schemaBreakpoints.size)
        schemaBreakpoints = schemaBreakpoints filterNot { _ == schemaBreakpoints(n) }
      else
        inputBreakpoints = inputBreakpoints filterNot { _ == inputBreakpoints(n-schemaBreakpoints.size) }
    }catch{
      case e:Exception => println(e)
    }
  }
}
