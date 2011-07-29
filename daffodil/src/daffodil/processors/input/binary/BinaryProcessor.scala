package daffodil.processors.input.binary

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

import java.math.BigInteger
import java.nio.ByteBuffer

import org.jdom.Parent
import org.jdom.Element

import daffodil.exceptions.DFDLSchemaDefinitionException
import daffodil.exceptions.UnimplementedException
import daffodil.processors.xpath.StringResult
import daffodil.processors.xpath.NodeResult
import daffodil.processors.xpath.XPathUtil
import daffodil.schema.annotation.AnnotationDefaults
import daffodil.xml.XMLUtil
import daffodil.xml.Namespaces
import daffodil.schema.annotation.enumerations._
import daffodil.parser.RollbackStream
import daffodil.processors.{ScanResult, NothingFound}
import daffodil.processors.input.BasicProcessor
import daffodil.parser.regex.Regex
import daffodil.processors.{LastEmpty, Success, ProcessorResult, VariableMap}

/**
 * A BasicProcessor that reads data with dfdl:representation=binary
 */
abstract class BinaryProcessor extends BasicProcessor{

  private var alignment:Int = _
  
  private var leadingSkipBytes:Int = _
  
  private var trailingSkipBytes:Int = _
  
  protected var binaryNumberRep:BinaryNumberRepresentation = _
   
  protected var typeName:String = _
  
  private var length:String = _
  
  private var lengthKind:LengthKind = _
  
  private var byteOrder:ByteOrder = _
  
  private val MAX_UNSIGNED_BYTE:Int = 256 
  
  private val MAX_UNSIGNED_SHORT:Int = 65536
  
  private val MAX_UNSIGNED_INT:Long = 4294967296L 
  
  private val MAX_UNSIGNED_LONG = new BigInteger("18446744073709551616") 
  
  def setAlignment(alignment:Int,units:AlignmentUnit) = {
    units match {
      case BitAlignment =>
        if (alignment%8 != 0)
          throw new UnimplementedException("no support for sub-byte alignment") // TODO implement?
        else
          this.alignment = alignment/8
      case ByteAlignment => this.alignment = alignment 
    }
  }
  
  def setLeadingSkipBytes(leadingSkipBytes:Int) = this.leadingSkipBytes = leadingSkipBytes
  
  def setTrailingSkipBytes(trailingSkipBytes:Int) = this.trailingSkipBytes = trailingSkipBytes  

  def setBinaryNumberRepresentation(binaryRepresentation:BinaryNumberRepresentation) =
    this.binaryNumberRep = binaryNumberRep
  
  def setLengthKind(lengthKind:LengthKind) = this.lengthKind = lengthKind
  
  def setLength(length:String) = this.length = length
  
  def setByteOrder(byteOrder:ByteOrder) = this.byteOrder = byteOrder
  
  def setTypeName(typeName:String) = this.typeName = typeName
  
  override def findPrefixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                    	namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult = NothingFound

    
  override def findPostfixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                    	namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult = NothingFound
  
  //TODO missing length of parent for the end-of-parent case
  override def apply(input:RollbackStream,element:Element,
                     variables:VariableMap,namespaces:Namespaces,
  					terminators:List[Regex]):ProcessorResult =
    lengthKind match {
      case Prefixed => getPrefixed(input,element,variables)
      case EndOfParent => getVariableLength(input,element,variables,terminators)
      case _ => getFixedLength(input,element,variables,namespaces)
    }
    
  //TODO implement
  private def getPrefixed(input:RollbackStream,element:Element,
                     variables:VariableMap):ProcessorResult =
    throw new UnimplementedException("prefixed length",documentContext = element,position = Some(input getPosition))
  
  //TODO implement
  private def getVariableLength(input:RollbackStream,element:Element,
                     variables:VariableMap,terminators:List[Regex]):ProcessorResult =
    throw new UnimplementedException("end-of-parent length",documentContext = element,position = Some(input getPosition))
  
  private def getFixedLength(input:RollbackStream,element:Element,
                     variables:VariableMap,namespaces:Namespaces):ProcessorResult = {
    
    val actualLength = lengthKind match {      
      case Implicit => if (typeName==XMLUtil.XSD_HEX_BINARY) -1 else AnnotationDefaults implicitLength(typeName)
      case Explicit => getLength(element,variables,namespaces)
      case a:LengthKind => throw new DFDLSchemaDefinitionException("length type not allowed for binary "+a,
        documentContext = element,position = Some(input getPosition))
    }
    
    if (typeName==XMLUtil.XSD_HEX_BINARY){
      val string = new StringBuilder
      //input checkpoint
        
      var read = 0      
      while(read!=actualLength){
        val char = input.read
        if (char != -1){
        	string append(Integer.toString(char,16))
        }else{
          if (string.length > 0){
            element setText(string toString)
            setType(typeName.substring(XMLUtil.XSD_NAMESPACE.length),element,namespaces)
            return Success
          }else{
        	return LastEmpty
          }            
        }
        read += 1
      }
      element setText(string toString)
      setType(typeName.substring(XMLUtil.XSD_NAMESPACE.length),element,namespaces)
      Success
    }else{
    	val buffer = getBuffer(actualLength)
    	byteOrder match {
    	  case BigEndian => buffer order(java.nio.ByteOrder.BIG_ENDIAN)
    	  case LittleEndian => buffer order(java.nio.ByteOrder.LITTLE_ENDIAN)
       }
    	//input checkpoint
    	for(i <- 0 until actualLength){
    	  val char = input.read
    	  if (char != -1)
    		  buffer.put(char.toByte)
    	  else{
    		  //input rollback
    		  return LastEmpty
    	  }
    	}
    	buffer flip;

      setValue(element,buffer)
      
      setType(typeName.substring(XMLUtil.XSD_NAMESPACE.length),element,namespaces)
     
    	//input uncheck;
    	Success     
    }
  }
  
  override def init(input:RollbackStream,element:Element,variables:VariableMap,
                    namespaces:Namespaces) = {
    for(i <- 0 until leadingSkipBytes)
      input read;
    //TODO FIXME check for end of file
    while(input.getPosition%alignment != 0)
      input read;
  }
  
  override def terminate(input:RollbackStream,element:Element,variables:VariableMap,
                         namespaces:Namespaces,terminators:List[Regex]) = {
    for(i <- 0 until trailingSkipBytes)
      input read;
  }
  
  private def getLength(element:Element,variables:VariableMap,namespaces:Namespaces):Int = {
    if (XPathUtil isExpression(length)){
      val expression = XPathUtil getExpression(length)
      
      XPathUtil evalExpression(expression,variables,element,namespaces) match {
        case StringResult(s) => s toInt
        case NodeResult(n) => n.getText.toInt
      }      
    }else
      length toInt
  }
  
  private def getBuffer(actualLength:Int):ByteBuffer = 
	  ByteBuffer allocate(actualLength)

  /**
   * Sets the textValue of the parsed XML element based on the bytes read from the input
   *
   * @param element the element which value should be set
   * @param buffer the byteBuffer read from the input corresponding to the element
   */
  protected def setValue(element:Element,buffer:ByteBuffer)
  
  protected def unsign(byte:Byte):String =
    if (byte >= 0) 
      byte toString
    else{
      var i:Int = MAX_UNSIGNED_BYTE
      i += byte
      i toString
    }
  
  protected def unsign(short:Short):String =
    if (short >= 0)
      short toString
    else {
      var s:Int = MAX_UNSIGNED_SHORT
      s += short
      s toString
    }
  
  protected def unsign(int:Int):String =
    if (int >= 0)
      int toString
    else{
      var l:Long = MAX_UNSIGNED_INT
      l += int
      l toString
    } 
  
  protected def unsign(long:Long):String = {
    if (long >= 0)
      long toString
    else {
      val b = MAX_UNSIGNED_LONG
      b.add(new BigInteger(long toString)).toString
    }
  }

}