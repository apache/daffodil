package daffodil.processors.input.binary

import org.jdom.Element
import daffodil.xml.XMLUtil
import java.nio.ByteBuffer
import daffodil.exceptions.ElementNotValidException
import daffodil.schema.annotation.enumerations.{BinaryNumberRepresentation, PackedNumber, BCDNumber, BinaryNumber}

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

class DecimalBinaryProcessor(binaryNumberRep:BinaryNumberRepresentation) extends BinaryProcessor {

  override protected def setValue(element:Element,buffer:ByteBuffer) = {
    binaryNumberRep match {
      case BinaryNumber => setBinary(element,buffer)
      case PackedNumber => setPackedNumber(element,buffer)
      case BCDNumber => setBCDNumber(element,buffer)
    }
  }

  private def setBinary(element:Element,buffer:ByteBuffer) = {
    typeName match {
      case XMLUtil.XSD_BYTE => element setText(buffer.get.toString)
      case XMLUtil.XSD_UNSIGNED_BYTE => element setText(unsign(buffer.get))
      case XMLUtil.XSD_SHORT => element setText(buffer.getShort.toString)
      case XMLUtil.XSD_UNSIGNED_SHORT => element setText(unsign(buffer.getShort))
      case XMLUtil.XSD_INT => element setText(buffer.getInt.toString)
      case XMLUtil.XSD_UNSIGNED_INT => element setText(unsign(buffer.getInt))
      case XMLUtil.XSD_LONG => element setText(buffer.getLong.toString)
      case XMLUtil.XSD_UNSIGNED_LONG => element setText(unsign(buffer.getLong))
    }
  }

  private def setPackedNumber(element:Element,buffer:ByteBuffer) = {
    val sb = new StringBuilder
    var result:String = null

    while(buffer.remaining>0){
      val byte = buffer.get
      val lowestPart = byte & 0xF0
      val highestPart = byte & 0x0F

      lowestPart match {
        case 0x00 => sb.append("0")
        case 0x10 => sb.append("1")
        case 0x20 => sb.append("2")
        case 0x30 => sb.append("3")
        case 0x40 => sb.append("4")
        case 0x50 => sb.append("5")
        case 0x60 => sb.append("6")
        case 0x70 => sb.append("7")
        case 0x80 => sb.append("8")
        case 0x90 => sb.append("9")
        case _ =>   throw new ElementNotValidException("Cannot parse into a packed number byte "+byte,
          documentContext = element)
      }

      if (buffer.remaining != 0)
       highestPart match {
        case 0x00 => sb.append("0")
        case 0x01 => sb.append("1")
        case 0x02 => sb.append("2")
        case 0x03 => sb.append("3")
        case 0x04 => sb.append("4")
        case 0x05 => sb.append("5")
        case 0x06 => sb.append("6")
        case 0x07 => sb.append("7")
        case 0x08 => sb.append("8")
        case 0x09 => sb.append("9")
        case _ =>   throw new ElementNotValidException("Cannot parse into a packed number byte "+byte,
          documentContext = element)
       }
      else
        highestPart match {
          case 0x0c | 0x0a | 0x0e | 0x0f => result = sb toString
          case 0x0b | 0x0d => result = "-" + sb.toString
          case _ =>   throw new ElementNotValidException("Cannot parse into a packed number byte "+byte,
          documentContext = element)
        }
    }

    //TODO check result is in valid range    
    element setText(result)
  }

  private def setBCDNumber(element:Element,buffer:ByteBuffer) = {
    try {
      val result = BinaryUtil bcdToNumber(buffer)
      //TODO check result is in valid range
      element setText(result toString)
    }catch{
      case e:IllegalArgumentException => throw new ElementNotValidException("Cannot parse into a bcd number",
          cause = e,documentContext = element)
    }
  }

}