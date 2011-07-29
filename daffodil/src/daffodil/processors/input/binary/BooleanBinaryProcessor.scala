package daffodil.processors.input.binary

import org.jdom.Element
import java.nio.ByteBuffer
import java.math.BigInteger
import daffodil.exceptions.{DFDLSchemaDefinitionException, ElementNotValidException, UnimplementedException}
import daffodil.schema.annotation.enumerations.{BinaryNumberRepresentation, BCDNumber, PackedNumber, BinaryNumber}

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

/**
 * Scans a binary boolean element
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
class BooleanBinaryProcessor(binaryNumberRep:BinaryNumberRepresentation) extends BinaryProcessor {

  private var trueRep:Long = -1
  private var falseRep:Long = _

  /**
   * Sets binaryBooleanTrueRep
   */
  def setBinaryTrueRep(value:Long) =
    this.trueRep = value

  /**
   * Sets binaryBooleanFalseRep
   */
  def setBinaryFalseRep(value:Long) =
    this.falseRep = value

  override protected def setValue(element:Element,buffer:ByteBuffer) = {

    binaryNumberRep match {
      case BinaryNumber => setBinary(element,buffer)
      case BCDNumber => setBCDNumber(element,buffer)
      case PackedNumber => throw new DFDLSchemaDefinitionException("PackedNumber not a valid boolean representation",
        documentContext = element)
    }
  }

  private def setBinary(element:Element,buffer:ByteBuffer) = {
    var value:Long = 0

    //TODO FIXME this assumes BigEndian order
    while(buffer.remaining>0)
      value = (value<<8) + unsign(buffer.get).toInt

    if (value==falseRep)
      element.setText("false")
    else if (trueRep == -1 || value==trueRep)
      element.setText("true")
    else
      throw new ElementNotValidException("Cannot parse into a boolean value "+value,
        documentContext = element)
  }

  private def setBCDNumber(element:Element,buffer:ByteBuffer) = {
    val value = BinaryUtil.bcdToNumber(buffer).toLong

    if (value==falseRep)
      element.setText("false")
    else if (trueRep == -1 || value==trueRep)
      element.setText("true")
    else
      throw new ElementNotValidException("Cannot parse into a boolean value "+value,
        documentContext = element)
  }
}