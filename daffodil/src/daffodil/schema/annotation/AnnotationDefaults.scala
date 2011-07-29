package daffodil.schema.annotation

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

import java.nio.charset.Charset

import daffodil.exceptions.DFDLSchemaDefinitionException
import daffodil.schema.annotation.enumerations._
import daffodil.xml.XMLUtil

/**
 * Holds the default values for format DFDL properties
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
object AnnotationDefaults {

  def defaultAlignment = 1

  def defaultAlignmentUnits = ByteAlignment

  def defaultBase = 10

  def defaultBinaryFloatRepresentation = IEEE

  def defaultBinaryNumberRepresentation = BinaryNumber

  def defaultByteOrder = BigEndian

  def defaultCalendarPatternKind = ImplicitCalendar

  def defaultEncoding = Charset forName ("UTF-8")

  def defaultEndOfParent = false

  def defaultEscapeKind = NoEscape

  def defaultFinalTerminatorCanBeMissing = false

  def defaultIgnoreCase = false

  def defaultInitiatedContent = false

  def defaultLeadingSkipBytes = 0

  def defaultLengthKind = Delimited

  def defaultOccursCountKind = Parsed

  def defaultSeparatorPolicy = Require

  def defaultSeparatorPosition = Infix

  def defaultSequenceKind = OrderedSequence

  def defaultTextNumberRep = StandardTextNumberRep

  def defaultTextNumberCheckPolicy = Strict

  def defaultTextStringJustification = NoneJustification

  def defaultTimingBefore = true

  def defaultTrailingSkipBytes = 0


  //TODO missing xsd:decimal
  def implicitLength(typeName:String) = 
    typeName match {
      case XMLUtil.XSD_BYTE| XMLUtil.XSD_UNSIGNED_BYTE => 1
      case XMLUtil.XSD_FLOAT => 4
      case XMLUtil.XSD_DOUBLE => 8
      case XMLUtil.XSD_LONG | XMLUtil.XSD_UNSIGNED_LONG => 8
      case XMLUtil.XSD_INT | XMLUtil.XSD_UNSIGNED_INT => 4
      case XMLUtil.XSD_SHORT | XMLUtil.XSD_UNSIGNED_SHORT => 2
      case XMLUtil.XSD_BOOLEAN => 4
      case s:String => throw new DFDLSchemaDefinitionException("Unknown implicit length for type "+s)
    }

}
