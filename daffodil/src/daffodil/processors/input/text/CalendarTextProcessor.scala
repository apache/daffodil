package daffodil.processors.input.text

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
import java.util.TimeZone

import org.jdom.Element
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

import daffodil.exceptions.ElementNotValidException
import daffodil.schema.annotation.enumerations.CalendarPatternKind
import daffodil.schema.annotation.enumerations.ExplicitCalendar
import daffodil.schema.annotation.enumerations.ImplicitCalendar
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil.XSD_DATE
import daffodil.xml.XMLUtil.XSD_DATE_TIME
import daffodil.xml.XMLUtil.XSD_TIME
import daffodil.processors.{Success, Last, ProcessorResult, VariableMap}
import daffodil.parser.RollbackStream
import daffodil.parser.regex.Regex

class CalendarTextProcessor(charset:Charset,
                            acceptEOF:Boolean) extends
        TextProcessor(charset,acceptEOF){

  private val DEFAULT_DATE_FORMAT = DateTimeFormat forPattern("yyyy-MM-dd")
  private val DEFAULT_DATE_TIME_FORMAT = DateTimeFormat forPattern("yyyy-MM-dd'T'HH:mm:ss")
  private val DEFAULT_TIME_FORMAT = DateTimeFormat forPattern("HH:mm:ssZ")

  private var patternKind:CalendarPatternKind = _
  private var pattern:String = _
  private var timeZone:String = _

  def setPatternKind(kind:CalendarPatternKind) = patternKind = kind
  def setPattern(pattern:String) =
    pattern match {
      case "I" | "IU" => this.pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZZ"
      case "T" | "TU" => this.pattern = "HH:mm:ss.SSSZZ"
      case _ => this.pattern = pattern replaceAll("v","ZZZ")
    }
  def setTimeZone(timeZone:String) = this.timeZone = typeName

  override def apply(input:RollbackStream,element:Element,
                     variables:VariableMap,
                     namespaces:Namespaces,terminators:List[Regex]):ProcessorResult = {
    if (timeZone != null)
      TimeZone setDefault(TimeZone getTimeZone(timeZone));

    val v = super.apply(input,element,variables,namespaces,terminators)
    v match {
      case Success | Last => makeIntoDate(element)
      case _ =>
    }
    v
  }

  override def canEqual(o:Any):Boolean = o.isInstanceOf[CalendarTextProcessor]

  override def equals(o:Any) =
    o match {
      case that:CalendarTextProcessor => that.canEqual(this) && this.patternKind == that.patternKind &&
              this.pattern == that.pattern && this.typeName == that.typeName && this.timeZone == that.timeZone
      super.equals(o)
      case _ => false
    }

  override def toString = "CalendarTextProcessor("+parametersToString+")"

  override def parametersToString = super.parametersToString+",patternKind:"+patternKind+",pattern:"+pattern

  private def makeIntoDate(element:Element) = {
    val text = element getText()
    try {
      val date = patternKind match {
        case ExplicitCalendar => DateTimeFormat.forPattern(pattern).parseDateTime(text)
        case ImplicitCalendar =>  getDefaultDate(text)
      }
      element setText(makeIntoString(date))
    }catch {
      case e:IllegalArgumentException => throw new ElementNotValidException("Cannot parse '"+text+"' into date",
              cause = e,documentContext = element)
    }
  }

  private def makeIntoString(date:DateTime):String =
    typeName match {
      case XSD_DATE => DEFAULT_DATE_FORMAT print(date)
      case XSD_DATE_TIME => DEFAULT_DATE_TIME_FORMAT print(date)
      case XSD_TIME => DEFAULT_TIME_FORMAT print(date)
    }

  private def getDefaultDate(text:String):DateTime =
    typeName match {
      case XSD_DATE => DEFAULT_DATE_FORMAT parseDateTime(text)
      case XSD_DATE_TIME => DEFAULT_DATE_TIME_FORMAT parseDateTime(text)
      case XSD_TIME => DEFAULT_TIME_FORMAT parseDateTime(text)
    }
}