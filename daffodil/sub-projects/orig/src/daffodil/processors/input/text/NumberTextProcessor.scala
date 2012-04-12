package daffodil.processors.input.text

import java.nio.charset.Charset
import java.text.ParseException

import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import org.jdom.Element

import daffodil.exceptions.{ElementNotValidException, UnimplementedException}
import daffodil.parser.regex.{ Regex, RegexCompiled }
import daffodil.parser.RollbackStream
import daffodil.processors.{Success, Last, VariableMap, ProcessorResult}
import daffodil.schema.annotation.enumerations._
import daffodil.schema.annotation.{ExpressionValue, ListLiteralValue, EmptyValue, AttributeValue}
import daffodil.xml.{XMLUtil, Namespaces}
import java.math.BigInteger


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

class NumberTextProcessor(charset:Charset,acceptEOF:Boolean) extends
                           TextProcessor(charset,acceptEOF){
  private var base:Int = _

  private var textStandardInfinityRep:String = _

  private var textStandardNanRep:String = _

  private var textNumberPattern:String = _

  private var textStandardDecimalSeparator:String = _

  private var textStandardGroupingSeparator:String = _

  private var textStandardExponentCharacter:String = _

  private var textStandardZeroRep:AttributeValue = EmptyValue

  def setBase(b:Int) =
    base = b

  def setTextStandardInfinityRep(s:String) =
    textStandardInfinityRep = s

  def setTextStandardNanRep(s:String) =
    textStandardNanRep = s

  def setTextStandardDecimalSeparator(s:String) =
    textStandardDecimalSeparator = s

  def setTextStandardGroupingSeparator(s:String) =
    textStandardGroupingSeparator = s

  def setTextStandardExponentCharacter(s:String) =
    textStandardExponentCharacter = s

  def setTextNumberPattern(s:String) =
    textNumberPattern = s

    // TODO implement lax policy
  def setTextNumberCheckPolicy(s:CheckPolicy) = s match{
    case Strict =>
    case Lax => throw new UnimplementedException("textNumberCheckPolicy=lax")
  }

  // TODO implement zoned text number representation
  def setTextNumberRep(representation:TextNumberRep) =
    representation match {
      case ZonedTextNumberRep => throw new UnimplementedException("zoned textNumberRep")
      case StandardTextNumberRep =>
    }

  def setTextStandardZeroRep(v:AttributeValue) =
    textStandardZeroRep = v

  override def apply(input:RollbackStream,element:Element,
                     variables:VariableMap,namespaces:Namespaces,
                     terminators:List[Regex]):ProcessorResult = {
    val v = super.apply(input,element,variables,namespaces,terminators)
    v match {
      case Success | Last => makeIntoNumber(element,variables,namespaces)
      case _ =>
    }
    v
  }

  private def makeIntoNumber(element:Element,variables:VariableMap,namespaces:Namespaces) = {
    val text = element.getText().trim
    try {
     
      val zeros:List[RegexCompiled] = textStandardZeroRep match {
        case ListLiteralValue(l) => l map { _ compile(ignoreCase)}
        case e:ExpressionValue => XMLUtil getListFromExpression(e,variables,element,namespaces) map { _ compile(ignoreCase)}
        case EmptyValue => List()
      }

      if (zeros exists { _ matches (text.trim) } )
        element setText ("0")
      else{

        if (base==10){
          val symbols = new DecimalFormatSymbols()

          if (textStandardInfinityRep != null) //can be expression
            symbols setInfinity textStandardInfinityRep

          if (textStandardNanRep != null) //can be expression
            symbols setNaN textStandardNanRep

          //TODO
          if (textStandardDecimalSeparator != null) //can be expression
            symbols setDecimalSeparator textStandardDecimalSeparator(0)

          if (textStandardGroupingSeparator != null) //can be expression
            symbols setGroupingSeparator textStandardGroupingSeparator(0)

          if (textStandardExponentCharacter != null) //can be expression
            symbols setExponentSeparator textStandardExponentCharacter


          val pattern =
            if (textNumberPattern != null)
              new DecimalFormat(textNumberPattern)
            else
              new DecimalFormat()


          pattern setDecimalFormatSymbols symbols

          val value = pattern parse(text)

          element setText (value toString)

        } else{
          val value = new BigInteger(text,base)
          element setText (value toString)
        }
      }

      //TODO check range
    }catch {
      case e:ParseException => throw new ElementNotValidException("Cannot parse into number '"+text+"'",
        cause = e,documentContext = element)
      case e:NumberFormatException => throw new ElementNotValidException("Cannot parse into number '"+text+"'",
        cause = e,documentContext = element)
    }
  }

}