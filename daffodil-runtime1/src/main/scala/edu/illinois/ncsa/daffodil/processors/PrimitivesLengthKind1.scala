/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import scala.Array.canBuildFrom
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.toMaybe
import edu.illinois.ncsa.daffodil.processors.unparsers.UState

/**
 * What's going on here is that we have three factory classes:
 *
 * 1. EscapeScheme
 * 2. Delimiters
 * 3. Field
 *
 * Field depends on Delimiters and EscapeScheme. As such it needs to handle
 * dynamic/static cases. Delimiters can be static or dynamic and so need to
 * be generated prior to Field. EscapeScheme can also be static or
 * dynamic and so needs to also be generated prior to Field.
 *
 * We use information regarding whether or not we have Static vs. Dynamic
 * Delimiters/EscapeScheme to determine if we can statically generate the
 * Field. So there is some cascading going on here as a result of these
 * dependencies.
 */
sealed abstract class FieldFactoryBase(ef: Option[EscapeSchemeFactoryBase],
  context: ThrowsSDE) extends Serializable {

  def getFieldDFA(state: PState)
}

case class FieldFactoryStatic(ef: Option[EscapeSchemeFactoryBase], context: ThrowsSDE)
  extends FieldFactoryBase(ef, context) {

  lazy val fieldDFA = {
    val res = if (ef.isDefined) {
      val scheme = ef.get
      val theScheme = scheme.getEscapeSchemeParser(null)
      theScheme match {
        case s: EscapeSchemeBlockParserHelper => CreateFieldDFA()
        case s: EscapeSchemeCharParserHelper => CreateFieldDFA(s.ec, s.eec)
      }

    } else {
      CreateFieldDFA()
    }
    res
  }

  def getFieldDFA(start: PState) = {

    val delimDFAs = start.mpstate.getAllTerminatingMarkup
    val delimsCooked = delimDFAs.map(d => d.lookingFor).toList

    start.mpstate.currentFieldDFA = One(fieldDFA)
    start.mpstate.currentDelimsCooked = One(delimsCooked)
  }
}

case class FieldFactoryDynamic(ef: Option[EscapeSchemeFactoryBase],
  context: ThrowsSDE)
  extends FieldFactoryBase(ef, context) {

  def getFieldDFA(start: PState) = {

    val scheme = start.mpstate.currentEscapeScheme
    val delimDFAs = start.mpstate.getAllTerminatingMarkup
    val delimsCooked = delimDFAs.map(d => d.lookingFor).toList

    val fieldDFA =
      if (scheme.isDefined) {
        val theScheme = scheme.get
        val res = theScheme match {
          case s: EscapeSchemeBlockParserHelper => CreateFieldDFA()
          case s: EscapeSchemeCharParserHelper => CreateFieldDFA(s.ec, s.eec)
        }
        res
      } else {
        CreateFieldDFA()
      }

    start.mpstate.currentFieldDFA = One(fieldDFA)
    start.mpstate.currentDelimsCooked = One(delimsCooked)
  }
}

abstract class EscapeSchemeFactoryBase(
  escapeSchemeObject: EscapeSchemeObject,
  context: ThrowsSDE)
  extends Serializable {

  def escapeKind = escapeSchemeObject.escapeKind

  protected def getOptEscChar = {
    escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => None
      case EscapeKind.EscapeCharacter => {
        if (!escapeSchemeObject.optionEscapeCharacter.isDefined) {
          context.SDE("escapeCharacter cannot be the empty string when EscapeSchemeKind is Character.")
        }
        escapeSchemeObject.optionEscapeCharacter
      }
    }
  }

  protected def getEscValue(escChar: String, context: ThrowsSDE): String = {
    val l = EscapeCharacterCooker.convertConstant(escChar, context, forUnparse = false)
    l
  }

  protected def getBlockStart: String = {
    if (escapeSchemeObject.escapeKind == EscapeKind.EscapeCharacter) Assert.usageError("getBlockStart called when escapeKind = character")
    if (escapeSchemeObject.optionEscapeBlockStart.get == "") { context.SDE("escapeBlockStart cannot be the empty string when EscapeSchemeKind is Block.") }

    val bs = EscapeBlockStartCooker.convertConstant(escapeSchemeObject.optionEscapeBlockStart.get, context, forUnparse = false)
    bs
  }

  protected def getBlockEnd: String = {
    if (escapeSchemeObject.escapeKind == EscapeKind.EscapeCharacter) Assert.usageError("getBlockStart called when escapeKind = character")
    if (escapeSchemeObject.optionEscapeBlockEnd.get == "") { context.SDE("escapeBlockEnd cannot be the empty string when EscapeSchemeKind is Block.") }

    val be = EscapeBlockEndCooker.convertConstant(escapeSchemeObject.optionEscapeBlockEnd.get, context, forUnparse = false)
    be
  }

  protected def getExtraEscapedChars: Maybe[Seq[String]] = {
    val res =
      if (escapeSchemeObject.optionExtraEscapedCharacters.isDefined)
        One(ExtraEscapedCharactersCooker.convertConstant(escapeSchemeObject.optionExtraEscapedCharacters.get, context, forUnparse = false))
      else Nope
    res
  }

  def getEscapeSchemeParser(state: PState): EscapeSchemeParserHelper
  def getEscapeSchemeUnparser(state: UState): EscapeSchemeUnparserHelper

}
case class EscapeSchemeFactoryStatic(
  escapeSchemeObject: EscapeSchemeObject,
  context: ThrowsSDE)
  extends EscapeSchemeFactoryBase(escapeSchemeObject, context) {

  val escChar = getOptEscChar.map { ec =>
    EscapeCharacterCooker.convertConstant(ec.constant, context, forUnparse = false)
  }

  val escEscChar = escapeSchemeObject.optionEscapeEscapeCharacter.map { ec =>
    EscapeEscapeCharacterCooker.convertConstant(ec.constant, context, forUnparse = false)
  }

  def generateEscapeScheme: EscapeSchemeParserHelper = {
    val result = escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => new EscapeSchemeBlockParserHelper(escEscChar, getBlockStart, getBlockEnd)
      case EscapeKind.EscapeCharacter => new EscapeSchemeCharParserHelper(escChar, escEscChar)
    }
    result
  }

  val theScheme = generateEscapeScheme

  def getEscapeSchemeParser(state: PState) = {
    theScheme
  }
  def getEscapeSchemeUnparser(state: UState) = {
    val scheme = escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => new EscapeSchemeBlockUnparserHelper(escEscChar, getBlockStart, getBlockEnd, getExtraEscapedChars, escapeSchemeObject.generateEscapeBlock)
      case EscapeKind.EscapeCharacter => new EscapeSchemeCharUnparserHelper(escChar, escEscChar, getExtraEscapedChars)
    }
    scheme
  }
}

case class EscapeSchemeFactoryDynamic(
  escapeSchemeObject: EscapeSchemeObject,
  context: ThrowsSDE)
  extends EscapeSchemeFactoryBase(escapeSchemeObject, context) with Dynamic {

  val escapeCharacterCached: Maybe[CachedDynamic[String, String]] = {
    escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => // do nothing
      case EscapeKind.EscapeCharacter => {
        if (!escapeSchemeObject.optionEscapeCharacter.isDefined) {
          context.SDE("escapeCharacter cannot be the empty string when EscapeSchemeKind is Character.")
        }
      }
    }
    val ec = escapeSchemeObject.optionEscapeCharacter match {
      case None => Nope
      case Some(c) => One(c)
    }
    cacheConstantExpressionMaybe(ec) {
      (a: Any) => EscapeCharacterCooker.convertConstant(a.asInstanceOf[String], context, forUnparse = false)
    }
  }

  val escapeEscapeCharacterCached: Maybe[CachedDynamic[String, String]] = cacheConstantExpressionMaybe(escapeSchemeObject.optionEscapeEscapeCharacter) {
    (a: Any) => EscapeEscapeCharacterCooker.convertConstant(a.asInstanceOf[String], context, forUnparse = false)
  }

  def getEscapeSchemeParser(state: PState) = {
    val theScheme = escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeCharacter => {
        val finalOptEscChar = evalWithConversionMaybe(state, escapeCharacterCached) {
          (s: ParseOrUnparseState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        val finalOptEscEscChar = evalWithConversionMaybe(state, escapeEscapeCharacterCached) {
          (s: ParseOrUnparseState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        new EscapeSchemeCharParserHelper(finalOptEscChar, finalOptEscEscChar)
      }
      case EscapeKind.EscapeBlock => {
        val finalOptEscEscChar = evalWithConversionMaybe(state, escapeEscapeCharacterCached) {
          (s: ParseOrUnparseState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        new EscapeSchemeBlockParserHelper(finalOptEscEscChar, getBlockStart, getBlockEnd)
      }
    }
    theScheme
  }

  def getEscapeSchemeUnparser(state: UState) = {
    val theScheme = escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeCharacter => {
        val finalOptEscChar = evalWithConversionMaybe(state, escapeCharacterCached) {
          (s: ParseOrUnparseState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        val finalOptEscEscChar = evalWithConversionMaybe(state, escapeEscapeCharacterCached) {
          (s: ParseOrUnparseState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }

        new EscapeSchemeCharUnparserHelper(finalOptEscChar, finalOptEscEscChar, getExtraEscapedChars)
      }
      case EscapeKind.EscapeBlock => {
        val finalOptEscEscChar = evalWithConversionMaybe(state, escapeEscapeCharacterCached) {
          (s: ParseOrUnparseState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }

        new EscapeSchemeBlockUnparserHelper(finalOptEscEscChar, getBlockStart, getBlockEnd, getExtraEscapedChars, escapeSchemeObject.generateEscapeBlock)
      }
    }
    theScheme
  }
}
