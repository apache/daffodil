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
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.EscapeSchemeObject
import edu.illinois.ncsa.daffodil.dsom.SingleCharacterLiteralES
import edu.illinois.ncsa.daffodil.dsom.StringValueAsLiteral
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.toMaybe

sealed abstract class EscapeScheme
case class EscapeSchemeChar(private val escChar: Maybe[String], private val escEscChar: Maybe[String])
  extends EscapeScheme {
  val ec: Maybe[Char] = if (escChar.isDefined) One(escChar.get.charAt(0)) else Nope
  val eec: Maybe[Char] = if (escEscChar.isDefined) One(escEscChar.get.charAt(0)) else Nope

  override def toString() = "<EscapeSchemeChar escapeChar='" + escChar.getOrElse("") +
    "' escapeEscapeChar='" + escEscChar.getOrElse("") + "'/>"
}
case class EscapeSchemeBlock(private val escEscChar: Maybe[String],
  blockStart: String,
  blockEnd: String)
  extends EscapeScheme {
  // Should note there that fieldDFA (not here) is dependent on
  // the whether or not the delimiters are constant or not.
  // As a result, it cannot be generated here.

  val eec: Maybe[Char] = if (escEscChar.isDefined) One(escEscChar.get.charAt(0)) else Nope
  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(blockStart)
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(blockEnd)
  val fieldEscDFA = CreateFieldDFA(blockEndDFA, eec)

  override def toString() = "<EscapeSchemeBlock escapeEscapeChar='" + escEscChar.getOrElse("") +
    "' blockStart='" + blockStart + "' blockEnd='" + blockEnd + "'/>"
}

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

  def getFieldDFA(state: PState): (PState, Seq[DFADelimiter], List[String], DFAField, Maybe[EscapeScheme])
}

case class FieldFactoryStatic(ef: Option[EscapeSchemeFactoryBase], context: ThrowsSDE)
  extends FieldFactoryBase(ef, context) {

  lazy val fieldDFA = {
    val res = if (ef.isDefined) {
      val scheme = ef.get
      val (_, theScheme) = scheme.getEscapeScheme(null)
      theScheme match {
        case s: EscapeSchemeBlock => CreateFieldDFA()
        case s: EscapeSchemeChar => CreateFieldDFA(s.ec, s.eec)
      }

    } else {
      CreateFieldDFA()
    }
    res
  }

  def getFieldDFA(start: PState) = {
    val scheme = if (ef.isDefined) start.mpstate.currentEscapeScheme else Nope
    val delimsCooked = start.mpstate.getAllTerminatingMarkup.map(d => d.lookingFor).toList
    val delimDFAs = start.mpstate.getAllTerminatingMarkup

    (start, delimDFAs, delimsCooked, fieldDFA, scheme)
  }
}

case class FieldFactoryDynamic(ef: Option[EscapeSchemeFactoryBase],
  context: ThrowsSDE)
  extends FieldFactoryBase(ef, context) {

  def getFieldDFA(start: PState): (PState, Seq[DFADelimiter], List[String], DFAField, Maybe[EscapeScheme]) = {

    val scheme = start.mpstate.currentEscapeScheme
    val delimsCooked = start.mpstate.getAllTerminatingMarkup.map(d => d.lookingFor).toList
    val delimDFAs = start.mpstate.getAllTerminatingMarkup

    val fieldDFA =
      if (scheme.isDefined) {
        val theScheme = scheme.get
        val res = theScheme match {
          case s: EscapeSchemeBlock => CreateFieldDFA()
          case s: EscapeSchemeChar => CreateFieldDFA(s.ec, s.eec)
        }
        res
      } else {
        CreateFieldDFA()
      }

    (start, delimDFAs, delimsCooked, fieldDFA, scheme)
  }
}

abstract class EscapeSchemeFactoryBase(
  escapeSchemeObject: EscapeSchemeObject,
  context: ThrowsSDE)
  extends Serializable {

  def escapeKind = escapeSchemeObject.escapeKind

  protected def constEval(knownValue: Option[String]) = {
    val optConstValue = knownValue match {
      case None => None
      case Some(constValue) => {
        val l = new SingleCharacterLiteralES(constValue, context)
        val result = l.cooked
        Some(result)
      }
    }
    optConstValue
  }

  protected def evalAsConstant(knownValue: Option[CompiledExpression]) = {
    knownValue match {
      case None => None
      case Some(ce) if ce.isConstant => {
        val constValue = ce.constantAsString
        val l = new SingleCharacterLiteralES(constValue, context)
        val result = l.cooked
        Some(result)
      }
      case Some(_) => None
    }
  }

  protected def constEval(knownValue: String, context: ThrowsSDE) = {
    val l = new SingleCharacterLiteralES(knownValue, context)
    val result = l.cooked
    result
  }

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
    val l = new SingleCharacterLiteralES(escChar, context).cooked
    l
  }

  protected def getBlockStart: String = {
    if (escapeSchemeObject.escapeKind == EscapeKind.EscapeCharacter) Assert.usageError("getBlockStart called when escapeKind = character")
    if (escapeSchemeObject.optionEscapeBlockStart.get == "") { context.SDE("escapeBlockStart cannot be the empty string when EscapeSchemeKind is Block.") }

    val bs = new StringValueAsLiteral(escapeSchemeObject.optionEscapeBlockStart.get, context).cooked
    bs
  }

  protected def getBlockEnd: String = {
    if (escapeSchemeObject.escapeKind == EscapeKind.EscapeCharacter) Assert.usageError("getBlockStart called when escapeKind = character")
    if (escapeSchemeObject.optionEscapeBlockEnd.get == "") { context.SDE("escapeBlockEnd cannot be the empty string when EscapeSchemeKind is Block.") }

    val be = new StringValueAsLiteral(escapeSchemeObject.optionEscapeBlockEnd.get, context).cooked
    be
  }

  def getEscapeScheme(state: PState): (PState, EscapeScheme)

}
case class EscapeSchemeFactoryStatic(
  escapeSchemeObject: EscapeSchemeObject,
  context: ThrowsSDE)
  extends EscapeSchemeFactoryBase(escapeSchemeObject, context) {

  val escChar = evalAsConstant(getOptEscChar)
  val escEscChar = evalAsConstant(escapeSchemeObject.optionEscapeEscapeCharacter)

  def generateEscapeScheme: EscapeScheme = {
    val result = escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => new EscapeSchemeBlock(escEscChar, getBlockStart, getBlockEnd)
      case EscapeKind.EscapeCharacter => new EscapeSchemeChar(escChar, escEscChar)
    }
    result
  }

  val theScheme = generateEscapeScheme

  def getEscapeScheme(state: PState) = {
    (state, theScheme)
  }
}

case class EscapeSchemeFactoryDynamic(
  escapeSchemeObject: EscapeSchemeObject,
  context: ThrowsSDE)
  extends EscapeSchemeFactoryBase(escapeSchemeObject, context) with Dynamic {

  val escapeCharacterCached: Maybe[CachedDynamic[String]] = {
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
    cacheConstantExpression(ec) {
      (a: Any) => constEval(a.asInstanceOf[String], context)
    }
  }

  val escapeEscapeCharacterCached: Maybe[CachedDynamic[String]] = cacheConstantExpression(escapeSchemeObject.optionEscapeEscapeCharacter) {
    (a: Any) => constEval(a.asInstanceOf[String], context)
  }

  def getEscapeScheme(state: PState) = {
    val (finalState, theScheme) = escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeCharacter => {
        val (afterEscCharEval, finalOptEscChar) = evalWithConversion(state, escapeCharacterCached) {
          (s: PState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        val (afterEscEscCharEval, finalOptEscEscChar) = evalWithConversion(afterEscCharEval, escapeEscapeCharacterCached) {
          (s: PState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        (afterEscEscCharEval, new EscapeSchemeChar(finalOptEscChar, finalOptEscEscChar))
      }
      case EscapeKind.EscapeBlock => {
        val (afterEscEscCharEval, finalOptEscEscChar) = evalWithConversion(state, escapeEscapeCharacterCached) {
          (s: PState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        (afterEscEscCharEval, new EscapeSchemeBlock(finalOptEscEscChar, getBlockStart, getBlockEnd))
      }
    }
    (finalState, theScheme)
  }
}

