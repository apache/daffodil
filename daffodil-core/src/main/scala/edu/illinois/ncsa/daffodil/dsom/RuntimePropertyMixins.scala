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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

/**
 * These are the DFDL properties which can have their values come
 * from the data stream itself by way of expressions.
 *
 * TODO: EscapeScheme's have a few more of these runtime properties
 * escapeCharacter, and escapeEscapeCharacter.
 */
trait TermRuntimeValuedPropertiesMixin
  extends DFDLBaseTypeMixin
  with RawCommonRuntimeValuedPropertiesMixin { decl: Term =>

  private lazy val encodingExpr = LV('encoding) {
    val qn = this.qNameForProperty("encoding")
    this match {
      case eb: ElementBase if (eb.isSimpleType && eb.primType =:= PrimType.HexBinary) => {
        //
        // We treat hex binary as a string in iso-8859-1 encoding.
        // That lets us reuse the various string parsing bases to grab the content.
        //
        val qn = decl.qNameForProperty("encoding")
        val exp = ConstantExpression(qn, PrimType.HexBinary, "iso-8859-1")
        exp
      }
      case _ => ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, encodingRaw)
    }
  }.value

  final lazy val encodingEv = new EncodingEv(encodingExpr, termRuntimeData)

  // these are public so that EncodingChange parsers and unparser can express dependencies on them.
  lazy val decoderEv = new DecoderEv(encodingEv, termRuntimeData)
  lazy val encoderEv = new EncoderEv(encodingEv, termRuntimeData)

  /*
   * Property OutputNewLine
   */

  lazy val outputNewLineEv = {
    val outputNewLineExpr = {
      val qn = this.qNameForProperty("outputNewLine")
      ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, outputNewLineRaw)
    }
    new OutputNewLineEv(outputNewLineExpr, termRuntimeData)
  }

}

trait DelimitedRuntimeValuedPropertiesMixin
  extends TermRuntimeValuedPropertiesMixin
  with RawDelimitedRuntimeValuedPropertiesMixin { decl: Term =>

  // Can be whitespace separated lists, as a result the entity replacement needs to take place elsewhere
  // as it's possible to replace an entity with a whitespace character.
  //  final lazy val initiator = ExpressionCompiler.compile('String, EntityReplacer.replaceAll(initiatorRaw))
  //  final lazy val terminator = ExpressionCompiler.compile('String, EntityReplacer.replaceAll(terminatorRaw))

  lazy val isLengthKindDelimited = {
    decl match {
      case mg: ModelGroup => mg.enclosingElement.get.lengthKind == LengthKind.Delimited
      case eb: ElementBase => eb.lengthKind == LengthKind.Delimited
    }
  }

  private lazy val initiatorExpr = {
    val qn = this.qNameForProperty("initiator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, initiatorRaw)
  }

  lazy val initiatorParseEv = {
    val ev = new InitiatorParseEv(initiatorExpr, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val initiatorUnparseEv = {
    val ev = new InitiatorUnparseEv(initiatorExpr, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }

  final def initiatorLoc = (this.prettyName, this.path)

  private lazy val terminatorExpr = LV('terminator) {
    val qn = this.qNameForProperty("terminator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, terminatorRaw)
  }.value

  final def terminatorLoc = (this.prettyName, this.path)

  lazy val terminatorParseEv = {
    val ev = new TerminatorParseEv(terminatorExpr, isLengthKindDelimited, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val terminatorUnparseEv = {
    val ev = new TerminatorUnparseEv(terminatorExpr, isLengthKindDelimited, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }
}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin
  with LengthAGMixin
  with SimpleTypeRuntimeValuedPropertiesMixin
  with RawElementRuntimeValuedPropertiesMixin { decl: ElementBase =>

  private lazy val byteOrderExpr = LV('byteOrder) {
    val qn = this.qNameForProperty("byteOrder")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, byteOrderRaw)
  }.value

  final lazy val byteOrderEv = LV('byteOrderEv) {
    new ByteOrderEv(byteOrderExpr, elementRuntimeData)
  }.value

  private lazy val lengthExpr = {
    val qn = this.qNameForProperty("length")
    ExpressionCompilers.JLong.compile(qn, NodeInfo.Long, lengthRaw)
  }

  lazy val lengthEv = {
    val ev = new LengthEv(lengthExpr, erd)
    ev.compile()
    ev
  }

  //
  // The occursCount expression is written on the array element, but that expression
  // is actually evaluated before any instances of the element exist. Hence, an
  // expression like { ../c } that appears to be reaching back to a prior peer to get
  // the count... well that expression gets evaluated while we are in the parent context
  // hence if we literally execute the ".." we'll be one element too high in the
  // infoset.
  //
  // So we adjust the expression so that the context is as if on the parent.
  //
  // Not at all sure why this worked with Saxon, but in our new Infoset and DPath
  // implementation, the ".." does get literally evaluated.
  //

  private lazy val occursCountExpr = LV('occursCount) {
    val qn = this.qNameForProperty("occursCount")
    val isEvaluatedAbove = true
    ExpressionCompilers.JLong.compile(qn, NodeInfo.Long, occursCountRaw, isEvaluatedAbove)
  }.value

  lazy val occursCountEv = {
    val ev = new OccursCountEv(occursCountExpr, erd)
    ev.compile()
    ev
  }
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin { decl: GroupBase =>

  private lazy val separatorExpr = {
    val qn = this.qNameForProperty("separator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, separatorRaw)
  }

  lazy val separatorParseEv = {
    val ev = new SeparatorParseEv(separatorExpr, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val separatorUnparseEv = {
    val ev = new SeparatorUnparseEv(separatorExpr, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }

  final def separatorLoc = (this.prettyName, this.path)
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: ElementBase =>

  private lazy val textStandardDecimalSeparatorExpr = LV('textStandardDecimalSeparator) {
    val qn = this.qNameForProperty("textStandardDecimalSeparator")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardDecimalSeparatorRaw)
    c
  }.value

  final lazy val textStandardDecimalSeparatorEv = {
    val ev = new TextStandardDecimalSeparatorEv(textStandardDecimalSeparatorExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textStandardGroupingSeparatorExpr = LV('textStandardGroupingSeparator) {
    val qn = this.qNameForProperty("textStandardGroupingSeparator")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardGroupingSeparatorRaw)
    c
  }.value

  final lazy val textStandardGroupingSeparatorEv = {
    val ev = new TextStandardGroupingSeparatorEv(textStandardGroupingSeparatorExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textStandardExponentRepExpr = LV('textStandardExponentRep) {
    val qn = this.qNameForProperty("textStandardExponentRep")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardExponentRepRaw)
    c
  }.value

  final lazy val textStandardExponentRepEv = {
    val ev = new TextStandardExponentRepEv(textStandardExponentRepExpr, erd)
    ev.compile()
    ev
  }

  private lazy val binaryFloatRepExpr = LV('binaryFloatRep) {
    val qn = this.qNameForProperty("binaryFloatRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, binaryFloatRepRaw)
  }.value

  final lazy val binaryFloatRepEv = {
    val ev = new BinaryFloatRepEv(binaryFloatRepExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textBooleanTrueRepExpr = LV('textBooleanTrueRep) {
    val qn = this.qNameForProperty("textBooleanTrueRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, textBooleanTrueRepRaw)
  }.value

  final lazy val textBooleanTrueRepEv = {
    val ev = new TextBooleanTrueRepEv(textBooleanTrueRepExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textBooleanFalseRepExpr = LV('textBooleanFalseRep) {
    val qn = this.qNameForProperty("textBooleanFalseRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, textBooleanFalseRepRaw)
  }.value

  final lazy val textBooleanFalseRepEv = {
    val ev = new TextBooleanFalseRepEv(textBooleanFalseRepExpr, erd)
    ev.compile()
    ev
  }

  final lazy val calendarLanguage = LV('calendarLanguage) {
    val qn = this.qNameForProperty("calendarLanguage")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, calendarLanguageRaw)
    c
  }.value
}
