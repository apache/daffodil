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

  final lazy val encoding = LV('encoding) {
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

  final lazy val encodingEv = new EncodingEv(encoding, termRuntimeData)

  // these are public so that EncodingChange parsers and unparser can express dependencies on them.
  lazy val decoderEv = new DecoderEv(encodingEv, termRuntimeData)
  lazy val encoderEv = new EncoderEv(encodingEv, termRuntimeData)

  /*
   * Property OutputNewLine
   *
   * This has not been cut over to use Evaluatables. There were a number of subtle problems
   * this lead to. The commented 3 lines below are what this eventually wants to become.
   *
   * This transition is best done in an isolated commit.
   */

  // compatibility with code that expects to look at CompiledExpression
  final lazy val outputNewLine2 = {
    outputNewLineEv.compile()
    outputNewLineExpr
  }

  private final lazy val outputNewLineExpr = {
    val qn = this.qNameForProperty("outputNewLine")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, outputNewLineRaw)
  }

  lazy val outputNewLineEv =
    new OutputNewLineEv(outputNewLineExpr, termRuntimeData)

  /**
   * This is pretty much the original code for outputNewLine with its own ad-hoc checks done
   * in the static case, and not using the new cookers or anything else.
   */
  // @deprecated("2016", "use outputNewLineEv")
  final lazy val outputNewLine = LV('outputNewLine) {
    val qn = this.qNameForProperty("outputNewLine")

    // First compile is just to take advantage of the expression compiler's ability
    // to detect expressions vs. string literals, so we know whether or not to substitute
    // for entities.
    val ce = ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, outputNewLineRaw)
    val exprOrLiteral = if (ce.isConstant) EntityReplacer { _.replaceAll(ce.constant, Some(decl)) } else outputNewLineRaw.value

    val c =
      if (ce.isConstant) {
        // compile again, since we've now substituted for entities
        ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, Found(exprOrLiteral, outputNewLineRaw.location, "outputNewLine"))
      } else ce // use what we already compiled.
    if (c.isConstant) {
      val s = c.constant
      this.schemaDefinitionUnless(!s.contains("%NL;"), "outputNewLine cannot contain NL")
      this.schemaDefinitionUnless(!s.contains("%WSP;"), "outputNewLine cannot contain WSP")
      this.schemaDefinitionUnless(!s.contains("%WSP+;"), "outputNewLine cannot contain WSP+")
      this.schemaDefinitionUnless(!s.contains("%WSP*;"), "outputNewLine cannot contain WSP*")
      this.schemaDefinitionUnless(!s.contains("%ES;"), "outputNewLine cannot contain ES")

      val validNLs: List[Char] = List('\u000A', '\u000D', '\u0085', '\u2028')
      s.foreach(x => {
        this.schemaDefinitionUnless(validNLs.contains(x), "'" + x + "' is not a valid new line character for outputNewLine!")
      })
    }
    c
  }.value

}

trait DelimitedRuntimeValuedPropertiesMixin
  extends TermRuntimeValuedPropertiesMixin
  with RawDelimitedRuntimeValuedPropertiesMixin { decl: Term =>

  // Can be whitespace separated lists, as a result the entity replacement needs to take place elsewhere
  // as it's possible to replace an entity with a whitespace character.
  //  final lazy val initiator = ExpressionCompiler.compile('String, EntityReplacer.replaceAll(initiatorRaw))
  //  final lazy val terminator = ExpressionCompiler.compile('String, EntityReplacer.replaceAll(terminatorRaw))

  // compatibility with code that expects to look at CompiledExpression
  lazy val initiator = {
    initiatorEv.compile()
    initiatorExpr
  }

  private lazy val initiatorExpr = {
    val qn = this.qNameForProperty("initiator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, initiatorRaw)
  }

  lazy val initiatorEv = new InitiatorEv(initiatorExpr, decl.termRuntimeData)

  final def initiatorLoc = (this.prettyName, this.path)

  // compatibility with code that expects to look at CompiledExpression
  lazy val terminator = {
    terminatorEv.compile()
    terminatorExpr
  }

  private lazy val terminatorExpr = LV('terminator) {
    val qn = this.qNameForProperty("terminator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, terminatorRaw)
  }.value

  final def terminatorLoc = (this.prettyName, this.path)

  lazy val terminatorEv = new TerminatorEv(terminatorExpr, decl.termRuntimeData)
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

  final lazy val length = {
    val qn = this.qNameForProperty("length")
    ExpressionCompilers.JLong.compile(qn, NodeInfo.Long, lengthRaw)
  }

  lazy val lengthEv = new LengthEv(length, erd)

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
  final lazy val occursCount = LV('occursCount) {
    val qn = this.qNameForProperty("occursCount")
    val isEvaluatedAbove = true
    ExpressionCompilers.JLong.compile(qn, NodeInfo.Long, occursCountRaw, isEvaluatedAbove)
  }.value

  lazy val occursCountEv = new OccursCountEv(occursCount, erd)
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin { decl: GroupBase =>

  // compatibility with code that expects to look at CompiledExpression
  final lazy val separator = {
    separatorEv.compile()
    separatorExpr
  }

  private lazy val separatorExpr = {
    val qn = this.qNameForProperty("separator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, separatorRaw)
  }

  lazy val separatorEv = new SeparatorEv(separatorExpr, decl.termRuntimeData)

  final def separatorLoc = (this.prettyName, this.path)
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: ElementBase =>

  final lazy val textStandardDecimalSeparator = textStandardDecimalSeparatorExpr

  private lazy val textStandardDecimalSeparatorExpr = LV('textStandardDecimalSeparator) {
    val qn = this.qNameForProperty("textStandardDecimalSeparator")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardDecimalSeparatorRaw)
    c
  }.value

  final lazy val textStandardDecimalSeparatorEv = new TextStandardDecimalSeparatorEv(textStandardDecimalSeparatorExpr, erd)

  final lazy val textStandardGroupingSeparator = textStandardGroupingSeparatorExpr

  private lazy val textStandardGroupingSeparatorExpr = LV('textStandardGroupingSeparator) {
    val qn = this.qNameForProperty("textStandardGroupingSeparator")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardGroupingSeparatorRaw)
    c
  }.value

  final lazy val textStandardGroupingSeparatorEv = new TextStandardGroupingSeparatorEv(textStandardGroupingSeparatorExpr, erd)

  final lazy val textStandardExponentRep = textStandardExponentRepExpr

  private lazy val textStandardExponentRepExpr = LV('textStandardExponentRep) {
    val qn = this.qNameForProperty("textStandardExponentRep")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardExponentRepRaw)
    c
  }.value

  final lazy val textStandardExponentRepEv = new TextStandardExponentRepEv(textStandardExponentRepExpr, erd)

  final lazy val binaryFloatRep = binaryFloatRepExpr

  private lazy val binaryFloatRepExpr = LV('binaryFloatRep) {
    val qn = this.qNameForProperty("binaryFloatRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, binaryFloatRepRaw)
  }.value

  final lazy val binaryFloatRepEv = new BinaryFloatRepEv(binaryFloatRepExpr, erd)

  final lazy val textBooleanTrueRep = textBooleanTrueRepExpr

  private lazy val textBooleanTrueRepExpr = LV('textBooleanTrueRep) {
    val qn = this.qNameForProperty("textBooleanTrueRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, textBooleanTrueRepRaw)
  }.value

  final lazy val textBooleanTrueRepEv = new TextBooleanTrueRepEv(textBooleanTrueRepExpr, erd)

  final lazy val textBooleanFalseRep = textBooleanFalseRepExpr

  private lazy val textBooleanFalseRepExpr = LV('textBooleanFalseRep) {
    val qn = this.qNameForProperty("textBooleanFalseRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, textBooleanFalseRepRaw)
  }.value

  final lazy val textBooleanFalseRepEv = new TextBooleanFalseRepEv(textBooleanFalseRepExpr, erd)

  final lazy val calendarLanguage = LV('calendarLanguage) {
    val qn = this.qNameForProperty("calendarLanguage")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, calendarLanguageRaw)
    c
  }.value
}
