package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer._
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.dpath.NodeInfo

/**
 * These are the DFDL properties which can have their values come
 * from the data stream itself by way of expressions.
 *
 * TODO: EscapeScheme's have a few more of these runtime properties
 * escapeCharacter, and escapeEscapeCharacter.
 */
trait CommonRuntimeValuedPropertiesMixin
  extends DFDLBaseTypeMixin
  with RawCommonRuntimeValuedPropertiesMixin { decl: SchemaComponent =>

  lazy val byteOrder = _byteOrder.value
  private val _byteOrder = LV('byteOrder) { expressionCompiler.compile(NodeInfo.NonEmptyString, byteOrderRaw) }
  lazy val encoding = _encoding.value
  private val _encoding = LV('encoding) { expressionCompiler.compile(NodeInfo.NonEmptyString, encodingRaw) }
  lazy val outputNewLine = _outputNewLine.value
  private val _outputNewLine = LV('outputNewLine) {
    //
    // FIXME unparser: outputNewLineRaw might be a literal, in which case
    // we do entity replacements. However, if it is an expression, we don't
    // do entity replacements. This code just always replaces entities.
    val exprOrLiteral = EntityReplacer { _.replaceAll(outputNewLineRaw.value, Some(decl)) }

    val c = expressionCompiler.compile(NodeInfo.NonEmptyString, Found(exprOrLiteral, outputNewLineRaw.location))
    if (c.isConstant) {
      val s = c.constantAsString
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
  }
}

trait DelimitedRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with RawDelimitedRuntimeValuedPropertiesMixin { decl: SchemaComponent =>

  // Can be whitespace separated lists, as a result the entity replacement needs to take place elsewhere
  // as it's possible to replace an entity with a whitespace character.
  //  lazy val initiator = expressionCompiler.compile('String, EntityReplacer.replaceAll(initiatorRaw))
  //  lazy val terminator = expressionCompiler.compile('String, EntityReplacer.replaceAll(terminatorRaw))
  lazy val initiator = _initiator.value
  private val _initiator = LV('initiator) {
    val c = {
      val typeIfStaticallyKnown = NodeInfo.String
      val typeIfRuntimeKnown = NodeInfo.NonEmptyString
      expressionCompiler.compile(typeIfStaticallyKnown, typeIfRuntimeKnown, initiatorRaw)
    }
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%ES;"), "Initiator cannot contain ES")
    }
    c
  }

  lazy val terminator = _terminator.value
  private val _terminator = LV('terminator) {
    val c = {
      val typeIfStaticallyKnown = NodeInfo.String
      val typeIfRuntimeKnown = NodeInfo.NonEmptyString
      expressionCompiler.compile(typeIfStaticallyKnown, typeIfRuntimeKnown, terminatorRaw)
    }
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%ES;"), "Terminator cannot contain ES")
    }
    c
  }
  lazy val terminatorLoc = (this.prettyName, this.path)

}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin
  with LengthAGMixin
  with SimpleTypeRuntimeValuedPropertiesMixin
  with RawElementRuntimeValuedPropertiesMixin { decl: ElementBase =>

  lazy val length = _length.value
  private val _length = LV('length) { expressionCompiler.compile(NodeInfo.UnsignedLong, lengthRaw) } // NodeInfo.UnsignedInt

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
  lazy val occursCount = _occursCount.value
  private val _occursCount = LV('occursCount) {
    val isEvaluatedAbove = true
    expressionCompiler.compile(NodeInfo.UnsignedLong, occursCountRaw, isEvaluatedAbove) //NodeInfo.UnsignedInt
  }
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin { decl: GroupBase =>

  lazy val separator = _separator.value
  private val _separator = LV('separator) {
    val c = {
      val typeIfStaticallyKnown = NodeInfo.String
      val typeIfRuntimeKnown = NodeInfo.NonEmptyString
      expressionCompiler.compile(typeIfStaticallyKnown, typeIfRuntimeKnown, separatorRaw)
    }
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%ES;"), "Separator cannot contain ES")
    }
    c
  }

  lazy val separatorLoc = (this.prettyName, this.path)
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: SchemaComponent =>

  lazy val textStandardDecimalSeparator = _textStandardDecimalSeparator.value
  private val _textStandardDecimalSeparator = LV('textStandardDecimalSeparator) {
    val c = expressionCompiler.compile(NodeInfo.String, textStandardDecimalSeparatorRaw)
    c
  }

  lazy val textStandardGroupingSeparator = _textStandardGroupingSeparator.value
  private val _textStandardGroupingSeparator = LV('textStandardGroupingSeparator) {
    val c = expressionCompiler.compile(NodeInfo.String, textStandardGroupingSeparatorRaw)
    c
  }

  lazy val textStandardExponentRep = _textStandardExponentRep.value
  private val _textStandardExponentRep = LV('textStandardExponentRep) {
    val c = expressionCompiler.compile(NodeInfo.String, textStandardExponentRepRaw)
    c
  }

  lazy val binaryFloatRep = _binaryFloatRep.value
  private val _binaryFloatRep = LV('binaryFloatRep) { expressionCompiler.compile(NodeInfo.NonEmptyString, binaryFloatRepRaw) }

  // TODO: Will need to 'evaluate' and perform entity replacement on textBooleanTrueRep in Parser where it is used.
  lazy val textBooleanTrueRep = _textBooleanTrueRep.value
  private val _textBooleanTrueRep = LV('textBooleanTrueRep) {
    val c = expressionCompiler.compile(NodeInfo.NonEmptyString, textBooleanTrueRepRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%NL;"), "textBooleanTrueRep cannot contain NL")
      this.schemaDefinitionUnless(!s.contains("%WSP;"), "textBooleanTrueRep cannot contain WSP")
      this.schemaDefinitionUnless(!s.contains("%WSP*;"), "textBooleanTrueRep cannot contain WSP*")
      this.schemaDefinitionUnless(!s.contains("%WSP+;"), "textBooleanTrueRep cannot contain WSP+")
      this.schemaDefinitionUnless(!s.contains("%ES;"), "textBooleanTrueRep cannot contain ES")
    }
    c
  }

  // TODO: Will need to 'evaluate' and perform entity replacement on textBooleanFalseRep in Parser where it is used.
  lazy val textBooleanFalseRep = _textBooleanFalseRep.value
  private val _textBooleanFalseRep = LV('textBooleanFalseRep) {
    val c = expressionCompiler.compile(NodeInfo.NonEmptyString, textBooleanFalseRepRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%NL;"), "textBooleanFalseRep cannot contain NL")
      this.schemaDefinitionUnless(!s.contains("%WSP;"), "textBooleanFalseRep cannot contain WSP")
      this.schemaDefinitionUnless(!s.contains("%WSP*;"), "textBooleanFalseRep cannot contain WSP*")
      this.schemaDefinitionUnless(!s.contains("%WSP+;"), "textBooleanFalseRep cannot contain WSP+")
      this.schemaDefinitionUnless(!s.contains("%ES;"), "textBooleanFalseRep cannot contain ES")
    }
    c
  }

}
