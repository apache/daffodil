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

  lazy val byteOrder = expressionCompiler.compile(ConvertToType.String, byteOrderRaw)
  lazy val encoding = expressionCompiler.compile(ConvertToType.String, encodingRaw)
  lazy val outputNewLine = {
    //
    // FIXME unparser: outputNewLineRaw might be a literal, in which case
    // we do entity replacements. However, if it is an expression, we don't 
    // do entity replacements. This code just always replaces entities.
    val exprOrLiteral = EntityReplacer.replaceAll(outputNewLineRaw.value, Some(decl))

    val c = expressionCompiler.compile(ConvertToType.String, Found(exprOrLiteral, outputNewLineRaw.location))
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
  lazy val initiator = {
    val c = expressionCompiler.compile(ConvertToType.String, initiatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%ES;"), "Initiator cannot contain ES")
    }
    c
  }
  //  lazy val initatorLoc = {
  //    val (rawValue: String, location: LookupLocation) = 
  //      initiatorRaw match {
  //      case f: Found => (f.value, f.location)
  //      case _ => Assert.impossibleCase()
  //    }
  //    val sc = location.asInstanceOf[SchemaComponentBase]
  //    (sc.prettyName, sc.path)
  //  }

  lazy val terminator = {
    val c = expressionCompiler.compile(ConvertToType.String, terminatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%ES;"), "Terminator cannot contain ES")
    }
    c
  }
  lazy val terminatorLoc = terminatorRaw.location.nameAndPath

}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin
  with LengthAGMixin
  with SimpleTypeRuntimeValuedPropertiesMixin
  with RawElementRuntimeValuedPropertiesMixin { decl: ElementBase =>

  lazy val length = expressionCompiler.compile(ConvertToType.Long, lengthRaw)
  lazy val occursCount = expressionCompiler.compile(ConvertToType.Long, occursCountRaw)
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin { decl: GroupBase =>

  lazy val separator = {
    val c = expressionCompiler.compile(ConvertToType.String, separatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinitionUnless(!s.contains("%ES;"), "Separator cannot contain ES")
    }
    c
  }

  lazy val separatorLoc = separatorRaw.location.nameAndPath
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: SchemaComponent =>

  def textStandardDecimalSeparator = {
    val c = expressionCompiler.compile(ConvertToType.String, textStandardDecimalSeparatorRaw)
    c
  }

  def textStandardGroupingSeparator = {
    val c = expressionCompiler.compile(ConvertToType.String, textStandardGroupingSeparatorRaw)
    c
  }

  def textStandardExponentRep = {
    val c = expressionCompiler.compile(ConvertToType.String, textStandardExponentRepRaw)
    c
  }

  def binaryFloatRep = expressionCompiler.compile(ConvertToType.String, binaryFloatRepRaw)

  // TODO: Will need to 'evaluate' and perform entity replacement on textBooleanTrueRep in Parser where it is used.
  def textBooleanTrueRep = {
    val c = expressionCompiler.compile(ConvertToType.String, textBooleanTrueRepRaw)
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
  def textBooleanFalseRep = {
    val c = expressionCompiler.compile(ConvertToType.String, textBooleanFalseRepRaw)
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

