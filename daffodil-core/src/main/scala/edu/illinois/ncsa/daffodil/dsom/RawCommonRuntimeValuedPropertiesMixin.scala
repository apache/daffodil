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

import scala.collection.immutable.ListMap
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Utility
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.grammar.EmptyGram
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeScheme_AnnotationMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.processors.VariableUtils

trait RawCommonRuntimeValuedPropertiesMixin
  extends PropertyMixin {

  lazy val byteOrderRaw = findProperty("byteOrder")
  lazy val encodingRaw = findProperty("encoding")
  lazy val outputNewLineRaw = findProperty("outputNewLine")
}

trait RawDelimitedRuntimeValuedPropertiesMixin
  extends RawCommonRuntimeValuedPropertiesMixin {

  lazy val initiatorRaw = findProperty("initiator")
  lazy val terminatorRaw = findProperty("terminator")
}

trait RawElementRuntimeValuedPropertiesMixin
  extends RawDelimitedRuntimeValuedPropertiesMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin {

  // these are almost certainly not in scope, but on the local object
  // but not always. A type might define fixed length things for example.
  lazy val lengthRaw = findProperty("length")
  lazy val occursCountRaw = findProperty("occursCount")
}

trait RawSequenceRuntimeValuedPropertiesMixin
  extends RawDelimitedRuntimeValuedPropertiesMixin {

  lazy val separatorRaw = findProperty("separator")
}

trait RawEscapeSchemeRuntimeValuedPropertiesMixin
  extends PropertyMixin {

  lazy val escapeCharacterRaw = findProperty("escapeCharacter")
  lazy val escapeEscapeCharacterRaw = findProperty("escapeEscapeCharacter")
  lazy val escapeBlockStartRaw = findProperty("escapeBlockStart")
  lazy val escapeBlockEndRaw = findProperty("escapeBlockEnd")
}

trait RawSimpleTypeRuntimeValuedPropertiesMixin
  extends RawCommonRuntimeValuedPropertiesMixin {

  def textStandardDecimalSeparatorRaw = findProperty("textStandardDecimalSeparator")
  def textStandardGroupingSeparatorRaw = findProperty("textStandardGroupingSeparator")
  def textStandardExponentRepRaw = findProperty("textStandardExponentRep")
  def binaryFloatRepRaw = findProperty("binaryFloatRep")
  def textBooleanTrueRepRaw = findProperty("textBooleanTrueRep")
  def textBooleanFalseRepRaw = findProperty("textBooleanFalseRep")

}

