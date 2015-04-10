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

final class DFDLEscapeScheme(node: Node, decl: AnnotatedSchemaComponent, defES: DFDLDefineEscapeScheme)
  extends DFDLFormatAnnotation(node, decl)
  with EscapeScheme_AnnotationMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {

  final override lazy val enclosingComponent = Some(defES)

  final lazy val referringComponent: Option[SchemaComponent] = Some(defES)

  final override def findPropertyOption(pname: String): PropertyLookupResult = {
    ExecutionMode.requireCompilerMode // never get properties at runtime, only compile time.
    val propNodeSeq = xml.attribute(pname)
    propNodeSeq match {
      case None => NotFound(Seq(this), Nil) // attribute was not found
      case Some(nodeseq) => {
        //
        // Interesting that attributeName="" produces a Nil nodeseq, not an empty string.
        // 
        // This whole attributes as NodeSeq thing in Scala seems strange, but attributes
        // can contain unresolved entities, e.g., quote="&amp;quot;2B || ! 2B&amp;quot;"
        // so really they do have to return them as node sequences. It requires DTD processing
        // to resolve everything, and most code isn't going to process the DTDs. I.e., the scala 
        // XML library lets your code be the one doing the DTD resolving, so they can't do it for you.
        //
        nodeseq match {
          case Nil => Found("", this) // we want to hand back the empty string as a value.
          case _ => Found(nodeseq.toString, this)
        }
      }
    }
  }

  final def optionEscapeCharacter = LV('optionEscapeCharacter) {
    escapeCharacterRaw match {
      case Found("", loc) => None
      case found @ Found(v, loc) => Some(ExpressionCompiler.compile(NodeInfo.NonEmptyString, found))
    }
  }.value

  final def optionEscapeEscapeCharacter = LV('optionEscapeEscapeCharacter) {
    escapeEscapeCharacterRaw match {
      case Found("", loc) => None
      case found @ Found(v, loc) => {
        val typeIfStaticallyKnown = NodeInfo.String
        val typeIfRuntimeKnown = NodeInfo.NonEmptyString
        Some(ExpressionCompiler.compile(typeIfStaticallyKnown, typeIfRuntimeKnown, found))
      }
    }
  }.value

  final def optionEscapeBlockStart = LV('optionEscapeBlockStart) {
    escapeBlockStartRaw match {
      case Found("", loc) => None
      case Found(v, loc) => Some(v)
    }
  }.value

  final def optionEscapeBlockEnd = LV('optionEscapeBlockEnd) {
    escapeBlockEndRaw match {
      case Found("", loc) => None
      case Found(v, loc) => Some(v)
    }
  }.value

  final lazy val escapeScheme: EscapeSchemeObject = this.escapeKind match {
    case EscapeKind.EscapeBlock => new EscapeSchemeObject(this.escapeKind, None, this.optionEscapeEscapeCharacter, this.optionEscapeBlockStart, this.optionEscapeBlockEnd)
    case EscapeKind.EscapeCharacter => new EscapeSchemeObject(this.escapeKind, this.optionEscapeCharacter, this.optionEscapeEscapeCharacter, None, None)
  }

  //
  // These are sort of tri-state. We can know it is non-existant, 
  // we can know it and have its value, or we must check at runtime, 
  // which means there WILL be an escape character, we just don't know what it is.
  // Represent as follows None, Some(true), Some(false)
  final lazy val (isKnownEscapeCharacter, knownEscapeCharacter) = {
    optionEscapeCharacter match {
      case None => (None, None) // there isn't one
      case Some(ce) if (ce.isConstant) => (Some(true), Some(ce.constantAsString))
      case _ => (Some(false), None) // must evaluate at runtime
    }
  }

  final lazy val (isKnownEscapeEscapeCharacter, knownEscapeEscapeCharacter) = {
    optionEscapeEscapeCharacter match {
      case None => (None, None) // there isn't one
      case Some(ce) if (ce.isConstant) => (Some(true), Some(ce.constantAsString))
      case _ => (Some(false), None) // must evaluate at runtime
    }
  }

}

final class DFDLDefineEscapeSchemeFactory(node: Node, decl: SchemaDocument)
  extends DFDLDefiningAnnotation(node, decl) {
  def forComponent(pointOfUse: SchemaComponent) = new DFDLDefineEscapeScheme(node, decl, pointOfUse)
}

final class DFDLDefineEscapeScheme(node: Node, decl: SchemaDocument, pointOfUse: SchemaComponent)
  extends DFDLDefiningAnnotation(node, decl) // Note: defineEscapeScheme isn't a format annotation itself.
  // with DefineEscapeScheme_AnnotationMixin 
  {

  override lazy val enclosingComponent = Some(pointOfUse)

  /*
   * For diagnostic messages, we need the decl - because that's where the 
   * escapescheme definition is written.
   * 
   * But for purposes of compilation of expressions, we need the 
   * nearest enclosing element. That will be made to happen by way of 
   * the ExpressionCompiler - every schema component potentially has one.
   */
  requiredEvaluations(escapeScheme)

  lazy val referringComponent: Option[SchemaComponent] = Some(pointOfUse)

  lazy val escapeScheme = {
    val des = Utility.trim(node)
    val res = des match {
      case <dfdl:defineEscapeScheme>{ e @ <dfdl:escapeScheme>{ contents @ _* }</dfdl:escapeScheme> }</dfdl:defineEscapeScheme> =>
        new DFDLEscapeScheme(e, decl, this)
      case _ => SDE("The content of %s is not complete.", des.label)
    }
    res
  }

  override def toString(): String = {
    "DFDLDefineEscapeScheme." + name
  }
}
