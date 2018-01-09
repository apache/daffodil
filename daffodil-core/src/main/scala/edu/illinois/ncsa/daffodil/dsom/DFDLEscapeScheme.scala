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

import scala.xml.Node
import scala.xml.Utility
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeScheme_AnnotationMixin
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeParseEv
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlockParseEv
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeCharParseEv
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeUnparseEv
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlockUnparseEv
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeCharUnparseEv
import edu.illinois.ncsa.daffodil.processors.EscapeCharEv
import edu.illinois.ncsa.daffodil.processors.EscapeEscapeCharEv
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyLookupResult
import edu.illinois.ncsa.daffodil.schema.annotation.props.NotFound
import edu.illinois.ncsa.daffodil.schema.annotation.props.Found

final class DFDLEscapeScheme(node: Node, decl: AnnotatedSchemaComponent, defES: DFDLDefineEscapeScheme)
  extends DFDLFormatAnnotation(node, decl)
  with EscapeScheme_AnnotationMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {

  final protected override def enclosingComponentDef = Some(defES)

  final lazy val referringComponent: Option[SchemaComponent] = Some(defES)

  final override def findPropertyOption(pname: String): PropertyLookupResult = {
    ExecutionMode.requireCompilerMode // never get properties at runtime, only compile time.
    val propNodeSeq = xml.attribute(pname)
    propNodeSeq match {
      case None => NotFound(Seq(this), Nil, pname) // attribute was not found
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
          case Nil => Found("", this, pname, false) // we want to hand back the empty string as a value.
          case _ => Found(nodeseq.toString, this, pname, false)
        }
      }
    }
  }

  final def escapeCharacterEv = LV('escapeCharacterEv) {
    val qn = this.qNameForProperty("escapeCharacter")
    val expr = ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, escapeCharacterRaw, this)
    val ev = new EscapeCharEv(expr, runtimeData)
    ev.compile()
    ev
  }.value

  final def optionEscapeEscapeCharacterEv = LV('optionEscapeEscapeCharacterEv) {
    val qn = this.qNameForProperty("escapeEscapeCharacter")
    escapeEscapeCharacterRaw match {
      case Found("", loc, _, _) => Nope
      case found @ Found(v, loc, _, _) => {
        val typeIfStaticallyKnown = NodeInfo.String
        val typeIfRuntimeKnown = NodeInfo.NonEmptyString
        val expr = ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, found, this)
        val ev = new EscapeEscapeCharEv(expr, runtimeData)
        ev.compile()
        One(ev)
      }
    }
  }.value

  final def optionExtraEscapedCharacters = LV('optionExtraEscapedCharacters) {
    extraEscapedCharactersRaw match {
      case Found("", _, _, _) => Nope
      case Found(v, _, _, _) => One(v)
    }
  }.value

  final lazy val escapeSchemeParseEv: EscapeSchemeParseEv = {
    val espev = escapeKind match {
      case EscapeKind.EscapeBlock => new EscapeSchemeBlockParseEv(escapeBlockStart, escapeBlockEnd, optionEscapeEscapeCharacterEv, runtimeData)
      case EscapeKind.EscapeCharacter => new EscapeSchemeCharParseEv(escapeCharacterEv, optionEscapeEscapeCharacterEv, runtimeData)
    }
    espev.compile()
    espev
  }

  final lazy val escapeSchemeUnparseEv: EscapeSchemeUnparseEv = {
    val esuev = escapeKind match {
      case EscapeKind.EscapeBlock => new EscapeSchemeBlockUnparseEv(escapeBlockStart, escapeBlockEnd, optionEscapeEscapeCharacterEv, optionExtraEscapedCharacters, generateEscapeBlock, runtimeData)
      case EscapeKind.EscapeCharacter => new EscapeSchemeCharUnparseEv(escapeCharacterEv, optionEscapeEscapeCharacterEv, optionExtraEscapedCharacters, runtimeData)
    }
    esuev.compile()
    esuev
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

  final override protected def enclosingComponentDef = Some(pointOfUse)

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
    "dfdl:defineEscapeScheme " + name
  }
}
