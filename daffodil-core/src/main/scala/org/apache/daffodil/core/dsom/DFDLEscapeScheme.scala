/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom

import scala.xml.Elem
import scala.xml.Node
import scala.xml.Utility

import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.NotFound
import org.apache.daffodil.lib.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.lib.schema.annotation.props.gen.EscapeKind
import org.apache.daffodil.lib.schema.annotation.props.gen.EscapeScheme_AnnotationMixin
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors.EscapeCharEv
import org.apache.daffodil.runtime1.processors.EscapeEscapeCharEv
import org.apache.daffodil.runtime1.processors.EscapeSchemeBlockParseEv
import org.apache.daffodil.runtime1.processors.EscapeSchemeBlockUnparseEv
import org.apache.daffodil.runtime1.processors.EscapeSchemeCharParseEv
import org.apache.daffodil.runtime1.processors.EscapeSchemeCharUnparseEv
import org.apache.daffodil.runtime1.processors.EscapeSchemeParseEv
import org.apache.daffodil.runtime1.processors.EscapeSchemeUnparseEv
import org.apache.daffodil.runtime1.processors.ExtraEscapedCharsEv

final class DFDLEscapeScheme(
  node: Node,
  decl: AnnotatedSchemaComponent,
  defES: DFDLDefineEscapeScheme
) extends DFDLFormatAnnotation(node, decl)
  with EscapeScheme_AnnotationMixin
  with RawEscapeSchemeRuntimeValuedPropertiesMixin {

  final lazy val referringComponent: Option[SchemaComponent] = Some(defES)

  protected final override def lookupProperty(pname: String): PropertyLookupResult = {
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
          case Nil =>
            Found("", this, pname, false) // we want to hand back the empty string as a value.
          case _ => Found(nodeseq.toString, this, pname, false)
        }
      }
    }
  }

  /**
   * For unit testing. Must override because of multiple inheritance.
   *
   * The scala compiler complains as follows:
   * class DFDLEscapeScheme inherits conflicting members:
   * method verifyPropValue in class DFDLFormatAnnotation
   * of type (propName: String, expectedValue: String)Boolean
   * and method verifyPropValue in trait FindPropertyMixin
   * of type (key: String, value: String)Boolean
   * (Note: this can be resolved by declaring an override in class DFDLEscapeScheme.)
   */
  override def verifyPropValue(key: String, value: String): Boolean =
    super.verifyPropValue(key, value)

  final lazy val escapeCharacterEv = LV(Symbol("escapeCharacterEv")) {
    val qn = this.qNameForProperty("escapeCharacter")
    val expr = ExpressionCompilers.String.compileProperty(
      qn,
      NodeInfo.NonEmptyString,
      escapeCharacterRaw,
      this,
      defES.pointOfUse.dpathCompileInfo
    )
    val ev = new EscapeCharEv(expr, ci)
    ev.compile(tunable)
    ev
  }.value

  final lazy val optionEscapeEscapeCharacterEv = LV(Symbol("optionEscapeEscapeCharacterEv")) {
    val qn = this.qNameForProperty("escapeEscapeCharacter")
    escapeEscapeCharacterRaw match {
      case Found("", loc, _, _) => Nope
      case found @ Found(v, loc, _, _) => {
        val typeIfStaticallyKnown = NodeInfo.String
        val typeIfRuntimeKnown = NodeInfo.NonEmptyString
        val ci = defES.pointOfUse.dpathCompileInfo
        val expr = ExpressionCompilers.String.compileDelimiter(
          qn,
          typeIfStaticallyKnown,
          typeIfRuntimeKnown,
          found,
          this,
          ci
        )
        val ev = new EscapeEscapeCharEv(expr, ci)
        ev.compile(tunable)
        One(ev)
      }
    }
  }.value

  final lazy val optionExtraEscapedCharactersEv = LV(Symbol("optionExtraEscapedCharacters")) {
    val qn = this.qNameForProperty("extraEscapedCharacters")
    extraEscapedCharactersRaw match {
      case Found("", _, _, _) => Nope
      case found @ Found(v, _, _, _) => {
        val typeIfStaticallyKnown = NodeInfo.String
        val typeIfRuntimeKnown = NodeInfo.NonEmptyString
        val ci = defES.pointOfUse.dpathCompileInfo
        val expr = ExpressionCompilers.String.compileDelimiter(
          qn,
          typeIfStaticallyKnown,
          typeIfRuntimeKnown,
          found,
          this,
          ci
        )
        val ev = new ExtraEscapedCharsEv(expr, ci)
        ev.compile(tunable)
        One(ev)
      }
    }
  }.value

  final lazy val escapeSchemeParseEv: EscapeSchemeParseEv = {
    val espev = escapeKind match {
      case EscapeKind.EscapeBlock =>
        new EscapeSchemeBlockParseEv(
          escapeBlockStart,
          escapeBlockEnd,
          optionEscapeEscapeCharacterEv,
          ci
        )
      case EscapeKind.EscapeCharacter =>
        new EscapeSchemeCharParseEv(escapeCharacterEv, optionEscapeEscapeCharacterEv, ci)
    }
    espev.compile(tunable)
    espev
  }

  final lazy val escapeSchemeUnparseEv: EscapeSchemeUnparseEv = {
    val esuev = escapeKind match {
      case EscapeKind.EscapeBlock =>
        new EscapeSchemeBlockUnparseEv(
          escapeBlockStart,
          escapeBlockEnd,
          optionEscapeEscapeCharacterEv,
          optionExtraEscapedCharactersEv,
          generateEscapeBlock,
          ci
        )
      case EscapeKind.EscapeCharacter =>
        new EscapeSchemeCharUnparseEv(
          escapeCharacterEv,
          optionEscapeEscapeCharacterEv,
          optionExtraEscapedCharactersEv,
          ci
        )
    }
    esuev.compile(tunable)
    esuev
  }

}

object DFDLDefineEscapeSchemeFactory {
  def apply(node: Node, decl: SchemaDocument) = {
    val desf = new DFDLDefineEscapeSchemeFactory(node, decl)
    desf.initialize()
    desf
  }
}

final class DFDLDefineEscapeSchemeFactory private (node: Node, decl: SchemaDocument)
  extends DFDLDefiningAnnotation(node, decl) {
  def forComponent(pointOfUse: SchemaComponent) = DFDLDefineEscapeScheme(node, decl, pointOfUse)
}

object DFDLDefineEscapeScheme {
  def apply(node: Node, sd: SchemaDocument, pointOfUse: SchemaComponent) = {
    val des = new DFDLDefineEscapeScheme(node, sd, pointOfUse)
    des.initialize()
    des
  }
}

/**
 * Escape scheme definitions
 *
 * These cannot be shared easily, since they can contain expressions (for escapeCharacter and
 * escapeEscapeCharacter) which must be evaluated in the context of a point of use location.
 *
 * These are copied for each point of use so that these properties behave the way regular properties
 * that are not grouped together on a common structure, all work, which is that each term
 * has its own expression compilation for these properties, as it would for any regular expression-valued
 * property like byteOrder or encoding or a delimiter.
 */
final class DFDLDefineEscapeScheme private (
  node: Node,
  decl: SchemaDocument,
  val pointOfUse: SchemaComponent
) extends DFDLDefiningAnnotation(node, decl) { // Note: defineEscapeScheme isn't a format annotation itself.

  requiredEvaluationsAlways(escapeScheme)

  /**
   * For diagnostic messages, we need the decl - because that's where the
   * escapescheme definition is written.
   *
   * But for purposes of compilation of expressions, we need the
   * nearest enclosing element. That will be made to happen by way of
   * the ExpressionCompiler - every schema component potentially has one.
   */
  lazy val referringComponent: Option[SchemaComponent] = Some(pointOfUse)

  lazy val escapeScheme = {
    val des = Utility.trim(node)
    val res = des match {
      case Elem(
            "dfdl",
            "defineEscapeScheme",
            _,
            _,
            Seq(e @ Elem("dfdl", "escapeScheme", _, _, _*))
          ) =>
        new DFDLEscapeScheme(e, decl, this)
      case _ => SDE("The content of %s is not complete.", des.label)
    }
    res
  }

  override def toString(): String = {
    "dfdl:defineEscapeScheme " + name
  }
}
