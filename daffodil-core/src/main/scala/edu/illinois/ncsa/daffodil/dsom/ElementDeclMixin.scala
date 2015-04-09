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
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.grammar.ElementDeclGrammarMixin

/**
 * Shared by all element declarations local or global
 */
trait ElementDeclMixin
  extends ElementDeclGrammarMixin
  with OverlapCheckMixin { self: ElementBase =>

  private lazy val eRefNonDefault: Option[ChainPropProvider] = eRefNonDefault_.value
  private val eRefNonDefault_ = LV('eRefNonDefault) {
    elementRef.map {
      _.nonDefaultFormatChain
    }
  }

  private lazy val eRefDefault: Option[ChainPropProvider] = eRefDefault_.value
  private val eRefDefault_ = LV('eRefDefault) {
    elementRef.map {
      _.defaultFormatChain
    }
  }

  private lazy val sTypeNonDefault: Seq[ChainPropProvider] = self.typeDef match {
    case st: SimpleTypeDefBase => st.nonDefaultPropertySources
    case _ => Seq()
  }
  private lazy val sTypeDefault: Seq[ChainPropProvider] = self.typeDef match {
    case st: SimpleTypeDefBase => st.defaultPropertySources
    case _ => Seq()
  }

  /**
   * This and the partner defaultPropertySources are what ElementBase reaches back to get from
   * the ElementRef in order to have the complete picture of all the properties in effect for
   * that ElementBase.
   */
  final lazy val nonDefaultPropertySources = nonDefaultPropertySources_.value
  private val nonDefaultPropertySources_ = LV('nonDefaultPropertySources) {
    val seq = (eRefNonDefault.toSeq ++ Seq(this.nonDefaultFormatChain) ++ sTypeNonDefault).distinct
    checkNonOverlap(seq)
    seq
  }

  final lazy val defaultPropertySources = defaultPropertySources_.value
  private val defaultPropertySources_ = LV('defaultPropertySources) {
    val seq = (eRefDefault.toSeq ++ Seq(this.defaultFormatChain) ++ sTypeDefault).distinct
    seq
  }

  override lazy val prettyName = "element." + name

  final lazy val immediateType = immediateType_.value
  private val immediateType_ = LV('immediateType) {
    val st = xml \ "simpleType"
    val ct = xml \ "complexType"
    val nt = typeName
    if (st.length == 1)
      Some(new LocalSimpleTypeDef(st(0), self))
    else if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), self))
    else {
      Assert.invariant(nt != "")
      None
    }
  }

  private lazy val typeName = getAttributeOption("type")

  private lazy val namedTypeQName: Option[RefQName] = namedTypeQName_.value
  private val namedTypeQName_ = LV('namedTypeQName) {
    typeName match {
      case Some(tname) =>
        Some(QName.resolveRef(tname, namespaces).get)
      case None => None
    }
  }

  private lazy val namedTypeDef = namedTypeDef_.value
  private val namedTypeDef_ = LV('namedTypeDef) {
    namedTypeQName match {
      case None => None
      case Some(qn) => {

        val ss = schemaSet
        val prim = ss.getPrimType(qn)
        //
        if (prim != None) prim
        else {
          val gstd = ss.getGlobalSimpleTypeDef(qn)
          val gctd = ss.getGlobalComplexTypeDef(qn)
          val res = (gstd, gctd) match {
            case (Some(gstdFactory), None) => Some(gstdFactory.forElement(this))
            case (None, Some(gctdFactory)) => Some(gctdFactory.forElement(this))
            // Note: Validation of the DFDL Schema doesn't necessarily check referential integrity
            // or other complex constraints like conflicting names.
            // So we check it here explicitly.
            case (None, None) => schemaDefinitionError("No type definition found for '%s'.", namedTypeQName)
            case (Some(_), Some(_)) => schemaDefinitionError("Both a simple and a complex type definition found for '%s'", namedTypeQName)
          }
          res
        }
      }
    }
  }

  final lazy val typeDef = typeDef_.value
  private val typeDef_ = LV('typeDef) {
    (immediateType, namedTypeDef) match {
      case (Some(ty), None) => ty
      case (None, Some(ty)) => ty
      // Note: Schema validation should find this for us, but referential integrity checks like this
      // might not be done, so we check explicitly for this.
      case _ => SDE("Must have one of an immediate type or a named type but not both")
    }
  }

  final lazy val isSimpleType = isSimpleType_.value
  private val isSimpleType_ = LV('isSimpleType) {
    typeDef match {
      case _: SimpleTypeBase => true
      case _: ComplexTypeBase => false
      case _ => Assert.invariantFailed("Must be either SimpleType or ComplexType")
    }
  }

  final lazy val isComplexType = !isSimpleType

  final lazy val defaultValueAsString = (xml \ "@default").text

  final lazy val hasDefaultValue: Boolean = defaultValueAsString != ""

  final lazy val isNillable = (xml \ "@nillable").text == "true"

  final lazy val elementComplexType = {
    Assert.usage(isComplexType)
    typeDef.asInstanceOf[ComplexTypeBase]
  }

  final lazy val elementSimpleType = {
    Assert.usage(isSimpleType)
    typeDef.asInstanceOf[SimpleTypeBase]
  }

  /**
   * We require that there be a concept of empty if we're going to be able to default something
   * and we are going to require that we can tell this statically. I.e., we're not going to defer this to runtime
   * just in case the delimiters are being determined at runtime.
   *
   * That is to say, if a delimiter is an expression, then we're assuming that means
   * at runtime it will not evaluate to empty string (so you can specify the delimiter
   * at runtime, but you cannot turn on/off the whole delimited format at runtime.)
   */
  final lazy val isDefaultable = isDefaultable_.value
  private val isDefaultable_ = LV('isDefaultable) {
    if (isSimpleType) {
      val hasDefaultValue = defaultValueAsString match {
        case "" => false // allowed for type string.
        case _ if (emptyIsAnObservableConcept) => true
        case _ => false
      }
      hasDefaultValue &&
        !isOptional &&
        (isScalar || isRequiredArrayElement)
    } else {
      // TODO: Implement complex element defaulting
      // JIRA issue DFDL-1277
      //
      // a complex element is defaultable
      // recursively if everything in it is defaultable
      // and everything in it has no required representation 
      // (e.g., no required delimiters, no alignment, no skip, etc.)
      // furthermore, even the defaultable things inside must satisfy 
      // a stricter criterion. They must have emptyValueDelimiterPolicy='none'
      // if delimiters are defined. 
      false
    }
  }
}

