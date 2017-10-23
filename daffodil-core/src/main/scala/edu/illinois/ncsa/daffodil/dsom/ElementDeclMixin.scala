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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.equality._
import scala.xml.Node

trait ElementLikeMixin
  extends AnnotatedSchemaComponent
  with ProvidesDFDLStatementMixin {

  private lazy val elementBase = this match {
    case ged: GlobalElementDecl => ged.elementRef
    case eb: ElementBase => eb
  }

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => Some(new DFDLElement(node, this))
      case _ => elementBase.annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final lazy val emptyFormatFactory = new DFDLElement(newDFDLAnnotationXML("element"), this)

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLElement]
}
/**
 * Shared by all element declarations local or global
 */
trait ElementDeclMixin
  extends ElementLikeMixin {

  def namedQName: NamedQName

  final override protected def optReferredToComponent = typeDef match {
    case std: SimpleTypeDefBase => Some(std)
    case ctd: ComplexTypeBase => None // in DFDL v1.0 complex types are not annotated, so can't carry properties nor statements.
    case _ => None
  }

  final def immediateType: Option[TypeBase] = LV('immediateType) {
    val st = xml \ "simpleType"
    val ct = xml \ "complexType"
    val nt = typeName
    if (st.length == 1) {
      val lstd = new LocalSimpleTypeDef(st(0), this)
      Some(lstd)
    } else if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), this))
    else {
      Assert.invariant(nt != "")
      None
    }
  }.value

  private lazy val typeName = getAttributeOption("type")

  private def namedTypeQName: Option[RefQName] = LV('namedTypeQName) {
    typeName match {
      case Some(tname) =>
        Some(QName.resolveRef(tname, namespaces, tunable).get)
      case None => None
    }
  }.value

  private def namedType: Option[TypeBase] = LV('namedTypeDef) {
    namedTypeQName match {
      case None => None
      case Some(qn) => {

        val ss = schemaSet
        val optPrimitiveType = ss.getPrimitiveType(qn)
        if (optPrimitiveType.isDefined) optPrimitiveType
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
  }.value

  final lazy val typeDef: TypeBase = LV('typeDef) {
    (immediateType, namedType) match {
      case (Some(ty), None) => ty
      case (None, Some(ty)) => ty
      // Note: Schema validation should find this for us, but referential integrity checks like this
      // might not be done, so we check explicitly for this.
      case (Some(ity), Some(nty)) => SDE("Must have one of an immediate type or a named type (%s) but not both", namedTypeQName.get.toPrettyString)
      case (None, None) => SDE("Must have an immediate type, or a named type, but neither was found.")
    }
  }.value

  final lazy val isSimpleType = LV('isSimpleType) {
    typeDef match {
      case _: SimpleTypeBase => true
      case _: ComplexTypeBase => false
      case _ => Assert.invariantFailed("Must be either SimpleType or ComplexType")
    }
  }.value

  final def isComplexType = !isSimpleType

  private def defaultAttr = xml.attribute("default")

  final lazy val defaultValueAsString = {
    Assert.usage(hasDefaultValue)
    val dv = defaultAttr.get.text
    schemaDefinitionWhen(dv =:= "" && !(primType =:= PrimType.String), "Type was %s, but only type xs:string can have XSD default=\"\".",
      primType.toString)
    dv
  }

  // shorthand
  final lazy val primType = {
    val res = typeDef.asInstanceOf[SimpleTypeBase].primType
    res
  }

  final lazy val hasDefaultValue: Boolean = {
    Assert.usage(isSimpleType)
    defaultAttr.isDefined
  }

  final lazy val isNillable = (xml \ "@nillable").text == "true"

  final def simpleType = typeDef match {
    case st: SimpleTypeBase => st
    case ct: ComplexTypeBase =>
      Assert.invariantFailed("Must be simple type: " + namedQName)
  }

  final def complexType = typeDef.asInstanceOf[ComplexTypeBase]
  /**
   * Convenience methods for unit testing purposes.
   */
  final def sequence = complexType.sequence
  final def choice = complexType.choice

}
