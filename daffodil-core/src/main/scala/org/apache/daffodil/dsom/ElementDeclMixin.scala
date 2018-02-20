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

package org.apache.daffodil.dsom

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.xml._
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.equality._
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
            case (None, None) => schemaDefinitionError("No type definition found for '%s'.", qn.toPrettyString)
            case (Some(_), Some(_)) => schemaDefinitionError("Both a simple and a complex type definition found for '%s'", qn.toPrettyString)
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
