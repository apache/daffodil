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

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => Some(new DFDLElement(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
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

  final def isSimpleType: Boolean = optSimpleType.isDefined

  final def isComplexType = !isSimpleType

  final def primType = optSimpleType.get.primType

  final def hasDefaultValue: Boolean = defaultAttr.isDefined

  final def simpleType: SimpleTypeBase = optSimpleType.get

  final def complexType: ComplexTypeBase = optComplexType.get

  /**
   * Convenience methods for unit testing purposes.
   */
  final def sequence = complexType.sequence
  final def choice = complexType.choice

  final override lazy val optReferredToComponent = typeDef match {
    case std: SimpleTypeDefBase => Some(std)
    case ctd: ComplexTypeBase => None // in DFDL v1.0 complex types are not annotated, so can't carry properties nor statements.
    case _ => None
  }

  final lazy val optNamedComplexType: Option[GlobalComplexTypeDef] = {
    namedTypeQName.flatMap { qn =>
      val res = schemaSet.getGlobalComplexTypeDef(qn)
      res
    }
  }

  final lazy val optImmediateComplexType: Option[ComplexTypeBase] = LV('optImmediateComplexType) {
    val ct = xml \ "complexType"
    val nt = typeName
    if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), this))
    else {
      Assert.invariant(nt != "")
      None
    }
  }.value

  final lazy val optComplexType =
    optNamedComplexType.orElse(optImmediateComplexType.collect { case ct: ComplexTypeBase => ct })

  final lazy val namedType: Option[TypeBase] = LV('namedTypeDef) {
    val res = optNamedSimpleType.orElse(optNamedComplexType).orElse {
      namedTypeQName.map { qn => SDE("No type definition found for '%s'.", qn.toPrettyString) }
    }
    if (optNamedSimpleType.isDefined &&
      optNamedComplexType.isDefined)
      SDE("Both a simple type and a complex type definition found for %s.", namedTypeQName.get.toPrettyString)
    res
  }.value

  final lazy val immediateType = optImmediateSimpleType.orElse(optImmediateComplexType)

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

  final lazy val optImmediateSimpleType: Option[SimpleTypeBase] = LV('optImmediateSimpleType) {
    val st = xml \ "simpleType"
    if (st.length == 1) {
      val lstd = new LocalSimpleTypeDef(st(0), this)
      Some(lstd)
    } else None
  }.value

  final lazy val typeName = getAttributeOption("type")

  final lazy val namedTypeQName: Option[RefQName] = {
    typeName.map { tname =>
      QName.resolveRef(tname, namespaces, tunable.unqualifiedPathStepPolicy).get
    }
  }

  final lazy val optNamedSimpleType: Option[SimpleTypeBase] = {
    namedTypeQName.flatMap { qn =>
      schemaSet.getPrimitiveType(qn).orElse(schemaSet.getGlobalSimpleTypeDef(qn))
    }
  }

  final lazy val optSimpleType =
    optNamedSimpleType.orElse(optImmediateSimpleType.collect { case st: SimpleTypeBase => st })

  final lazy val defaultAttr = xml.attribute("default")

  final lazy val defaultValueAsString = {
    Assert.usage(hasDefaultValue)
    val dv = defaultAttr.get.text
    schemaDefinitionWhen(
      dv =:= "" && !(primType =:= PrimType.String),
      "Type was %s, but only type xs:string can have XSD default=\"\".",
      primType.toString)
    dv
  }

  final lazy val isNillable = (xml \ "@nillable").text == "true"
}

