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

import org.apache.daffodil.core.dsom.walker.ElementDeclView
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.xml._
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType

trait ElementLikeMixin extends AnnotatedSchemaComponent with ProvidesDFDLStatementMixin {

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case Elem("dfdl", "element", _, _, _*) =>
        Some(new DFDLElement(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final lazy val emptyFormatFactory =
    new DFDLElement(newDFDLAnnotationXML("element"), this)

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLElement]
}

/**
 * Shared by all element declarations local or global
 */
trait ElementDeclMixin extends ElementLikeMixin with ElementDeclView {

  override final def isSimpleType: Boolean = optSimpleType.isDefined

  override final def isComplexType = !isSimpleType

  final def primType = optSimpleType.get.primType

  final def hasDefaultValue: Boolean = defaultAttr.isDefined

  final def hasFixedValue: Boolean = fixedAttr.isDefined

  override final def simpleType: SimpleTypeBase = optSimpleType.get

  override final def complexType: ComplexTypeBase = optComplexType.get

  /**
   * Convenience methods for unit testing purposes.
   */
  final def sequence = complexType.sequence
  final def choice = complexType.choice

  final override lazy val optReferredToComponent = typeDef match {
    case std: SimpleTypeDefBase => Some(std)
    case ctd: ComplexTypeBase =>
      None // in DFDL v1.0 complex types are not annotated, so can't carry properties nor statements.
    case _ => None
  }

  final lazy val optNamedComplexType: Option[GlobalComplexTypeDef] = {
    namedTypeQName.flatMap { qn =>
      schemaSet.getGlobalComplexTypeDef(qn)
    }
  }

  final lazy val optImmediateComplexType: Option[ComplexTypeBase] =
    LV(Symbol("optImmediateComplexType")) {
      val ct = xml \ "complexType"
      val nt = typeName
      if (ct.length == 1)
        Some(new LocalComplexTypeDef(ct(0), this))
      else {
        nt.foreach { s => Assert.invariant(s != "") }
        None
      }
    }.value

  final lazy val optComplexType =
    optNamedComplexType.orElse(optImmediateComplexType.collect { case ct: ComplexTypeBase =>
      ct
    })

  final lazy val namedType: Option[TypeBase] = {
    val res = optNamedSimpleType.orElse(optNamedComplexType).orElse {
      namedTypeQName.map { qn => SDE("No type definition found for '%s'.", qn.toPrettyString) }
    }
    if (
      optNamedSimpleType.isDefined &&
      optNamedComplexType.isDefined
    )
      SDE(
        "Both a simple type and a complex type definition found for %s.",
        namedTypeQName.get.toPrettyString
      )
    res
  }

  final lazy val immediateType = optImmediateSimpleType.orElse(optImmediateComplexType)

  final lazy val typeDef: TypeBase = LV(Symbol("TypeBase")) {
    (immediateType, namedType) match {
      case (Some(ty), None) => ty
      case (None, Some(ty)) => ty
      // Note: Schema validation should find this for us, but referential integrity checks like this
      // might not be done, so we check explicitly for this.
      case (Some(ity), Some(nty)) =>
        SDE(
          "Must have one of an immediate type or a named type (%s) but not both",
          namedTypeQName.get.toPrettyString
        )
      case (None, None) =>
        SDE("Must have an immediate type, or a named type, but neither was found.")
    }
  }.value

  final lazy val optImmediateSimpleType: Option[LocalSimpleTypeDef] =
    LV(Symbol("optImmediateSimpleType")) {
      val st = xml \ "simpleType"
      if (st.length == 1) {
        val lstd = LocalSimpleTypeDef(st(0), this)
        Some(lstd)
      } else None
    }.value

  final lazy val typeName = getAttributeOption("type")

  final lazy val namedTypeQName: Option[RefQName] = typeName.map { resolveQName(_) }

  final lazy val optNamedSimpleType: Option[SimpleTypeBase] = {
    namedTypeQName.flatMap { qn =>
      PrimType.fromQName(qn).map { PrimitiveType(_) }.orElse {
        schemaSet.getGlobalSimpleTypeDef(qn)
      }
    }
  }

  final lazy val optSimpleType =
    optNamedSimpleType.orElse(optImmediateSimpleType.collect { case st: SimpleTypeBase => st })

  final lazy val defaultAttr = xml.attribute("default")

  final lazy val fixedAttr = xml.attribute("fixed")

  final lazy val defaultValueAsString = {
    Assert.usage(hasDefaultValue)
    val dv = defaultAttr.get.text
    schemaDefinitionWhen(
      dv =:= "" && !(primType =:= PrimType.String),
      "Type was %s, but only type xs:string can have XSD default=\"\".",
      primType.toString
    )
    dv
  }

  final lazy val fixedValueAsString = {
    Assert.usage(hasFixedValue)
    fixedAttr.get.text
  }

  final lazy val isNillable = (xml \ "@nillable").text == "true"
}
