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
sealed trait ElementDeclMixin
  extends ElementLikeMixin {

  def namedQName: NamedQName

  def optReferredToComponent: Option[AnnotatedSchemaComponent]

  def optImmediateSimpleType: Option[SimpleTypeBase]

  def optImmediateComplexType: Option[ComplexTypeBase]

  def immediateType: Option[TypeBase]

  def typeName: Option[String]

  def optNamedSimpleType: Option[SimpleTypeBase]

  def optNamedComplexType: Option[GlobalComplexTypeDef]

  def optSimpleType: Option[SimpleTypeBase]

  def optComplexType: Option[ComplexTypeBase]

  def namedTypeQName: Option[RefQName]

  def namedType: Option[TypeBase]

  def typeDef: TypeBase

  final def isSimpleType: Boolean = optSimpleType.isDefined

  final def isComplexType = !isSimpleType

  def defaultAttr: Option[Seq[Node]]

  def defaultValueAsString: String

  final def primType = optSimpleType.get.primType

  final def hasDefaultValue: Boolean = defaultAttr.isDefined

  def isNillable: Boolean

  final def simpleType: SimpleTypeBase = optSimpleType.get

  final def complexType: ComplexTypeBase = optComplexType.get

  /**
   * Convenience methods for unit testing purposes.
   */
  final def sequence = complexType.sequence
  final def choice = complexType.choice
}

sealed trait ElementDeclInstanceImplMixin
  extends ElementDeclMixin {

  final override lazy val optReferredToComponent = typeDef match {
    case std: SimpleTypeDefBase => Some(std)
    case ctd: ComplexTypeBase => None // in DFDL v1.0 complex types are not annotated, so can't carry properties nor statements.
    case _ => None
  }

  final override lazy val optNamedComplexType: Option[GlobalComplexTypeDef] = {
    namedTypeQName.flatMap { qn =>
      val gctdFactory = schemaSet.getGlobalComplexTypeDef(qn)
      val res = gctdFactory.map { gctdf => gctdf.forElement(this) }
      res
    }
  }

  final override lazy val optImmediateComplexType: Option[ComplexTypeBase] = LV('optImmediateComplexType) {
    val ct = xml \ "complexType"
    val nt = typeName
    if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), this))
    else {
      Assert.invariant(nt != "")
      None
    }
  }.value

  final override lazy val optComplexType =
    optNamedComplexType.orElse(optImmediateComplexType.collect { case ct: ComplexTypeBase => ct })

  final override lazy val namedType: Option[TypeBase] = LV('namedTypeDef) {
    val res = optNamedSimpleType.orElse(optNamedComplexType).orElse {
      namedTypeQName.map { qn => SDE("No type definition found for '%s'.", qn.toPrettyString) }
    }
    if (optNamedSimpleType.isDefined &&
      optNamedComplexType.isDefined)
      SDE("Both a simple type and a complex type definition found for %s.", namedTypeQName.get.toPrettyString)
    res
  }.value

  final override lazy val immediateType = optImmediateSimpleType.orElse(optImmediateComplexType)

  final override lazy val typeDef: TypeBase = LV('typeDef) {
    (immediateType, namedType) match {
      case (Some(ty), None) => ty
      case (None, Some(ty)) => ty
      // Note: Schema validation should find this for us, but referential integrity checks like this
      // might not be done, so we check explicitly for this.
      case (Some(ity), Some(nty)) => SDE("Must have one of an immediate type or a named type (%s) but not both", namedTypeQName.get.toPrettyString)
      case (None, None) => SDE("Must have an immediate type, or a named type, but neither was found.")
    }
  }.value

}

trait ElementDeclFactoryImplMixin
  extends ElementDeclMixin {

  final override lazy val optImmediateSimpleType: Option[SimpleTypeBase] = LV('optImmediateSimpleType) {
    val st = xml \ "simpleType"
    if (st.length == 1) {
      val lstd = new LocalSimpleTypeDef(st(0), this)
      Some(lstd)
    } else None
  }.value

  final override lazy val typeName = getAttributeOption("type")

  final override lazy val namedTypeQName: Option[RefQName] = {
    typeName.map { tname =>
      QName.resolveRef(tname, namespaces, tunable).get
    }
  }

  final lazy val optNamedSimpleType: Option[SimpleTypeBase] = {
    namedTypeQName.flatMap { qn =>
      schemaSet.getPrimitiveType(qn).orElse(schemaSet.getGlobalSimpleTypeDef(qn))
    }
  }

  final override lazy val optSimpleType =
    optNamedSimpleType.orElse(optImmediateSimpleType.collect { case st: SimpleTypeBase => st })

  final override lazy val defaultAttr = xml.attribute("default")

  final override lazy val defaultValueAsString = {
    Assert.usage(hasDefaultValue)
    val dv = defaultAttr.get.text
    schemaDefinitionWhen(
      dv =:= "" && !(primType =:= PrimType.String),
      "Type was %s, but only type xs:string can have XSD default=\"\".",
      primType.toString)
    dv
  }

  final override lazy val isNillable = (xml \ "@nillable").text == "true"

  override def namedType: Option[TypeBase] = Assert.usageError("Not to be called on Element Decl Factories")
  override def optComplexType: Option[ComplexTypeBase] = Assert.usageError("Not to be called on Element Decl Factories")
  override def optNamedComplexType: Option[GlobalComplexTypeDef] = Assert.usageError("Not to be called on Element Decl Factories")
  override def typeDef: TypeBase = Assert.usageError("Not to be called on Element Decl Factories")
  override def immediateType: Option[TypeBase] = Assert.usageError("Not to be called on Element Decl Factories")
  override def optImmediateComplexType: Option[ComplexTypeBase] = Assert.usageError("Not to be called on Element Decl Factories")
}

trait ElementDeclNonFactoryDelegatingMixin
  extends ElementDeclFactoryImplMixin
  with ElementDeclInstanceImplMixin

trait ElementDeclFactoryDelegatingMixin
  extends ElementDeclInstanceImplMixin {

  protected def delegate: ElementDeclFactoryImplMixin

  final override def typeName: Option[String] = delegate.typeName

  final override def namedTypeQName: Option[RefQName] = delegate.namedTypeQName

  final override def optImmediateSimpleType: Option[SimpleTypeBase] = delegate.optImmediateSimpleType

  final override def optNamedSimpleType: Option[SimpleTypeBase] = delegate.optNamedSimpleType

  final override def optSimpleType: Option[SimpleTypeBase] = delegate.optSimpleType

  final override def defaultAttr: Option[Seq[Node]] = delegate.defaultAttr

  final override def defaultValueAsString: String = delegate.defaultValueAsString

  final override def isNillable: Boolean = delegate.isNillable

}
