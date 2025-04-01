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

import org.apache.daffodil.core.dsom.walker.SimpleTypeView
import org.apache.daffodil.core.runtime1.SimpleTypeRuntime1Mixin
import org.apache.daffodil.lib.cookers.RepValueCooker
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive

trait TypeBase {
  def optRestriction: Option[Restriction] = None
  def optUnion: Option[Union] = None
  def typeNode: NodeInfo.AnyType.Kind
}

trait NonPrimTypeMixin

sealed trait SimpleTypeBase extends TypeBase with SimpleTypeView {

  override def primType: PrimType
}

/**
 * PrimType nodes are part of the runtime. For compilation, we need a notion
 * of primitive type that derives from the same base as SimpleTypeBase and
 * ComplexTypeBase, and it needs to have methods that take and return
 * compiler-only object types; hence we can't define a base in the runtime
 * because it can't have those methods; hence, can't achieve the
 * polymorphism over all sorts of types.
 *
 * So for the compiler, a PrimitiveType is just a wrapper around a PrimType object.
 */
final class PrimitiveType private (tn: PrimType) extends SimpleTypeBase with NamedMixin {
  override def optRestriction = None
  override def optUnion = None
  override def primType = tn
  override def typeNode = tn

  override def name = diagnosticDebugName

  override def namedQName = typeNode.globalQName
  override def namespace = namedQName.namespace
  override def prefix = namedQName.prefix.get
  override def xml = Assert.invariantFailed("Primitive types do not have XML")
  override def schemaDocument =
    Assert.invariantFailed("Primitive types do not have schemaDocument")

  /*
   * These methods don't really make sense here, but are needed by NamedMixin
   */
  override def SDE(id: String, args: Any*) =
    Assert.invariantFailed("Primitive types shouldn't ever have an SDE")
  override def schemaFileLocation =
    Assert.invariantFailed("Primitive types don't have a schemaFileLocation")

  override def toString = namedQName.toQNameString
}

object PrimitiveType {

  def apply(typeNode: PrimType) = {
    typeNode match {
      case PrimType.String => String
      case PrimType.Int => Int
      case PrimType.Byte => Byte
      case PrimType.Short => Short
      case PrimType.Long => Long
      case PrimType.Integer => Integer
      case PrimType.Decimal => Decimal
      case PrimType.UnsignedInt => UnsignedInt
      case PrimType.UnsignedByte => UnsignedByte
      case PrimType.UnsignedShort => UnsignedShort
      case PrimType.UnsignedLong => UnsignedLong
      case PrimType.NonNegativeInteger => NonNegativeInteger
      case PrimType.Double => Double
      case PrimType.Float => Float
      case PrimType.HexBinary => HexBinary
      case PrimType.Boolean => Boolean
      case PrimType.DateTime => DateTime
      case PrimType.Date => Date
      case PrimType.Time => Time
      case PrimType.AnyURI => AnyURI
    }
  }

  val String = new PrimitiveType(PrimType.String)
  val Int = new PrimitiveType(PrimType.Int)
  val Byte = new PrimitiveType(PrimType.Byte)
  val Short = new PrimitiveType(PrimType.Short)
  val Long = new PrimitiveType(PrimType.Long)
  val Integer = new PrimitiveType(PrimType.Integer)
  val Decimal = new PrimitiveType(PrimType.Decimal)
  val UnsignedInt = new PrimitiveType(PrimType.UnsignedInt)
  val UnsignedByte = new PrimitiveType(PrimType.UnsignedByte)
  val UnsignedShort = new PrimitiveType(PrimType.UnsignedShort)
  val UnsignedLong = new PrimitiveType(PrimType.UnsignedLong)
  val NonNegativeInteger = new PrimitiveType(PrimType.NonNegativeInteger)
  val Double = new PrimitiveType(PrimType.Double)
  val Float = new PrimitiveType(PrimType.Float)
  val HexBinary = new PrimitiveType(PrimType.HexBinary)
  val Boolean = new PrimitiveType(PrimType.Boolean)
  val DateTime = new PrimitiveType(PrimType.DateTime)
  val Date = new PrimitiveType(PrimType.Date)
  val Time = new PrimitiveType(PrimType.Time)
  val AnyURI = new PrimitiveType(PrimType.AnyURI)

}

abstract class SimpleTypeDefBase(xml: Node, lexicalParent: SchemaComponent)
  extends AnnotatedSchemaComponentImpl(xml, lexicalParent)
  with SimpleTypeBase
  with NonPrimTypeMixin
  with ProvidesDFDLStatementMixin
  with OverlapCheckMixin
  with NamedMixin
  with SimpleTypeRuntime1Mixin {

  override def typeNode = primType

  def toOpt[R <: AnyRef](b: Boolean, v: => R): Option[R] = Misc.boolToOpt(b, v)

  lazy val noFacetChecks =
    optRestriction
      .map { r =>
        if (
          r.hasPattern || r.hasEnumeration || r.hasLength || r.hasMinLength || r.hasMaxLength ||
          r.hasMinInclusive || r.hasMaxInclusive || r.hasMinExclusive || r.hasMaxExclusive ||
          r.hasTotalDigits || r.hasFractionDigits
        ) false
        else true
      }
      .getOrElse(true)

  // override def name = diagnosticDebugName // don't do this. names are used by diagnosticDebugName

  override final lazy val optReferredToComponent = optRestriction.flatMap { _.optBaseTypeDef }
  override final lazy val emptyFormatFactory =
    new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  override final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  override final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case Elem("dfdl", "simpleType", _, _, _*) => Some(new DFDLSimpleType(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  lazy val primType: NodeInfo.PrimType = {
    optRestriction.map { _.primType }.getOrElse {
      optUnion.map { _.primType }.getOrElse {
        Assert.invariantFailed("must be either a restriction or union")
      }
    }
  }

  final lazy val restrictions = {
    val thisR = optRestriction.toSeq
    val res = thisR ++
      thisR.flatMap { _.derivationBaseRestrictions }
    res
  }

  /**
   * Exclusive of self.
   */
  final lazy val bases: Seq[SimpleTypeDefBase] =
    if (restrictions.isEmpty) Nil
    else restrictions.tail.map { _.simpleTypeDef }

  override lazy val (optRestriction, optUnion) = {
    val restrictionNodeSeq = xml \ "restriction"
    if (restrictionNodeSeq.isEmpty) {
      val unionNodeSeq = xml \ "union"
      Assert.invariant(unionNodeSeq.length == 1)
      (None, Some(Union(unionNodeSeq(0), this)))
    } else {
      (Some(Restriction(restrictionNodeSeq(0), this)), None)
    }
  }

}

object LocalSimpleTypeDef {
  def apply(xmlArg: Node, lexicalParent: SchemaComponent) = {
    val lstd = new LocalSimpleTypeDef(xmlArg, lexicalParent)
    lstd.initialize()
    lstd
  }
}

final class LocalSimpleTypeDef private (xmlArg: Node, lexicalParent: SchemaComponent)
  extends SimpleTypeDefBase(xmlArg, lexicalParent)
  with LocalNonElementComponentMixin
  with NestingLexicalMixin {

  requiredEvaluationsIfActivated(primType)

  /**
   * For anonymous simple type def, uses the base name, or primitive type name
   */
  override protected lazy val diagnosticDebugNameImpl: String = {
    if (optRestriction.isDefined)
      optRestriction.get.baseQNameString // unresolved string
    else {
      Assert.invariant(optUnion.isDefined)
      optUnion.get.primType.globalQName.toQNameString
    }
  }
}

object GlobalSimpleTypeDef {
  def apply(xmlArg: Node, schemaDocumentArg: SchemaDocument) = {
    val gstd = new GlobalSimpleTypeDef(xmlArg, schemaDocumentArg)
    gstd.initialize()
    gstd
  }
}

final class GlobalSimpleTypeDef private (xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SimpleTypeDefBase(xmlArg, schemaDocumentArg)
  with GlobalNonElementComponentMixin
  with NestingLexicalMixin
  with NamedMixin {

  override lazy val name = super[NamedMixin].name
}

/*
 * This isn't really a Factory class, as it already knows everything about the enumeration,
 * and we never actually use it to construct a concrete EnumerationDef object.
 *
 * It is, however, a "Factory" in the sense that DSOM makes a factory/object distinction,
 * where a factory holds all the information stored on the node itself, while the object
 * computes additional information based on the context in the schema where it is being used.
 * In this sense, all usages of EnumerationDefs are using them as a "factory".
 */
final class EnumerationDef(xml: Node, parentType: SimpleTypeDefBase)
  extends SchemaComponentImpl(xml, parentType.schemaDocument)
  with AnnotatedSchemaComponent
  with NestingLexicalMixin {

  Assert.invariant(xml.label == "enumeration")

  lazy val enumValueRaw: String = (xml \ "@value").head.text
  lazy val enumValueCooked: DataValuePrimitive =
    try {
      parentType.primType.fromXMLString(enumValueRaw)
    } catch {
      case e: InvalidPrimitiveDataException =>
        SDE("Invalid data for enumeration: %s", e.getMessage)
    }

  lazy val repValuesRaw: Seq[String] = {
    val optNodes = xml.attribute(XMLUtils.DFDLX_NAMESPACE, "repValues")
    val res = optNodes.map(_.toSeq).getOrElse(Seq.empty).flatMap { node =>
      RepValueCooker.convertConstant(node.text, this, false)
    }
    res
  }

  lazy val repValueRangesRaw: Seq[String] = {
    val optNodes = xml.attribute(XMLUtils.DFDLX_NAMESPACE, "repValueRanges")
    val res = optNodes.map(_.toSeq).getOrElse(Seq.empty).flatMap { node =>
      val ranges = RepValueCooker.convertConstant(node.text, this, false)
      if (ranges.length % 2 != 0) {
        SDE("dfdlx:repValueRanges must specify an even number of values")
      }
      ranges
    }
    res
  }

  override val optReferredToComponent = None

  protected def annotationFactory(node: Node): Option[DFDLAnnotation] =
    Assert.invariantFailed("Should not be called")
  protected lazy val emptyFormatFactory: DFDLFormatAnnotation =
    new DFDLEnumerationFactory(newDFDLAnnotationXML("enumeration"), this)
  protected def isMyFormatAnnotation(a: DFDLAnnotation): Boolean =
    Assert.invariantFailed("Should not be called")

}
