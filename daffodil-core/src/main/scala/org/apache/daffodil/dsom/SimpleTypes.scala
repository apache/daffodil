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

import scala.xml.Node
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.processors.SimpleTypeRuntimeData
import org.apache.daffodil.util.Misc

trait TypeBase {
  def optRestriction: Option[Restriction] = None
  def optUnion: Option[Union] = None
  def typeNode: NodeInfo.AnyType.Kind

}

trait NonPrimTypeMixin {
  def elementDecl: ElementDeclMixin

  def elementBase: ElementBase = elementDecl match {
    case eb: ElementBase => eb
    case ged: GlobalElementDecl => ged.elementRef
  }
}

sealed trait SimpleTypeBase extends TypeBase {
  def primType: NodeInfo.PrimType = {
    optRestriction.map { _.primType }.getOrElse {
      optUnion.map { _.primType }.getOrElse {
        Assert.invariantFailed("must be either a restriction or union")
      }
    }
  }
}

/**
 * PrimType nodes are part of the runtime. For compilation, we need a notion
 * of primitive type that derives from the same base a SimpleTypeBase and
 * ComplexTypeBase, and it needs to have methods that take and return
 * compiler-only object types; hence we can't define a base in the runtime
 * because it can't have those methods; hence, can't achieve the
 * polymorphism over all sorts of types.
 *
 * So for the compiler, a PrimitiveType is just a wrapper around a PrimType object.
 */
class PrimitiveType private (tn: PrimType)
  extends SimpleTypeBase {
  override def optRestriction = None
  override def optUnion = None
  override def primType = tn
  override def typeNode = tn
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
      case _ => Assert.usageError("Not a primitive type node: " + typeNode)
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

}

abstract class SimpleTypeDefBase(xml: Node, parent: SchemaComponent)
  extends AnnotatedSchemaComponentImpl(xml, parent)
  with SimpleTypeBase
  with NonPrimTypeMixin
  with ProvidesDFDLStatementMixin
  with OverlapCheckMixin {

  requiredEvaluations(if (elementDecl.isSimpleType) simpleTypeRuntimeData.preSerialization)

  override def typeNode = primType

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
    else restrictions.tail.map { _.simpleType }

  override final def optReferredToComponent = optRestriction.flatMap { _.optBaseDef }

  protected final def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => Some(new DFDLSimpleType(node, this))
      case _ => annotationFactoryForDFDLStatement(node, elementBase)
    }
  }

  final override lazy val (optRestriction, optUnion) = {
    val restrictionNodeSeq = xml \ "restriction"
    if (restrictionNodeSeq.isEmpty) {
      val unionNodeSeq = xml \ "union"
      Assert.invariant(unionNodeSeq.length == 1)
      (None, Some(new Union(unionNodeSeq(0), this)))
    } else {
      (Some(new Restriction(restrictionNodeSeq(0), this)), None)
    }
  }

  def toOpt[R <: AnyRef](b: Boolean, v: => R) = Misc.boolToOpt(b, v)

  lazy val simpleTypeRuntimeData: SimpleTypeRuntimeData = {
    new SimpleTypeRuntimeData(
      variableMap,
      schemaFileLocation,
      diagnosticDebugName,
      path,
      namespaces,
      primType,
      noFacetChecks,
      //
      // TODO: Cleanup code: really these should all have been option types
      // not these pairs of a boolean, and a value that can only be evaluated
      // if the boolean is true. Options types are preferable for these as
      // they can be manipulated more easily with flatmap/orElse, etc.
      //
      optRestriction.toSeq.flatMap { r => if (r.hasPattern) r.patternValues else Nil },
      optRestriction.flatMap { r => toOpt(r.hasEnumeration, r.enumerationValues.get) },
      optRestriction.flatMap { r => toOpt(r.hasMinLength, r.minLengthValue) },
      optRestriction.flatMap { r => toOpt(r.hasMaxLength, r.maxLengthValue) },
      optRestriction.flatMap { r => toOpt(r.hasMinInclusive, r.minInclusiveValue) },
      optRestriction.flatMap { r => toOpt(r.hasMaxInclusive, r.maxInclusiveValue) },
      optRestriction.flatMap { r => toOpt(r.hasMinExclusive, r.minExclusiveValue) },
      optRestriction.flatMap { r => toOpt(r.hasMaxExclusive, r.maxExclusiveValue) },
      optRestriction.flatMap { r => toOpt(r.hasTotalDigits, r.totalDigitsValue) },
      optRestriction.flatMap { r => toOpt(r.hasFractionDigits, r.fractionDigitsValue) },
      optUnion.orElse(optRestriction.flatMap { _.optUnion }).toSeq.flatMap { _.unionMemberTypes.map { _.simpleTypeRuntimeData } },
      tunable)
  }

  private lazy val noFacetChecks =
    optRestriction.map { r =>
      if (r.hasPattern || r.hasEnumeration || r.hasMinLength || r.hasMaxLength ||
        r.hasMinInclusive || r.hasMaxInclusive || r.hasMinExclusive || r.hasMaxExclusive ||
        r.hasTotalDigits || r.hasFractionDigits) false
      else true
    }.getOrElse(true)
}

final class LocalSimpleTypeDef(
  xmlArg: Node, elementArg: ElementDeclMixin)
  extends SimpleTypeDefBase(xmlArg, elementArg)
  with LocalNonElementComponentMixin
  with NestingLexicalMixin {

  override lazy val elementDecl = elementArg

  /**
   * For anonymous simple type def, uses the base name, or primitive type name
   */
  override lazy val diagnosticDebugName: String = {
    //
    // TODO: implement a daf:name property to give an alternate name. If present, use that.
    //
    val rName = optRestriction.flatMap { r =>
      r.optBaseDef.map { _.namedQName }.orElse(Some(r.primType.globalQName))
    }.getOrElse(this.optUnion.map { u => u.primType.globalQName }.getOrElse(
      Assert.invariantFailed("Simple type is neither a union nor a restriction: " + this)))
    rName.toQNameString
  }

}

/**
 * The factory is sharable even though the global object it creates cannot
 * be shared.
 *
 * Call forElement(element) and supply the element referring
 * to the global type, then you get back an instance that is one-to-one with the
 * element.
 *
 * This then allows attributes of the type to refer to the element in deciding things.
 * I.e., the context is clear and kept separate for each place a global type is used.
 */

final class GlobalSimpleTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponentFactory(xmlArg, schemaDocumentArg)
  with GlobalNonElementComponentMixin {

  /**
   * Create a private instance for this element's use.
   */
  def forElement(elementDecl: ElementDeclMixin) = new GlobalSimpleTypeDef(None, this, Some(elementDecl))
  def forDerivedType(derivedType: SimpleTypeDefBase) = new GlobalSimpleTypeDef(Some(derivedType), this, None)

}
/**
 * The instance type for global simple type definitions.
 */

final class GlobalSimpleTypeDef(
  derivedType: Option[SimpleTypeDefBase],
  val factory: GlobalSimpleTypeDefFactory,
  val referringElement: Option[ElementDeclMixin])
  extends SimpleTypeDefBase(factory.xml, factory.schemaDocument)
  with GlobalNonElementComponentMixin
  with NestingTraversesToReferenceMixin {

  // override def term = element

  override lazy val referringComponent: Option[SchemaComponent] =
    (derivedType, referringElement) match {
      case (Some(dt), None) => derivedType
      case (None, Some(elem)) => referringElement
      case _ => Assert.impossible("SimpleType must either have a derivedType or an element. Not both.")
    }

  override lazy val elementDecl: ElementDeclMixin = referringComponent match {
    case Some(dt: SimpleTypeDefBase) => dt.elementDecl
    case Some(e: ElementDeclMixin) => e
    case _ => Assert.invariantFailed("unexpected referringComponent")
  }

}

