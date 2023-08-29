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

import java.math.{ BigInteger => JBigInt }
import scala.xml.Node

import org.apache.daffodil.core.dsom.walker.SimpleTypeView
import org.apache.daffodil.core.runtime1.SimpleTypeRuntime1Mixin
import org.apache.daffodil.lib.cookers.IntRangeCooker
import org.apache.daffodil.lib.cookers.RepValueCooker
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.ParseUnparsePolicy
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.QName
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.processors.RangeBound
import org.apache.daffodil.runtime1.processors.RepValueSet
import org.apache.daffodil.runtime1.processors.RepValueSetCompiler
import org.apache.daffodil.runtime1.processors.TypeCalculator
import org.apache.daffodil.runtime1.processors.TypeCalculatorCompiler

trait TypeBase {
  def optRestriction: Option[Restriction] = None
  def optUnion: Option[Union] = None
  def typeNode: NodeInfo.AnyType.Kind
}

trait NonPrimTypeMixin

sealed trait SimpleTypeBase extends TypeBase with HasOptRepTypeMixin with SimpleTypeView {

  override def primType: PrimType
}

/*
 * For components which can define dfdlx:repValues and dfdlx:repValueRanges
 * Construct the repValueSet using only the above mentioned attributes on the element itself
 * Applies to simpleType and enumeration
 *
 * In the case of simpleType, it is possible that optRepValueSetFromAttribute will be none
 * but the element will still have an optRepValueSet for another source (eg. children elements)
 */
sealed trait HasRepValueAttributes
  extends AnnotatedSchemaComponent
  with ResolvesLocalProperties // for repValues, repValueRanges, repType
  {

  def optRepType: Option[SimpleTypeBase]
  def optRepValueSet: Option[RepValueSet]

  lazy val (
    repValuesAttrCooked: Seq[DataValuePrimitive],
    repValueRangesAttrCooked: Seq[(RangeBound, RangeBound)],
  ) =
    optRepType match {
      case Some(repType) => {
        val repValueSetRaw = findPropertyOption("repValues").toOption
          .map(_.split("\\s+").toSeq)
          .getOrElse(Seq())
        val repValueRangesRaw = findPropertyOption("repValueRanges").toOption.getOrElse("")
        repType.primType match {
          case PrimType.String => {
            if (repValueRangesRaw.size > 0)
              SDE("repValueRanges set when using a string repType")
            val repValueSetCooked = repValueSetRaw
              .flatMap(RepValueCooker.convertConstant(_, this, false))
              .map(DataValue.toDataValue)
            (repValueSetCooked, Seq())
          }
          case _: NodeInfo.Integer.Kind => {
            val ans1 = repValueSetRaw.map(new JBigInt(_): DataValueBigInt)
            val ans2 = IntRangeCooker
              .convertConstant(repValueRangesRaw, this, false)
              .map({ case (lower, upper) =>
                (new RangeBound(lower, true), new RangeBound(upper, true))
              })
            (ans1, ans2)
          }
          case x => SDE("repType must be either String or Integer type")
        }
      }
      case None => (Seq(), Seq())
    }

  lazy val optRepValueSetFromAttribute: Option[RepValueSet] = optRepType.flatMap(repType => {

    val ans = RepValueSetCompiler.compile(
      repValuesAttrCooked,
      repValueRangesAttrCooked.asInstanceOf[Seq[(RangeBound, RangeBound)]],
    )
    if (ans.isEmpty) None else Some(ans)

  })
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
final class PrimitiveType private (tn: PrimType) extends SimpleTypeBase with NamedMixin {
  override def optRestriction = None
  override def optUnion = None
  override def optRepType = None
  override def optRepValueSet = None
  override def primType = tn
  override def typeNode = tn
  override def optRepTypeElement = None

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
  val AnyURI = new PrimitiveType(PrimType.AnyURI)

}

abstract class SimpleTypeDefBase(xml: Node, lexicalParent: SchemaComponent)
  extends AnnotatedSchemaComponentImpl(xml, lexicalParent)
  with SimpleTypeBase
  with NonPrimTypeMixin
  with ProvidesDFDLStatementMixin
  with OverlapCheckMixin
  with HasOptRepTypeMixinImpl
  with NamedMixin
  with HasRepValueAttributes
  with SimpleTypeRuntime1Mixin {

  requiredEvaluationsIfActivated(validateRepType)

  override def typeNode = primType

  def toOpt[R <: AnyRef](b: Boolean, v: => R): Option[R] = Misc.boolToOpt(b, v)

  private lazy val validateRepType: Unit = {
    if (optRepType.isDefined && optRepType.get.isInstanceOf[PrimitiveType]) {
      val ees = enclosingElements
      //
      // for all enclosing elements (if this is a named type, there could be several),
      // they all have to be inputValueCalc.
      //
      val areNotAllIVC = ees.exists { ee => ee.isRepresented }
      if (areNotAllIVC) {
        //
        // Also, we don't care about this parsing check if all uses of the type
        // are unparse-only usages.
        //
        val isAtLeastOneUsageForParsing =
          ees.exists { ee => ee.defaultParseUnparsePolicy != ParseUnparsePolicy.UnparseOnly }
        if (isAtLeastOneUsageForParsing) {
          SDE(
            "Primitive types can only be used as repTypes for parsing when the enclosing element is computed with inputValueCalc",
          )
        }
      }
    }
  }

  lazy val noFacetChecks =
    optRestriction
      .map { r =>
        if (
          r.hasPattern || r.hasEnumeration || r.hasMinLength || r.hasMaxLength ||
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
      case <dfdl:simpleType>{contents @ _*}</dfdl:simpleType> =>
        Some(new DFDLSimpleType(node, this))
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

  /**
   * The repType is a QName that resolves to a simple used to hang properties that define the
   * physical representation of this element. When used, it is because we want to convert the
   * physical representation to something else that actually ends up in the infoset, usually
   * with a different logical type. For example, we may want to convert a numeric physical
   * representation to string logical representation for the infoset. This could be done using
   * multiple elements and input/outputValueCalc, but using repType allows it to be done by
   * Daffodil. The task to convert a physical type to a logical type during parse (or in reverse
   * during unparse) is done via a TypeCalculator. The goal of this optTypeCalculator is to
   * inspect the various properties (e.g. repType, enumeration, repValues,
   * repValueRanges) and create a TypeCalculator that does the necessary conversions. This type
   * calculator will later be provided to a parser/unparser to be evaluated at runtime after the
   * physical representation is parsed or prior to being unparsed.
   */
  lazy val optTypeCalculator: Option[TypeCalculator] = LV('optTypeCalculator) {
    optRepType.flatMap(repType => {
      val srcType = repType.primType
      val dstType = primType

      val fromRestriction: Option[TypeCalculator] = optRestriction.flatMap { restriction =>
        if (restriction.enumerations.isEmpty) {
          None
        } else {
          val terms = restriction.enumerations.flatMap { enum =>
            if (enum.optRepValueSet.isEmpty) {
              None
            } else {
              Assert.invariant(enum.canonicalRepValue.isDefined)
              val repValues = enum.optRepValueSet.get
              val canonValues = enum.canonicalRepValue.getNonNullable
              val cookedValue = enum.enumValueCooked
              Some((repValues, canonValues, cookedValue))
            }
          }

          if (terms.isEmpty) {
            if (srcType != dstType) {
              SDE(
                """An enumeration without any dfdlx:repValues or dfdlx:repValueRanges cannot have a different primitve type (%s) than that of its dfdlx:repType="%s" (%s)""",
                dstType.globalQName,
                repType.namedQName,
                srcType.globalQName,
              )
            }
            Some(TypeCalculatorCompiler.compileIdentity(srcType))
          } else {
            if (terms.size != restriction.enumerations.size) {
              SDE(
                "If one enumeration value defines dfdlx:repValues, then all must define a dfdlx:repValues",
              )
            }
            Some(TypeCalculatorCompiler.compileKeysetValue(terms, srcType, dstType))
          }
        }
      }

      schemaDefinitionUnless(
        fromRestriction.isDefined,
        "dfdlx:repType (%s) requires an enumeration with defined dfdlx:repValues",
        repType.namedQName,
      )

      fromRestriction
    })
  }.value

  private lazy val optRepTypeQNameString = findPropertyOption("repType").toOption

  private lazy val optRepTypeQName: Option[RefQName] = LV('optRepTypeQName) {
    optRepTypeQNameString
      .map(qn => {
        QName.resolveRef(qn, namespaces, tunable.unqualifiedPathStepPolicy).toOption match {
          case Some(x) => x
          case None => SDE(s"Cannot resolve type ${qn}")
        }
      })
  }.value

  override lazy val optRepType: Option[SimpleTypeBase with NamedMixin] = LV('optRepType) {
    val optRepTypeDef = optRepTypeQName.map { qname =>
      // throws an SDE if the simple type def is not found or if it is not a simple type (e.g. a
      // primitive type)
      schemaSet.getGlobalSimpleTypeDefNoPrim(qname, "dfdlx:repType", this)
    }
    optRepTypeDef
  }.value

  override lazy val optRepValueSet: Option[RepValueSet] = optRepTypeDef.flatMap(repType => {
    val primType: PrimType = repType.primType

    val fromRestriction: Option[RepValueSet] = optRestriction.flatMap(_.optRepValueSet)
    val fromSelf: Option[RepValueSet] = optRepValueSetFromAttribute

    (fromRestriction, fromSelf) match {
      case (None, None) => None
      case (Some(a), None) => Some(a)
      case (None, Some(a)) => Some(a)
      case (Some(_), Some(_)) =>
        SDE("Cannot put repValues or repRangeValues on a simple type defining an enumeration")
    }

  })

  lazy val optLogicalValueSet: Option[RepValueSet] = {
    val fromRestriction: Option[RepValueSet] = optRestriction.map(_.logicalValueSet)
    fromRestriction
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
  with NestingLexicalMixin
  with HasRepValueAttributes {

  Assert.invariant(xml.label == "enumeration")

  override lazy val optRepType = parentType.optRepType

  lazy val enumValueRaw: String = (xml \ "@value").head.text
  lazy val enumValueCooked: DataValuePrimitive =
    try {
      parentType.primType.fromXMLString(enumValueRaw)
    } catch {
      case e: InvalidPrimitiveDataException =>
        SDE("Invalid data for enumeration: %s", e.getMessage)
    }

  override lazy val optRepValueSet: Option[RepValueSet] = optRepValueSetFromAttribute
  lazy val logicalValueSet: RepValueSet =
    RepValueSetCompiler.compile(Seq(enumValueCooked), Seq())
  lazy val canonicalRepValue: DataValuePrimitiveNullable = {
    val ans1 = repValuesAttrCooked.headOption.getOrElse(DataValue.NoValue)
    val ans2 = repValueRangesAttrCooked.headOption
      .map(_._1)
      .map(asBound => {
        // TODO, currently, if the first repValue comes from an exclusive restriction we cannot
        // infer a canonical repValue
        if (asBound.isInclusive) asBound.maybeBound else DataValue.NoValue
      })
      .getOrElse(DataValue.NoValue)
    val ans = if (ans1.isDefined) ans1 else ans2
    Assert.invariant(ans.isDefined == optRepValueSet.isDefined)
    ans
  }

  override val optReferredToComponent = None

  protected def annotationFactory(node: Node): Option[DFDLAnnotation] =
    Assert.invariantFailed("Should not be called")
  protected lazy val emptyFormatFactory: DFDLFormatAnnotation =
    new DFDLEnumerationFactory(newDFDLAnnotationXML("enumeration"), this)
  protected def isMyFormatAnnotation(a: DFDLAnnotation): Boolean =
    Assert.invariantFailed("Should not be called")

}
