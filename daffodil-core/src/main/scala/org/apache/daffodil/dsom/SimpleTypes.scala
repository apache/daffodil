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
import org.apache.daffodil.schema.annotation.props.gen.Enumeration_AnnotationMixin
import org.apache.daffodil.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.ExecutionMode
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.xml.QName
import org.apache.daffodil.schema.annotation.props.NotFound
import org.apache.daffodil.processors.RepValueSet
import org.apache.daffodil.processors.RepValueSet
import org.apache.daffodil.processors.TypeCalculator
import org.apache.daffodil.processors.TypeCalculatorCompiler
import org.apache.daffodil.processors.RepValueSetCompiler
import org.apache.daffodil.dpath.NodeInfo.SignedIntegerKind
import org.apache.daffodil.dpath.NodeInfo.SignedInteger
import org.apache.daffodil.dpath.NodeInfo.Numeric
import org.apache.daffodil.dpath.NodeInfo.PrimType.UnsignedInt
import org.apache.daffodil.cookers.RepValueCooker
import org.apache.daffodil.cookers.RepValueCooker
import scala.xml.Elem
import scala.xml.MetaData
import scala.xml.UnprefixedAttribute
import scala.xml.Null
import org.apache.daffodil.processors.RangeBound
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.xml.XMLUtils
import scala.xml.NamespaceBinding
import org.apache.daffodil.processors.IdentifyTypeCalculator
import org.apache.daffodil.xml.NS
import org.apache.daffodil.exceptions.SchemaFileLocation

trait TypeBase {
  def optRestriction: Option[Restriction] = None
  def optUnion: Option[Union] = None
  def typeNode: NodeInfo.AnyType.Kind
}

trait NonPrimTypeMixin

sealed trait SimpleTypeBase extends TypeBase
  with HasOptRepTypeMixin {

  def primType: PrimType
}

/*
 * For components which can define dfdlx:repValues and dfdlx:repValueRanges
 * Construct the repValueSet using only the above mentioned attributes on the element itself
 * Applies to simpleType and enumeration
 *
 * In the case of simpleType, it is possible that optRepValueSetFromAttribute will be none
 * but the element will still have an optRepValueSet for another source (eg. children elements)
 */
sealed trait HasRepValueAttributes extends AnnotatedSchemaComponent
  with ResolvesProperties {

  def optRepType: Option[SimpleTypeBase]
  def optRepValueSet: Option[RepValueSet[AnyRef]]

  lazy val (repValuesAttrCooked: Seq[AnyRef], repValueRangesAttrCooked: Seq[(RangeBound[BigInt], RangeBound[BigInt])]) =
    optRepType match {
      case Some(repType) => {
        val repValueSetRaw = findPropertyOption("repValues").toOption
          .map(_.split("\\s+").toSeq).getOrElse(Seq())
        val repValueRangesRaw = findPropertyOption("repValueRanges").toOption
          .map(_.split("\\s+").toSeq).getOrElse(Seq())
        repType.primType match {
          case PrimType.String => {
            if (repValueRangesRaw.size > 0) SDE("repValueRanges set when using a string repType")
            val repValueSetCooked = repValueRangesRaw.map(RepValueCooker.convertConstant(_, this, false))
            (repValueSetCooked, Seq())
          }
          case _: NodeInfo.Integer.Kind => {
            if (repValueRangesRaw.size % 2 != 0) SDE("repValueRanges must have an even number of elements")
            val ans1 = repValueSetRaw.map(BigInt(_))
            def cookRepValueRanges(xs: Seq[String]): Seq[(RangeBound[BigInt], RangeBound[BigInt])] = {
              xs match {
                case Seq() => Seq()
                case a +: b +: rest => {
                  val a2 = BigInt(a)
                  val b2 = BigInt(b)
                  if (a2.compare(b2) > 0) {
                    SDE("min value must not be greater than max value")
                  }
                  val a3 = new RangeBound(Maybe(a2), true)
                  val b3 = new RangeBound(Maybe(b2), true)
                  (a3, b3) +: cookRepValueRanges(rest)
                }
              }
            }
            val ans2 = cookRepValueRanges(repValueRangesRaw)
            (ans1, ans2)
          }
          case x => SDE("repType must be either String or Integer type")
        }
      }
      case None => (Seq(), Seq())
    }

  lazy val optRepValueSetFromAttribute: Option[RepValueSet[AnyRef]] = optRepType.flatMap(repType => {

    val ans = RepValueSetCompiler.compile(repValuesAttrCooked, repValueRangesAttrCooked.asInstanceOf[Seq[(RangeBound[AnyRef], RangeBound[AnyRef])]])
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
final class PrimitiveType private (tn: PrimType)
  extends SimpleTypeBase
  with NamedMixin {
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
  override def schemaDocument = Assert.invariantFailed("Primitive types do not have schemaDocument")

  /*
   * These methods don't really make sense here, but are needed by NamedMixin
   */
  override def SDE(id: String, args: Any*) = Assert.invariantFailed("Primitive types shouldn't ever have an SDE")
  override def schemaFileLocation = Assert.invariantFailed("Primitive types don't have a schemaFileLocation")
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

abstract class SimpleTypeDefBase(xml: Node, lexicalParent: SchemaComponent)
  extends AnnotatedSchemaComponentImpl(xml, lexicalParent)
  with SimpleTypeBase
  with NonPrimTypeMixin
  with ProvidesDFDLStatementMixin
  with OverlapCheckMixin
  with HasOptRepTypeMixinImpl
  with NamedMixin
  with HasRepValueAttributes {

  requiredEvaluations(simpleTypeRuntimeData.preSerialization)

  override def typeNode = primType

  def toOpt[R <: AnyRef](b: Boolean, v: => R) = Misc.boolToOpt(b, v)

  //Perfoming this validation on construction causes infinite loops
  //So we defer it to later
  def validate: Unit = {
    if (optRepType.isDefined
      && optRepType.get.isInstanceOf[PrimitiveType]
      && enclosingElement.isDefined
      && enclosingElement.get.isRepresented) {
      SDE("Primitive types can only be used as repTypes when the enclosing element is computed with inputValueCalc")
    }
  }

  lazy val simpleTypeRuntimeData: SimpleTypeRuntimeData = {
    validate
    new SimpleTypeRuntimeData(
      variableMap,
      schemaFileLocation,
      diagnosticDebugName,
      path,
      namespaces,
      primType,
      noFacetChecks,
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
      tunable,
      optRepTypeDef.map(_.simpleTypeRuntimeData),
      optRepValueSet,
      optTypeCalculator)
  }

  private lazy val noFacetChecks =
    optRestriction.map { r =>
      if (r.hasPattern || r.hasEnumeration || r.hasMinLength || r.hasMaxLength ||
        r.hasMinInclusive || r.hasMaxInclusive || r.hasMinExclusive || r.hasMaxExclusive ||
        r.hasTotalDigits || r.hasFractionDigits) false
      else true
    }.getOrElse(true)

  // override def name = diagnosticDebugName // don't do this. names are used by diagnosticDebugName

  override final def optReferredToComponent = optRestriction.flatMap { _.optBaseDef }
  override final def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  override final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  override final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => Some(new DFDLSimpleType(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def primType: NodeInfo.PrimType = {
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
      (None, Some(new Union(unionNodeSeq(0), this)))
    } else {
      (Some(new Restriction(restrictionNodeSeq(0), this)), None)
    }
  }

  lazy val optInputTypeCalc = findPropertyOption("inputTypeCalc")
  lazy val optOutputTypeCalc = findPropertyOption("outputTypeCalc")

  lazy val optTypeCalculator: Option[TypeCalculator[AnyRef, AnyRef]] = {
    optRepType.flatMap(repType => {
      val srcType = repType.primType
      val dstType = primType

      val fromRestriction: Option[TypeCalculator[AnyRef, AnyRef]] = optRestriction.flatMap({ restriction =>
        val enumerations = restriction.enumerations.filter(_.optRepValueSet.isDefined)
        if (enumerations.isEmpty) {
          /*
           * In theory, we require srcType == dstType.
           * But, we also compute this when we are an expression calculator.
           * In such a case, the above invariant may not hold, which is okay, because we
           * will not actually use the identity calculator.
           */
          Some(TypeCalculatorCompiler.compileIdentity(srcType))
        } else {
          if (enumerations.size != restriction.enumerations.size) {
            SDE("If one enumeration value defines a repValue, then all must define a repValue")
          }
          val terms = enumerations.map(enum => (enum.optRepValueSet.get, enum.canonicalRepValue.get, enum.enumValueCooked))
          Some(TypeCalculatorCompiler.compileKeysetValue(terms, srcType, dstType))
        }
      })
      val fromUnion: Option[TypeCalculator[AnyRef, AnyRef]] = optUnion.map({ union =>
        val subCalculators: Seq[(RepValueSet[AnyRef], RepValueSet[AnyRef], TypeCalculator[AnyRef, AnyRef])] =
          union.unionMemberTypes.map(subType => (subType.optRepValueSet.get, subType.optLogicalValueSet.get, subType.optTypeCalculator.get))
        TypeCalculatorCompiler.compileUnion(subCalculators)
      })
      val fromExpression: Option[TypeCalculator[AnyRef, AnyRef]] = {
        val optInputCompiled = optInputTypeCalc.toOption.map(sExpr => {
          val prop = optInputTypeCalc.asInstanceOf[Found]
          val qn = GlobalQName(Some("daf"), "inputTypeCalc", XMLUtils.dafintURI)
          val exprNamespaces = prop.location.namespaces
          val exprComponent = prop.location.asInstanceOf[SchemaComponent]
          ExpressionCompilers.AnyRef.compileExpression(
            qn,
            dstType, sExpr, exprNamespaces, exprComponent.dpathCompileInfo, false, this, dpathCompileInfo)
        })
        val optOutputCompiled = optOutputTypeCalc.toOption.map(sExpr => {
          val prop = optOutputTypeCalc.asInstanceOf[Found]
          val qn = GlobalQName(Some("daf"), "outputTypeCalc", XMLUtils.dafintURI)
          val exprNamespaces = prop.location.namespaces
          val exprComponent = prop.location.asInstanceOf[SchemaComponent]
          ExpressionCompilers.AnyRef.compileExpression(
            qn,
            srcType, sExpr, exprNamespaces, exprComponent.dpathCompileInfo, false, this, dpathCompileInfo)
        })
        (optInputCompiled, optOutputCompiled) match {
          case (None, None) => None
          case _ => Some(TypeCalculatorCompiler.compileExpression(optInputCompiled, optOutputCompiled, srcType, dstType))
        }
      }

      val ans = (fromRestriction, fromUnion, fromExpression) match {
        case (Some(x), None, None) => Some(x)
        case (None, Some(x), None) => Some(x)
        case (None, None, Some(x)) => SDE("Usage of inputTypeCalc and outputTypeCalc requires an empty xs:restriction to determine the base type.")
        case (Some(x), _, Some(y)) if x.isInstanceOf[IdentifyTypeCalculator[AnyRef]] => Some(y)
        case (None, None, None) => {
          if (dstType != srcType) {
            val repTypeName = optRepTypeDef match {
              case Some(r) => r.diagnosticDebugName
              case None => repType.toString()
            }
            SDE(
              "repType (%s) with primitive type (%s) used without defining a transformation is not compatable with the baseType of (%s) with primitive type (%s)",
              repTypeName, srcType.name,
              diagnosticDebugName, dstType.name)
          }
          None
        }
        case (Some(_), Some(_), _) => Assert.invariantFailed("Cannot combine an enumeration with a union")
        case (Some(_), _, Some(_)) => SDE("Cannot use typeCalcExpressions while defining repValues of enumerations")
        case (_, Some(_), Some(_)) => SDE("Cannot use typeCalcExpressions while using a union that defines typeCalcs")
      }

      ans match {
        case Some(idt: IdentifyTypeCalculator[AnyRef]) => {
          if (srcType != dstType) {
            SDE("Identity transform requires that the basetype and reptype have a common primitive type")
          }
        }
        case _ => ()
      }

      ans

    })
  }

  /*
   * We don't really need the NamedMixin. It is only used for detecting duplicates
   * However, since only named types can be a repType, there is no problem
   * in requiring them to be named
   */
  override lazy val optRepType: Option[SimpleTypeBase with NamedMixin] = LV('optRepType) {
    lazy val fromSelf: Option[SimpleTypeBase with NamedMixin] = {
      val qName = findPropertyOption("repType").toOption
        .map(qn => {
          QName.resolveRef(qn, namespaces, tunable).toOption match {
            case Some(x) => x
            case None => SDE(s"Cannot resolve type ${qn}")
          }
        })
      val optRepTypeDef = qName.flatMap(schemaSet.getGlobalSimpleTypeDef(_))
      val optRepPrimType = qName.flatMap(schemaSet.getPrimitiveType(_))
      Assert.invariant(!(optRepPrimType.isDefined && optRepTypeDef.isDefined))
      if (qName.isDefined) {
        schemaDefinitionUnless(optRepTypeDef.isDefined || optRepPrimType.isDefined, s"Cannot find reptype ${qName.get}")
      }
      optRepTypeDef.orElse(optRepPrimType)
    }
    lazy val fromUnion: Option[SimpleTypeBase with NamedMixin] = optUnion.flatMap(union => {
      val repTypes = union.unionMemberTypes.map(_.optRepTypeDef)
      //check that all repTypes are the same
      //Because of how we inline types, we do not expect to see structual equality,
      //so we rely on the xml qname instead
      val numRepTypes = repTypes.map(_.map(_.namedQName)).toSet.size
      if (numRepTypes > 1) {
        SDE("If any child type of a union has a repType, they all must have the same repType")
      }
      if (numRepTypes == 0) {
        None
      } else {
        repTypes.head
      }
    })
    /*
     * Note that there is no fromRestriction option here
     * In theory, we could consider every restriction type without an explicit repType to be
     * either a restriction or identity transform.
     * In practice, this would introduce the overhead of a transform to almost every derived type.
     * Instead, when a user needs a restriction transform, they must simply provide the reptype explitly,
     * which is arguably a good design decision from a readability standpoint of the schema as well.
     */
    fromSelf.orElse(fromUnion)
  }.toOption.flatten

  override lazy val optRepValueSet: Option[RepValueSet[AnyRef]] = optRepTypeDef.flatMap(repType => {
    val primType: PrimType = repType.primType

    val fromRestriction: Option[RepValueSet[AnyRef]] = optRestriction.flatMap(_.optRepValueSet)
    val fromUnion: Option[RepValueSet[AnyRef]] = {
      val componentTypes = optUnion.map(_.unionMemberTypes).getOrElse(Seq())
      val componentValueSets = componentTypes.flatMap(_.optRepValueSet)
      val ans = componentValueSets.fold(RepValueSetCompiler.empty)((a, b) => a.merge(b))
      if (ans.isEmpty) None else Some(ans)
    }
    val fromSelf: Option[RepValueSet[AnyRef]] = optRepValueSetFromAttribute

    (fromRestriction, fromUnion, fromSelf) match {
      case (None, None, None) => None
      case (Some(a), None, None) => Some(a)
      case (None, Some(a), None) => Some(a)
      case (None, None, Some(a)) => Some(a)
      case (Some(_), Some(_), _) => throw new IllegalStateException("Can't happen")
      case (Some(_), _, Some(_)) => SDE("Cannot put repValues or repRangeValues on a simple type defining an enumeration")
      case (_, Some(_), Some(_)) => SDE("Cannot put repValue or repRangeValues on a simple type defined by a union")
    }

  })

  lazy val optLogicalValueSet: Option[RepValueSet[AnyRef]] = {
    val fromRestriction: Option[RepValueSet[AnyRef]] = optRestriction.map(_.logicalValueSet)
    val fromUnion: Option[RepValueSet[AnyRef]] = optUnion.map(union => {
      val subsets = union.unionMemberTypes.map(_.optLogicalValueSet).filter(_.isDefined).map(_.get)
      subsets.fold(RepValueSetCompiler.empty)((a, b) => a.merge(b))
    })
    fromRestriction.orElse(fromUnion)
  }

}

final class LocalSimpleTypeDef(
  xmlArg: Node, lexicalParent: SchemaComponent)
  extends SimpleTypeDefBase(xmlArg, lexicalParent)
  with LocalNonElementComponentMixin
  with NestingLexicalMixin {

  /**
   * For anonymous simple type def, uses the base name, or primitive type name
   */

  override lazy val diagnosticDebugName: String = {
    //
    // TODO: implement a daf:name property to give an alternate name. If present, use that.
    //
    val baseName = optRestriction.flatMap { r =>
      r.optBaseDef.map { _.namedQName }.orElse(Some(r.primType.globalQName))
    }.getOrElse(this.optUnion.map { u => u.primType.globalQName }.getOrElse(
      //Note that this.toString=diagnosticDebugName, so SDE cannot be used here
      Assert.invariantFailed("Anonymous Simple type is neither a union nor a restriction. Enclosing element is " + this.lexicalParent)))
    // furthermore, we can't call things that throw SDEs either.
    val repName = optRepTypeDef.map(_.name)
    repName match {
      case None => baseName.diagnosticDebugName
      case Some(n) => s"${n} -> ${baseName.diagnosticDebugName}"
    }
  }
}

/**
 * Call forElement, and supply an element using this globalSimpalType, and you
 * get back an instance that is one-to-one with the element.
 *
 * Call forDeriviedType, and supply a different simpleType that uses this one,
 * and you get back an instance that is one-to-one with that other type
 */
final class GlobalSimpleTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument)
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
final class EnumerationDefFactory(
  xml: Node,
  parentType: SimpleTypeDefBase)
  extends SchemaComponentFactory(xml, parentType.schemaDocument)
  with NestingLexicalMixin
  with HasRepValueAttributes
  with ResolvesProperties {

  Assert.invariant(xml.label == "enumeration")

  override lazy val optRepType = parentType.optRepType

  lazy val enumValueRaw: String = (xml \ "@value").head.text
  lazy val enumValueCooked: AnyRef = parentType.primType.fromXMLString(enumValueRaw)

  override lazy val optRepValueSet: Option[RepValueSet[AnyRef]] = optRepValueSetFromAttribute
  lazy val logicalValueSet: RepValueSet[AnyRef] = RepValueSetCompiler.compile(Seq(enumValueCooked), Seq())
  lazy val canonicalRepValue: Option[AnyRef] = {
    val ans1 = repValuesAttrCooked.headOption
    val ans2 = repValueRangesAttrCooked.headOption.map(_._1).flatMap(asBound => {
      //TODO, currently, if the first repValue comes from an exclusive restriction we cannot
      //infer a canonical repValue
      if (asBound.isInclusive) (Some(asBound.maybeBound.get)) else None
    })
    val ans = ans1.orElse(ans2)
    Assert.invariant(ans.isDefined == optRepValueSet.isDefined)
    ans
  }

  override protected val optReferredToComponent = None

  protected def annotationFactory(node: Node): Option[DFDLAnnotation] = Assert.invariantFailed("Should not be called")
  protected def emptyFormatFactory: DFDLFormatAnnotation = new DFDLEnumerationFactory(newDFDLAnnotationXML("enumeration"), this)
  protected def isMyFormatAnnotation(a: DFDLAnnotation): Boolean = Assert.invariantFailed("Should not be called")

}
