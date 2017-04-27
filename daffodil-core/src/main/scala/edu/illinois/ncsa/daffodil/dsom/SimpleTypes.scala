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
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.processors.SimpleTypeRuntimeData
import edu.illinois.ncsa.daffodil.util.Misc

trait TypeBase {
  def optRestriction: Option[Restriction] = None
  def optUnion: Option[Union] = None
  def typeNode: NodeInfo.AnyType.Kind
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

abstract class SimpleTypeDefBase(xmlArg: Node, override val parent: SchemaComponent)
  extends AnnotatedSchemaComponent(xmlArg, parent)
  with SimpleTypeBase
  with DFDLStatementMixin
  with OverlapCheckMixin {

  def element: ElementBase

  override def typeNode = primType

  final lazy val restrictions = {
    val thisR = optRestriction.toSeq
    val res = thisR ++
      thisR.flatMap { _.derivationBaseRestrictions }
    res
  }

  final lazy val bases: Seq[SimpleTypeDefBase] =
    if (restrictions.isEmpty) Nil
    else restrictions.tail.map { _.simpleType }

  private lazy val sTypeNonDefault: Seq[ChainPropProvider] = bases.reverse.map { _.nonDefaultFormatChain }
  private lazy val sTypeDefault: Seq[ChainPropProvider] = bases.reverse.map { _.defaultFormatChain }

  // want a QueueSet i.e., fifo order if iterated, but duplicates
  // kept out of the set. Will simulate by calling distinct.
  def nonDefaultPropertySources = LV('nonDefaultPropertySources) {
    val seq = (this.nonDefaultFormatChain +: sTypeNonDefault).distinct
    checkNonOverlap(seq)
    seq
  }.value

  def defaultPropertySources = LV('defaultPropertySources) {
    val seq = (this.defaultFormatChain +: sTypeDefault).distinct
    seq
  }.value

  protected final def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => Some(new DFDLSimpleType(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
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

  /**
   * Combine our statements with those of our base def (if there is one)
   *
   * The order is important here. I.e., we FIRST put in each list those from our base. Then our own local ones.
   */
  final lazy val statements: Seq[DFDLStatement] =
    bases.flatMap { _.statements } ++ localStatements

  private lazy val optBaseDef = optRestriction.flatMap { _.optBaseDef }

  // TODO: refactor into shared code for combining all the annotations in the resolved set of annotations
  // for a particular annotation point, checking that there is only one format annotation, that
  // asserts and discriminators are properly excluding each-other, etc.
  // Code should be sharable for many kinds of annotation points, perhaps specialized for groups, complex type
  // elements, and simple type elements.
  //
  // See JIRA issue DFDL-481
  final lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    optBaseDef.map { _.newVariableInstanceStatements }.getOrElse(Seq.empty) ++ localNewVariableInstanceStatements
  final lazy val (discriminatorStatements, assertStatements) = checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)
  private lazy val combinedAsserts: Seq[DFDLAssert] = optBaseDef.map { _.assertStatements }.getOrElse(Nil) ++ localAssertStatements
  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] = optBaseDef.map { _.discriminatorStatements }.getOrElse(Nil) ++ localDiscriminatorStatements

  final lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs = optBaseDef.map { _.setVariableStatements }.getOrElse(Nil) ++ localSetVariableStatements
    checkDistinctVariableNames(combinedSvs)
  }
  def toOpt[R <: AnyRef](b: Boolean, v: => R) = Misc.boolToOpt(b, v)

  lazy val simpleTypeRuntimeData: SimpleTypeRuntimeData = {
    new SimpleTypeRuntimeData(
      variableMap,
      schemaFileLocation,
      diagnosticDebugName,
      path,
      namespaces,
      element.simpleType.primType,
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
      optUnion.orElse(optRestriction.flatMap { _.optUnion }).toSeq.flatMap { _.unionMemberTypes.map { _.simpleTypeRuntimeData } })
  }

  private lazy val noFacetChecks =
    optRestriction.map { r =>
      if (r.hasPattern || r.hasEnumeration || r.hasMinLength || r.hasMaxLength ||
        r.hasMinInclusive || r.hasMaxInclusive || r.hasMinExclusive || r.hasMaxExclusive ||
        r.hasTotalDigits || r.hasFractionDigits) false
      else true
    }.getOrElse(true)
}

sealed abstract class SimpleTypeDefFactory(xml: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponentFactory(xml, schemaDocumentArg) {

}

final class LocalSimpleTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SimpleTypeDefFactory(xmlArg, schemaDocumentArg)
  with LocalNonElementComponentMixin {

  def forElement(element: ElementBase) =
    new LocalSimpleTypeDef(this, element)

}

final class LocalSimpleTypeDef(
  val factory: LocalSimpleTypeDefFactory,
  elementArg: ElementBase)
  extends SimpleTypeDefBase(factory.xml, elementArg)
  with LocalNonElementComponentMixin
  with NestingLexicalMixin {

  override def term = element
  override lazy val element = elementArg

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
  extends SimpleTypeDefFactory(xmlArg, schemaDocumentArg)
  with GlobalNonElementComponentMixin {

  /**
   * Create a private instance for this element's use.
   */
  def forElement(element: ElementBase) = new GlobalSimpleTypeDef(None, this, Some(element))
  def forDerivedType(derivedType: SimpleTypeDefBase) = new GlobalSimpleTypeDef(Some(derivedType), this, None)

}
/**
 * The instance type for global simple type definitions.
 */

final class GlobalSimpleTypeDef(
  derivedType: Option[SimpleTypeDefBase],
  val factory: GlobalSimpleTypeDefFactory,
  val referringElement: Option[ElementBase])
  extends SimpleTypeDefBase(factory.xml, factory.schemaDocument)
  with GlobalNonElementComponentMixin
  with NestingTraversesToReferenceMixin {

  override def term = element

  override lazy val referringComponent: Option[SchemaComponent] =
    (derivedType, referringElement) match {
      case (Some(dt), None) => derivedType
      case (None, Some(elem)) => referringElement
      case _ => Assert.impossible("SimpleType must either have a derivedType or an element. Not both.")
    }

  override lazy val element: ElementBase = referringComponent match {
    case Some(dt: SimpleTypeDefBase) => dt.element
    case Some(e: ElementBase) => e
    case _ => Assert.invariantFailed("unexpected referringComponent")
  }

}

