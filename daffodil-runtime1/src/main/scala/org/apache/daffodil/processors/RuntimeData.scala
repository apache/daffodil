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

package org.apache.daffodil.processors

import scala.xml.NamespaceBinding

import org.apache.daffodil.Implicits.ImplicitsSuppressUnusedImportWarning
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.dsom.ConstantExpression
import org.apache.daffodil.dsom.DPathCompileInfo
import org.apache.daffodil.dsom.DPathElementCompileInfo
import org.apache.daffodil.dsom.FacetTypes
import org.apache.daffodil.dsom.ImplementsThrowsSDE
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.HasSchemaFileLocation
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.infoset.PartialNextElementResolver
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.PreSerialization
import org.apache.daffodil.util.TransientParam
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.xml.LocalDeclQName
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.xml.QNameBase
import org.apache.daffodil.xml.RefQName
import org.apache.daffodil.xml.StepQName; object NoWarn { ImplicitsSuppressUnusedImportWarning() }
import java.util.regex.Matcher

import org.apache.daffodil.api.UnqualifiedPathStepPolicy
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DataValue
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveOrUseNilForDefaultOrNull
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.OKOrError

/*
 * NOTE: Any time you add a member to one of these objects, you must modify at least 3 places.
 *
 * 1) the argument list
 * 2) the lazy vals
 * 3) the pre-serialization method
 *
 * If however that member is passed to the constructor of a parent class, then steps 2 and 3
 * are only needed at the top of the class hierarchy. The other classes can just pass the argument
 * to the parent constructor.
 */

sealed trait RuntimeData
  extends ImplementsThrowsSDE
  with HasSchemaFileLocation
  with PreSerialization {
  def schemaFileLocation: SchemaFileLocation
  def diagnosticDebugName: String
  def path: String

  def variableMap: VariableMap
  override def toString = diagnosticDebugName

  def unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy

}

object TermRuntimeData {

  private var nextID = 0

  def generateTermID: Int = synchronized {
    val n = nextID
    nextID += 1
    n
  }

}

sealed abstract class TermRuntimeData(
  /**
   * These transient by-name args are part of how we
   * create a structure here which contains some objects that
   * somewhere within themselves, refer back to this structure.
   *
   * These are passed by-name, and ultimately what is serialized is not
   * these, but lazy vals that refer to them which are forced to have
   * values at the time of object serialization.
   */
  val position: Int,
  @TransientParam partialNextElementResolverArg: => PartialNextElementResolver,
  @TransientParam encodingInfoArg: => EncodingRuntimeData, // depends on CharsetEv
  @TransientParam dpathCompileInfoArg: => DPathCompileInfo,
  val isRepresented:  Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int, // depends ultimately on EncodingEv.isConstant
  val hasNoSkipRegions: Boolean,
  val defaultBitOrder:  BitOrder,
  val optIgnoreCase: Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends RuntimeData {

  private val termID = TermRuntimeData.generateTermID

  final override def hashCode(): Int = termID

  final override def equals(other: Any) = other match {
    case ref: AnyRef => this eq ref
    case _ => false
  }

  def isRequiredScalar: Boolean
  def isArray: Boolean

  /**
   * At some point TermRuntimeData is a ResolvesQNames which requires tunables:
   *
   *  Anything to do with expressions might deal with namespace prefixes on
   *  qnames in expressions, or on NCNames in expressions, so has to have
   *  the namespace default policy tunable so as to be able to be compatible
   *  with IBM's implementation of DPath.
   *
   * TermRuntimeData refers to a DPathCompileInfo which has the namespace
   * prefix, element name, etc. that are needed in order to compile DPath
   * expressions at runtime (in the debugger, which is part of the runtime;
   * hence, need this info at runtime.)
   */
  def unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy = dpathCompileInfo.unqualifiedPathStepPolicy

  lazy val partialNextElementResolver = partialNextElementResolverArg
  lazy val encodingInfo = encodingInfoArg
  lazy val dpathCompileInfo = dpathCompileInfoArg
  lazy val couldHaveText = couldHaveTextArg
  lazy val alignmentValueInBits = alignmentValueInBitsArg
  lazy val maybeFillByteEv = maybeFillByteEvArg
  lazy val maybeCheckByteAndBitOrderEv = maybeCheckByteAndBitOrderEvArg
  lazy val maybeCheckBitOrderAndCharsetEv = maybeCheckBitOrderAndCharsetEvArg

  override def preSerialization: Unit = {
    super.preSerialization
    partialNextElementResolver
    encodingInfo
    dpathCompileInfo
    couldHaveText
    alignmentValueInBits
    maybeFillByteEv
    maybeCheckByteAndBitOrderEv
    maybeCheckBitOrderAndCharsetEv
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

sealed class NonTermRuntimeData(
  override val variableMap:  VariableMap,
  val schemaFileLocation:  SchemaFileLocation,
  val diagnosticDebugName:  String,
  val path: String,
  val namespaces: NamespaceBinding,
  val unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy)
  extends RuntimeData {

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

/**
 * Singleton. If found as the default value, means to use nil as
 * the default value instead of an actual value.
 */
object UseNilForDefault

final class SimpleTypeRuntimeData(
  variableMapArg: VariableMap,
  schemaFileLocationArg:  SchemaFileLocation,
  diagnosticDebugNameArg:  String,
  pathArg:  String,
  namespacesArg:  NamespaceBinding,
  val primType: NodeInfo.PrimType,
  val noFacetChecks:  Boolean,
  val patternValues:  Seq[FacetTypes.FacetValueR],
  val enumerationValues:  Option[String],
  val minLength:  Option[java.math.BigDecimal],
  val maxLength:  Option[java.math.BigDecimal],
  val minInclusive:  Option[java.math.BigDecimal],
  val maxInclusive:  Option[java.math.BigDecimal],
  val minExclusive:  Option[java.math.BigDecimal],
  val maxExclusive:  Option[java.math.BigDecimal],
  val totalDigits:  Option[java.math.BigDecimal],
  val fractionDigits:  Option[java.math.BigDecimal],
  val unionMemberTypes:  Seq[SimpleTypeRuntimeData],
  unqualifiedPathStepPolicyArg: UnqualifiedPathStepPolicy,
  val repTypeRuntimeData:  Option[SimpleTypeRuntimeData],
  val repValueSet: Option[RepValueSet],
  val typeCalculator: Option[TypeCalculator],
  val optRepPrimType:  Option[PrimType])
  extends NonTermRuntimeData(variableMapArg, schemaFileLocationArg, diagnosticDebugNameArg,
    pathArg, namespacesArg, unqualifiedPathStepPolicyArg) {

  import org.apache.daffodil.util.OKOrError._

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  /**
   * These are creators of regex pattern matcher objects. we want to avoid
   * allocating Matchers, so this is thread-safe in terms of allocating and returning the
   * matchers, and we need this generally because matchers are stateful so cannot
   * be shared across threads.
   */
  def matchers: (Seq[Matcher], Option[Matcher]) = matcherTL.get()
  private lazy val matcherTL = new ThreadLocal[(Seq[Matcher], Option[Matcher])] {
    protected final override def initialValue() = {
      val patternMatchers = patternValues.map { case (_, r) => r.pattern.matcher("") }
      val optEnumMatcher = enumerationValues.map { en => en.r.pattern.matcher("") }
      (patternMatchers, optEnumMatcher)
    }
  }

  /**
   * This recursively walks down a tree of restriction/union types.
   *
   * For each restriction, it checks facets. For each union, it stops at the
   * first things satisfied, and sets the unionMember in the infoset element,
   * which provides DFDL's unionMemberSchema functionality.
   *
   * Note that we don't pre-flatten unions into flat lists. We could but
   * since a union can be a restriction derived from a union, and within that
   * union can be other restrictions derived from other unions, we'd have to
   * massage around the facet checking (push-down) to the "leaf level" types
   * of the union, and that would result in them being checked redundantly, and
   * potentially make diagnostics harder because the transformed type wouldn't
   * match the written DFDL schema's structure.
   *
   * Better to just walk the nest recursively.
   */
  def executeCheck(currentElement: DISimple): OKOrError = {
    if (currentElement.isNilled) OK
    else {
      val facetResult = if (noFacetChecks) OK else executeFacetCheck(currentElement)
      if (facetResult.isError) facetResult
      else {
        val umts = unionMemberTypes
        if (umts.length == 0) OK
        else {
          val optUnionMemberRuntimeData = umts.find { mt =>
            mt.executeCheck(currentElement).isOK
          }
          if (optUnionMemberRuntimeData.isDefined) {
            if (currentElement.unionMemberRuntimeData.isEmpty) {
              currentElement.setUnionMemberRuntimeData(optUnionMemberRuntimeData.get)
            }
            OK
          } else
            Error("Value '%s' is not one of the union members: %s".format(
              currentElement.dataValueAsString,
              umts.map { umt => umt.diagnosticDebugName }.mkString(", ")))
        }
      }
    }
  }

  /**
   * Performs the constraint checks using information contained within the
   * PState object.
   *
   * @param pstate the current parser state.
   *
   * @return a Unit on success, String (message) on failure.
   */

  def executeFacetCheck(currentElement: DISimple): OKOrError = {
    Assert.usage(!noFacetChecks)
    Assert.invariant(!currentElement.isNilled)

    val e = this

    lazy val (patternMatchers, optEnumMatcher) = this.matchers

    if (e.patternValues.isDefinedAt(0)) {
      val check = checkPatterns(currentElement, patternMatchers)
      if (!check) {
        val patternStrings = e.patternValues.map { case (_, pattern) => pattern }.mkString(",")
        return Error("facet pattern(s): %s".format(patternStrings))
      }
    }

    if (e.enumerationValues.isDefined) {
      val check = checkEnumerations(currentElement, optEnumMatcher.get)
      if (!check) {
        return Error("facet enumeration(s): %s".format(e.enumerationValues.mkString(",")))
      }
    }

    // Check minLength
    e.minLength.foreach { minLength =>
      val minAsLong = minLength.longValue()
      val isMinLengthGreaterThanEqToZero = minAsLong.compareTo(0L) >= 0
      if (isMinLengthGreaterThanEqToZero) {
        if (!checkMinLength(currentElement, minLength, e, primType))
          return Error("facet minLength (%s)".format(minLength))
      }
    }
    // Check maxLength
    e.maxLength.foreach { maxLength =>
      val maxAsLong = maxLength.longValue()
      val isMaxLengthGreaterThanEqToZero = maxAsLong.compareTo(0L) >= 0
      if (isMaxLengthGreaterThanEqToZero) {
        if (!checkMaxLength(currentElement, maxLength, e, primType))
          return Error("facet maxLength (%s)".format(maxLength))
      }
    }
    // Check minInclusive
    e.minInclusive.foreach { minInclusive =>
      if (!checkMinInc(currentElement, minInclusive, primType, e))
        return Error("facet minInclusive (%s)".format(minInclusive))
    }
    // Check maxInclusive
    e.maxInclusive.foreach { maxInclusive =>
      if (!checkMaxInc(currentElement, maxInclusive, primType, e))
        return Error("facet maxInclusive (%s)".format(maxInclusive))
    }
    // Check minExclusive
    e.minExclusive.foreach { minExclusive =>
      if (!checkMinExc(currentElement, minExclusive, primType, e))
        return Error("facet minExclusive (%s)".format(minExclusive))
    }
    // Check maxExclusive
    e.maxExclusive.foreach { maxExclusive =>
      if (!checkMaxExc(currentElement, maxExclusive, primType, e))
        return Error("facet maxExclusive (%s)".format(maxExclusive))
    }

    // Check totalDigits
    e.totalDigits.foreach { totalDigits =>
      val tdLong = totalDigits.longValue()
      val isTotalDigitsGreaterThanEqToZero = tdLong.compareTo(0L) >= 0
      if (isTotalDigitsGreaterThanEqToZero) {
        if (!checkTotalDigits(currentElement, tdLong))
          return Error("facet totalDigits (%s)".format(totalDigits))
      }
    }
    // Check fractionDigits
    e.fractionDigits.foreach { fractionDigits =>
      val fdLong = fractionDigits.longValue()
      val isFractionDigitsGreaterThanEqToZero = fdLong.compareTo(0L) >= 0
      if (isFractionDigitsGreaterThanEqToZero) {
        if (!checkFractionDigits(currentElement, fdLong))
          return Error("facet fractionDigits (%s)".format(fractionDigits))
      }
    }

    // Note: dont check occurs counts // if(!checkMinMaxOccurs(e, pstate.arrayPos)) { return java.lang.Boolean.FALSE }
    OK
  }

  private def checkMinLength(diNode: DISimple, minValue: java.math.BigDecimal,
    e: ThrowsSDE, primType: PrimType): java.lang.Boolean = {
    val minAsLong = minValue.longValueExact()
    primType match {
      case PrimType.String => {
        val data = diNode.dataValue.getString
        val dataLen = data.length.toLong
        val isDataLengthLess = dataLen.compareTo(minAsLong) < 0
        if (isDataLengthLess) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        val data = diNode.dataValue.getByteArray

        val dataLen = data.length.toLong
        val isDataLengthEqual = dataLen.compareTo(minAsLong) == 0
        if (isDataLengthEqual) java.lang.Boolean.TRUE
        else java.lang.Boolean.FALSE
      }
      case _ => e.SDE("MinLength facet is only valid for string and hexBinary.")
    }
  }

  private def checkMaxLength(diNode: DISimple, maxValue: java.math.BigDecimal,
    e: ThrowsSDE, primType: PrimType): java.lang.Boolean = {
    val maxAsLong = maxValue.longValueExact()
    primType match {
      case PrimType.String => {
        val data: String = diNode.dataValue.getString
        val dataLen: Long = data.length.toLong
        val isDataLengthGreater = dataLen.compareTo(maxAsLong) > 0
        if (isDataLengthGreater) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        val data: Array[Byte] = diNode.dataValue.getByteArray
        // Has to come through as a string in infoset
        // hex string is exactly twice as long as number of bytes
        // take length / 2 = length
        val dataLen = data.length.toLong
        val isDataLengthEqual = dataLen.compareTo(maxAsLong) == 0
        if (isDataLengthEqual) java.lang.Boolean.TRUE
        else java.lang.Boolean.FALSE
      }
      case _ => e.SDE("MaxLength facet is only valid for string and hexBinary.")
    }
  }

  private def checkMinInc(diNode: DISimple, minValue: java.math.BigDecimal, primType: PrimType, e: ThrowsSDE): Boolean = {
    val bdData = diNode.dataValueAsBigDecimal
    val isDataGreaterThanEqToMinInc = bdData.compareTo(minValue) >= 0
    isDataGreaterThanEqToMinInc
  }

  private def checkMinExc(diNode: DISimple, minValue: java.math.BigDecimal, primType: PrimType, e: ThrowsSDE): Boolean = {
    val bdData = diNode.dataValueAsBigDecimal
    val isDataGreaterThanEqToMinExc = bdData.compareTo(minValue) > 0
    isDataGreaterThanEqToMinExc
  }

  private def checkMaxInc(diNode: DISimple, maxValue: java.math.BigDecimal, primType: PrimType, e: ThrowsSDE): Boolean = {
    val bdData = diNode.dataValueAsBigDecimal
    val isDataLessThanEqToMaxInc = bdData.compareTo(maxValue) <= 0
    isDataLessThanEqToMaxInc
  }

  private def checkMaxExc(diNode: DISimple, maxValue: java.math.BigDecimal, primType: PrimType, e: ThrowsSDE): Boolean = {
    val bdData = diNode.dataValueAsBigDecimal
    val isDataLessThanMaxExc = bdData.compareTo(maxValue) < 0
    isDataLessThanMaxExc
  }

  private def checkTotalDigits(diNode: DISimple, digits: Long): Boolean = {
    // Per http://www.w3.org/TR/xmlschema-2/#rf-totalDigits
    // |i| < 10^totalDigits

    val number = new java.math.BigDecimal(scala.math.pow(10.0, digits.doubleValue()))
    val biNumber = new java.math.BigInteger(number.intValueExact().toString())
    val bdData = diNode.dataValueAsBigDecimal.unscaledValue()
    val isDataLessThanNumber = bdData.compareTo(biNumber) < 0
    isDataLessThanNumber
  }

  private def checkFractionDigits(diNode: DISimple, digits: Long): Boolean = {
    val bdData = diNode.dataValueAsBigDecimal
    // Rounding HALF_DOWN prevents us from accidentally increasing the value.
    val rounded = bdData.setScale(digits.intValue(), java.math.RoundingMode.HALF_DOWN)
    val isDataSameAsRounded = bdData.compareTo(rounded) == 0
    isDataSameAsRounded
  }

  private def checkEnumerations(diNode: DISimple, enumMatcher: Matcher): Boolean = {
    val data = diNode.dataValueAsString
    enumMatcher.reset(data)
    enumMatcher.matches()
  }

  private def checkPatterns(diNode: DISimple, matchers: Seq[Matcher]): Boolean = {
    val data = diNode.dataValueAsString

    var isSuccess: Boolean = true

    var i: Int = 0
    var done = false
    val n = matchers.length
    while (!done && i < n) {
      val m = matchers(i)
      i += 1
      // each pattern within simpleType is OR'd
      // each pattern between simpleType's is AND'd

      // Each elem represents a simpleType
      // each simpleType is allowed a facetPattern
      // each facetPattern represents all patterns on this particular
      // simpleType.
      //
      // Ex.
      // <SimpleType name="A">
      //   <restriction base="B">
      //     <pattern value="1"/>
      //     <pattern value="2"/>
      //   </restriction>
      // </SimpleType>
      //
      // <SimpleType name="B">
      //   <restriction base="int">
      //     <pattern value="3"/>
      //     <pattern value="4"/>
      //   </restriction>
      // </SimpleType>
      //
      // Here facetPattern for SimpleType-A = "1|2" (OR'd)

      // All patterns between simpleTypes must match (AND'd)
      m.reset(data)
      if (!m.matches()) {
        isSuccess = false
        done = true
      }
    }
    isSuccess
  }

}

/*
 * These objects have become too big. Most processors don't need most of this stuff.
 *
 * The objective should be to take things OUT of this structure and pass directly to the
 * constructor of the parser/unparser.
 *
 * These objects are for things that are generally heavily used everywhere like information for
 * providing diagnostics.
 */

sealed class ElementRuntimeData(
  /**
   * These transient by-name args are part of how we
   * create a structure here which contains some objects that
   * somewhere within themselves, refer back to this structure.
   *
   * These are passed by-name, and ultimately what is serialized is not
   * these, but lazy vals that refer to them which are forced to have
   * values at the time of object serialization.
   *
   * Note that all transient elements must be added to the preSerialization method below
   * to allow parser serialization/deserialization to work.
   */
  positionArg: Int,
  @TransientParam childrenArg: => Seq[ElementRuntimeData],
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam partialNextElementResolverArg: => PartialNextElementResolver,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam dpathElementCompileInfoArg: => DPathElementCompileInfo,
  val schemaFileLocation: SchemaFileLocation,
  val diagnosticDebugName: String,
  val path: String,
  @TransientParam minimizedScopeArg: => NamespaceBinding,
  defaultBitOrderArg: BitOrder,
  @TransientParam optPrimTypeArg: => Option[PrimType],
  @TransientParam targetNamespaceArg: => NS,
  @TransientParam thisElementsNamespaceArg: => NS,
  @TransientParam optSimpleTypeRuntimeDataArg: => Option[SimpleTypeRuntimeData],
  @TransientParam optComplexTypeModelGroupRuntimeDataArg: => Option[ModelGroupRuntimeData],
  @TransientParam minOccursArg: => Long,
  @TransientParam maxOccursArg: => Long,
  @TransientParam maybeOccursCountKindArg: => Maybe[OccursCountKind],
  @TransientParam nameArg: => String,
  @TransientParam targetNamespacePrefixArg: => String,
  @TransientParam thisElementsNamespacePrefixArg: => String,
  @TransientParam isNillableArg: => Boolean,
  @TransientParam isArrayArg: => Boolean, // can have more than 1 occurrence
  @TransientParam isOptionalArg: => Boolean, // can have only 0 or 1 occurrence
  @TransientParam isRequiredInUnparseInfosetArg: => Boolean, // must have at least 1 occurrence
  /**
   * This is the properly qualified name for recognizing this
   * element.
   *
   * This takes into account xs:schema's elementFormDefault attribute.
   * If 'qualified' then there will be a namespace component.
   * If 'unqualified' the the namespace component will be No_Namespace.
   */
  @TransientParam namedQNameArg: => NamedQName,
  isRepresentedArg: Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  hasNoSkipRegionsArg: Boolean,
  @TransientParam impliedRepresentationArg: => Representation,
  optIgnoreCaseArg: Option[YesNo],
  @TransientParam optDefaultValueArg: => DataValuePrimitiveOrUseNilForDefaultOrNull,
  //
  // Unparser-specific arguments
  //
  @TransientParam optTruncateSpecifiedLengthStringArg: => Option[Boolean],
  @TransientParam outputValueCalcExprArg: => Option[CompiledExpression[AnyRef]],
  @TransientParam maybeBinaryFloatRepEvArg: => Maybe[BinaryFloatRepEv],
  @TransientParam maybeByteOrderEvArg: => Maybe[ByteOrderEv],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv],
  @TransientParam isQuasiElementArg: => Boolean)
  extends TermRuntimeData(positionArg, partialNextElementResolverArg,
    encInfoArg, dpathElementCompileInfoArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg,
    defaultBitOrderArg, optIgnoreCaseArg, maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg) {

  override def isRequiredScalar = !isArray && isRequiredInUnparseInfoset
  override def namespaces: NamespaceBinding = dpathElementCompileInfo.namespaces

  lazy val children = childrenArg
  lazy val variableMap = variableMapArg
  lazy val encInfo = encInfoArg
  lazy val dpathElementCompileInfo = dpathElementCompileInfoArg
  lazy val minimizedScope = minimizedScopeArg
  lazy val optPrimType = optPrimTypeArg
  lazy val targetNamespace = targetNamespaceArg
  lazy val thisElementsNamespace = thisElementsNamespaceArg
  lazy val optSimpleTypeRuntimeData = optSimpleTypeRuntimeDataArg
  lazy val optComplexTypeModelGroupRuntimeData = optComplexTypeModelGroupRuntimeDataArg
  lazy val minOccurs = minOccursArg
  lazy val maxOccurs = maxOccursArg
  lazy val maybeOccursCountKind = maybeOccursCountKindArg
  lazy val name = nameArg
  lazy val targetNamespacePrefix = targetNamespacePrefixArg
  lazy val thisElementsNamespacePrefix = thisElementsNamespacePrefixArg
  lazy val isNillable = isNillableArg
  override lazy val isArray = isArrayArg
  lazy val isOptional = isOptionalArg
  lazy val isRequiredInUnparseInfoset = isRequiredInUnparseInfosetArg // if true, no uncertainty about number of occurrences.
  lazy val namedQName = namedQNameArg
  lazy val impliedRepresentation = impliedRepresentationArg
  lazy val optDefaultValue = optDefaultValueArg
  lazy val optTruncateSpecifiedLengthString = optTruncateSpecifiedLengthStringArg
  lazy val outputValueCalcExpr = outputValueCalcExprArg
  lazy val maybeBinaryFloatRepEv = maybeBinaryFloatRepEvArg
  lazy val maybeByteOrderEv = maybeByteOrderEvArg
  lazy val isQuasiElement = isQuasiElementArg

  override def preSerialization: Unit = {
    super.preSerialization
    children
    variableMap
    encInfo
    dpathElementCompileInfo
    minimizedScope
    optPrimType
    targetNamespace
    thisElementsNamespace
    optSimpleTypeRuntimeData
    optComplexTypeModelGroupRuntimeData
    minOccurs
    maxOccurs
    maybeOccursCountKind
    name
    targetNamespacePrefix
    thisElementsNamespacePrefix
    isNillable
    isArray
    isOptional
    isRequiredInUnparseInfoset
    namedQName
    impliedRepresentation
    optDefaultValue
    optTruncateSpecifiedLengthString
    outputValueCalcExpr
    maybeBinaryFloatRepEv
    maybeByteOrderEv
    isQuasiElement
  }

  @throws(classOf[java.io.IOException])
  private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  final def childERDs = children

  def isSimpleType = optPrimType.isDefined

  def schemaURIStringsForFullValidation = schemaURIStringsForFullValidation1.distinct
  private def schemaURIStringsForFullValidation1: Seq[String] = (schemaFileLocation.uriString +:
    childERDs.flatMap { _.schemaURIStringsForFullValidation1 })

  def isComplexType = !isSimpleType

  def prefixedName = {
    if (thisElementsNamespacePrefix != null) {
      thisElementsNamespacePrefix + ":" + name
    } else {
      name
    }
  }

}

/**
 * Used when unparsing to indicate that an expected ERD could not be created.
 *
 * Subclasses of this are for specific reasons why.
 *
 * The purposes of this is to allow the InfosetInputter to actually construct an
 * infoset event including a full infoset DIElement node, yet indicating that the
 * event is invalid.
 *
 * This enables improved diagnostic behavior. For example, a StartElement event
 * can be created for a name and namespace where that name + namespace are not
 * expected, the resulting infoset event will contain a DIElement having an
 * ErrorERD. The unparser can then inspect the infoset event, and
 * if it is expecting an EndElement event, it can issue a UnparseError that
 * correctly identifies that an expected EndElement event was not received.
 *
 * In all cases, there is no recovering from these errors in the Unparser; hence,
 * we don't really care if these bogus DIElement nodes having these Error ERDs are
 * spliced into the infoset or not.
 */
sealed abstract class ErrorERD(local: String, namespaceURI: String)
  extends ElementRuntimeData(
    0, // position
    Nil, // children
    null, // VariableMap
    null, // PartialNextElementResolver
    null, // EncodingRuntimeData
    new DPathElementCompileInfo(
      Nil, // parentsArg: => Seq[DPathElementCompileInfo],
      null, // variableMap: => VariableMap,
      Nil, // elementChildrenCompileInfoArg: => Seq[DPathElementCompileInfo],
      null, // namespaces: scala.xml.NamespaceBinding,
      local, // path: String,
      local, // val name: String,
      false, // val isArray: Boolean,
      LocalDeclQName(None, local, NS(namespaceURI)), // val namedQName: NamedQName,
      None, // val optPrimType: Option[PrimType],
      null, // sfl: SchemaFileLocation,
      null, // override val unqualifiedPathStepPolicy : UnqualifiedPathStepPolicy,
      null, // typeCalcMap: TypeCalcMap,
      null, // lexicalContextRuntimeData: RuntimeData,
      null, // val sscd: String),
      false), // val hasOutputValueCalc: Boolean
    null, // SchemaFileLocation
    local, // diagnosticDebugName: String,
    local, // pathArg: => String,
    null, // minimizedScopeArg: => NamespaceBinding,
    null, //defaultBitOrderArg: => BitOrder,
    None, // optPrimTypeArg: => Option[PrimType],
    null, // targetNamespaceArg: => NS,
    NS(namespaceURI), // thisElementsNamespaceArg: => NS,
    null, // optSimpleTypeRuntimeDataArg: => Option[SimpleTypeRuntimeData],
    null, // optComplexTypeModelGroupRuntimeDataArg: => Option[ModelGroupRuntimeData],
    0L, // minOccursArg: => Long,
    0L, // maxOccursArg: => Long,
    Nope, // maybeOccursCountKindArg: => Maybe[OccursCountKind],
    local, // nameArg: => String,
    null, // targetNamespacePrefixArg: => String,
    null, // thisElementsNamespacePrefixArg: => String,
    false, // isNillableArg: => Boolean,
    false, // isArrayArg: => Boolean, // can have more than 1 occurrence
    false, // isOptionalArg: => Boolean, // can have only 0 or 1 occurrence
    false, // isRequiredInUnparseInfosetArg: => Boolean, // must have at least 1 occurrence
    LocalDeclQName(None, local, NS(namespaceURI)), // namedQNameArg: => NamedQName,
    false, // isRepresentedArg: => Boolean,
    false, // couldHaveTextArg: => Boolean,
    0, // alignmentValueInBitsArg: => Int,
    false, // hasNoSkipRegionsArg: => Boolean,
    null, // impliedRepresentationArg: => Representation,
    null, // optIgnoreCaseArg: => Option[YesNo],
    DataValue.NoValue, // optDefaultValueArg: => DataValuePrimitiveOrUseNilForDefaultOrNull,
    null, // optTruncateSpecifiedLengthStringArg: => Option[Boolean],
    null, // outputValueCalcExprArg: => Option[CompiledExpression[AnyRef]],
    Nope, // maybeBinaryFloatRepEvArg: => Maybe[BinaryFloatRepEv],
    Nope, // maybeByteOrderEvArg: => Maybe[ByteOrderEv],
    Nope, // maybeFillByteEvArg: => Maybe[FillByteEv],
    Nope, // maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
    Nope, // maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv],
    false // isQuasiElementArg: => Boolean
  ) {

  override def toString() = Misc.getNameFromClass(this) + "(" + this.namedQName.toExtendedSyntax + ")"

  @throws(classOf[java.io.IOException])
  private def writeObject(out: java.io.ObjectOutputStream): Unit =
    Assert.usageError("Not for serialization")
}

/**
 * Used when unparsing to indicate that a next element event was detected that is
 * unexpected.
 */
final class UnexpectedElementErrorERD(
  optTRD: Option[TermRuntimeData],
  local: String,
  namespaceURI: String,
  val allPossibleNQNs: Seq[QNameBase])
  extends ErrorERD(local, namespaceURI) {
}

/**
 * Used when unparsing to indicate that multiple elements could be next that
 * differ only by namespace. This means for some event sources (like JSON) we
 * don't know which element to create.
 */
final class NamespaceAmbiguousElementErrorERD(
  optTRD: Option[TermRuntimeData],
  local: String,
  namespaceURI: String,
  val allPossibleNQNs: Seq[QNameBase])
  extends ErrorERD(local, namespaceURI) {

  /**
   * Causes unparse error with diagnostic about unexpected element.
   *
   * Pass argument true if the context is one where no element was expected
   * (e.g., because an EndElement was expected.)
   *
   * Pass false if the Term's ordinary nextElementResolver list of possibilities
   * is what was expected.
   */
  def toUnparseError(nothingWasExpected: Boolean = false) = {
    val sqn = StepQName(None, name, thisElementsNamespace)
    val sqnx = sqn.toExtendedSyntax
    val allPossiblesString =
      allPossibleNQNs.map { _.toExtendedSyntax }.mkString(", ")
    val maybeLoc: Maybe[SchemaFileLocation] = Maybe.toMaybe(optTRD.map { _.schemaFileLocation })
    UnparseError(maybeLoc, Nope,
      "Found multiple matches for element %s because infoset implementation ignores namespaces. Matches are %s",
      sqnx, allPossiblesString)
  }
}

sealed abstract class ModelGroupRuntimeData(
  /**
   * These transient by-name args are part of how we
   * create a structure here which contains some objects that
   * somewhere within themselves, refer back to this structure.
   *
   * These are passed by-name, and ultimately what is serialized is not
   * these, but lazy vals that refer to them which are forced to have
   * values at the time of object serialization.
   */
  positionArg: Int,
  @TransientParam partialNextElementResolverArg: => PartialNextElementResolver,
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  val schemaFileLocation:  SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  val diagnosticDebugName:  String,
  val path:  String,
  defaultBitOrderArg: BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  isRepresentedArg: Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  alignmentValueInBitsArg:  Int,
  hasNoSkipRegionsArg:  Boolean,
  optIgnoreCaseArg: Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends TermRuntimeData(
    positionArg, partialNextElementResolverArg,
    encInfoArg, ciArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg,
    defaultBitOrderArg, optIgnoreCaseArg, maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg) {

  final override def isRequiredScalar = true
  final override def isArray = false
  override def namespaces: NamespaceBinding = ci.namespaces

  lazy val variableMap = variableMapArg
  lazy val encInfo = encInfoArg
  lazy val ci = ciArg
  lazy val groupMembers = groupMembersArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
    encInfo
    ci
    groupMembers
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

final class SequenceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * create a structure here which contains some objects that
   * somewhere within themselves, refer back to this structure.
   *
   * These are passed by-name, and ultimately what is serialized is not
   * these, but lazy vals that refer to them which are forced to have
   * values at the time of object serialization.
   */
  positionArg: Int,
  @TransientParam partialNextElementResolverArg: => PartialNextElementResolver,
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  schemaFileLocationArg: SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  diagnosticDebugNameArg: String,
  pathArg:  String,
  defaultBitOrderArg: BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  isRepresentedArg: Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  alignmentValueInBitsArg: Int,
  hasNoSkipRegionsArg: Boolean,
  optIgnoreCaseArg: Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends ModelGroupRuntimeData(positionArg, partialNextElementResolverArg,
    variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, diagnosticDebugNameArg, pathArg, defaultBitOrderArg, groupMembersArg,
    isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, optIgnoreCaseArg,
    maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg)

final class ChoiceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * create a structure here which contains some objects that
   * somewhere within themselves, refer back to this structure.
   *
   * These are passed by-name, and ultimately what is serialized is not
   * these, but lazy vals that refer to them which are forced to have
   * values at the time of object serialization.
   */
  positionArg: Int,
  @TransientParam partialNextElementResolverArg: => PartialNextElementResolver,
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  schemaFileLocationArg: SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  diagnosticDebugNameArg: String,
  pathArg: String,
  defaultBitOrderArg: BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  isRepresentedArg: Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  alignmentValueInBitsArg: Int,
  hasNoSkipRegionsArg: Boolean,
  optIgnoreCaseArg: Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends ModelGroupRuntimeData(positionArg, partialNextElementResolverArg,
    variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, diagnosticDebugNameArg, pathArg, defaultBitOrderArg, groupMembersArg,
    isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, optIgnoreCaseArg, maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg)

final class VariableRuntimeData(
  schemaFileLocationArg:  SchemaFileLocation,
  diagnosticDebugNameArg:  String,
  pathArg: String,
  namespacesArg: NamespaceBinding,
  val external: Boolean,
  @TransientParam maybeDefaultValueExprArg: => Maybe[CompiledExpression[AnyRef]],
  val typeRef:  RefQName,
  val globalQName: GlobalQName,
  val primType:  NodeInfo.PrimType,
  unqualifiedPathStepPolicyArg:  UnqualifiedPathStepPolicy)
  extends NonTermRuntimeData(
    null, // no variable map
    schemaFileLocationArg,
    diagnosticDebugNameArg,
    pathArg,
    namespacesArg,
    unqualifiedPathStepPolicyArg)
  with Serializable {

  lazy val maybeDefaultValueExpr = maybeDefaultValueExprArg

  override def preSerialization: Unit = {
    super.preSerialization
    maybeDefaultValueExpr
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  private lazy val state =
    if (!maybeDefaultValueExpr.isDefined) VariableUndefined
    else VariableDefined

  private lazy val value: DataValuePrimitiveNullable =
    if (maybeDefaultValueExpr.isEmpty) DataValue.NoValue
    else {
      val defaultValueExpr = maybeDefaultValueExpr.get
      defaultValueExpr match {
        case constExpr: ConstantExpression[_] => DataValue.unsafeFromAnyRef(constExpr.constant)
        case _ => DataValue.NoValue
      }
    }

  def createVariableInstance: VariableInstance = VariableInstance(state, value, this, maybeDefaultValueExpr)

}
