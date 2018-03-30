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
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dsom.DPathCompileInfo
import org.apache.daffodil.dsom.ImplementsThrowsSDE
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.dsom.ConstantExpression
import org.apache.daffodil.exceptions.HasSchemaFileLocation
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import org.apache.daffodil.util.PreSerialization
import org.apache.daffodil.util.TransientParam
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.xml.RefQName
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.xml.NS
import org.apache.daffodil.dsom.FacetTypes
import scala.xml.NamespaceBinding
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.xml._
import org.apache.daffodil.dsom.DPathElementCompileInfo
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.infoset.NextElementResolver
import org.apache.daffodil.util.TransientParam
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import org.apache.daffodil.exceptions._
import org.apache.daffodil.Implicits._; object NoWarn { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.infoset._
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.util.OKOrError
import java.util.regex.Matcher
import org.apache.daffodil.api.DaffodilTunables

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
  with HasSchemaFileLocation {
  val schemaFileLocation: SchemaFileLocation
  val diagnosticDebugName: String
  val path: String
  val namespaces: NamespaceBinding

  def variableMap: VariableMap
  override def toString = diagnosticDebugName

  def tunable: DaffodilTunables

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
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam encodingInfoArg: => EncodingRuntimeData,
  @TransientParam dpathCompileInfoArg: => DPathCompileInfo,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam optIgnoreCaseArg: => Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends RuntimeData
  with Serializable
  with PreSerialization {

  private val termID = TermRuntimeData.generateTermID

  final override def hashCode(): Int = termID

  final override def equals(other: Any) = other match {
    case ref: AnyRef => this eq ref
    case _ => false
  }

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
  def tunable = dpathCompileInfo.tunable

  lazy val encodingInfo = encodingInfoArg
  lazy val dpathCompileInfo = dpathCompileInfoArg
  lazy val isRepresented = isRepresentedArg
  lazy val couldHaveText = couldHaveTextArg
  lazy val alignmentValueInBits = alignmentValueInBitsArg
  lazy val hasNoSkipRegions = hasNoSkipRegionsArg
  lazy val defaultBitOrder = defaultBitOrderArg
  lazy val optIgnoreCase = optIgnoreCaseArg
  lazy val maybeFillByteEv = maybeFillByteEvArg
  lazy val maybeCheckByteAndBitOrderEv = maybeCheckByteAndBitOrderEvArg
  lazy val maybeCheckBitOrderAndCharsetEv = maybeCheckBitOrderAndCharsetEvArg

  override def preSerialization: Unit = {
    super.preSerialization
    encodingInfo
    dpathCompileInfo
    isRepresented
    couldHaveText
    alignmentValueInBits
    hasNoSkipRegions
    defaultBitOrder
    optIgnoreCase
    maybeFillByteEv
    tunable
    maybeCheckByteAndBitOrderEv
    maybeCheckBitOrderAndCharsetEv
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

sealed class NonTermRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam tunableArg: => DaffodilTunables)
  extends RuntimeData
  with PreSerialization {

  override lazy val variableMap = variableMapArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val diagnosticDebugName = diagnosticDebugNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val tunable = tunableArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
    schemaFileLocation
    diagnosticDebugName
    path
    namespaces
    tunable
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

/**
 * Singleton. If found as the default value, means to use nil as
 * the default value instead of an actual value.
 */
object UseNilForDefault

final class SimpleTypeRuntimeData(
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam primTypeArg: => NodeInfo.PrimType,
  @TransientParam noFacetChecksArg: => Boolean,
  @TransientParam patternValuesArg: => Seq[FacetTypes.FacetValueR],
  @TransientParam enumerationValuesArg: => Option[String],
  @TransientParam minLengthArg: => Option[java.math.BigDecimal],
  @TransientParam maxLengthArg: => Option[java.math.BigDecimal],
  @TransientParam minInclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam maxInclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam minExclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam maxExclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam totalDigitsArg: => Option[java.math.BigDecimal],
  @TransientParam fractionDigitsArg: => Option[java.math.BigDecimal],
  @TransientParam unionMemberTypesArg: => Seq[SimpleTypeRuntimeData],
  @TransientParam tunableArg: => DaffodilTunables) extends NonTermRuntimeData(variableMapArg, schemaFileLocationArg, diagnosticDebugNameArg,
  pathArg, namespacesArg, tunableArg) {

  import OKOrError._

  lazy val primType = primTypeArg
  lazy val noFacetChecks = noFacetChecksArg
  lazy val patternValues = patternValuesArg
  lazy val enumerationValues = enumerationValuesArg
  lazy val minLength = minLengthArg
  lazy val maxLength = maxLengthArg
  lazy val minInclusive = minInclusiveArg
  lazy val maxInclusive = maxInclusiveArg
  lazy val minExclusive = minExclusiveArg
  lazy val maxExclusive = maxExclusiveArg
  lazy val totalDigits = totalDigitsArg
  lazy val fractionDigits = fractionDigitsArg
  lazy val unionMemberTypes = unionMemberTypesArg

  override def preSerialization: Unit = {
    super.preSerialization
    primType
    noFacetChecks
    patternValues
    enumerationValues
    minLength
    maxLength
    minInclusive
    maxInclusive
    minExclusive
    maxExclusive
    totalDigits
    fractionDigits
    unionMemberTypes
  }

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
        val data = diNode.dataValue.asInstanceOf[String]
        val dataLen = data.length.toLong
        val isDataLengthLess = dataLen.compareTo(minAsLong) < 0
        if (isDataLengthLess) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        val data = diNode.dataValue.asInstanceOf[Array[Byte]]

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
        val data: String = diNode.dataValue.asInstanceOf[String]
        val dataLen: Long = data.length.toLong
        val isDataLengthGreater = dataLen.compareTo(maxAsLong) > 0
        if (isDataLengthGreater) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        val data: Array[Byte] = diNode.dataValue.asInstanceOf[Array[Byte]]
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

final class ElementRuntimeData(
  /**
   * These transient by-name args are part of how we hook these objects into a
   * parent-child tree without having to use an assignment to a var. Note that
   * all transient elements must be added to the preSerialization method below
   * to allow parser serialization/deserialization to work.
   */
  @TransientParam childrenArg: => Seq[ElementRuntimeData],
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam nextElementResolverArg: => NextElementResolver,
  @TransientParam childElementResolverArg: => NextElementResolver,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam dpathElementCompileInfoArg: => DPathElementCompileInfo,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam minimizedScopeArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam optPrimTypeArg: => Option[PrimType],
  @TransientParam targetNamespaceArg: => NS,
  @TransientParam thisElementsNamespaceArg: => NS,
  @TransientParam optSimpleTypeRuntimeDataArg: => Option[SimpleTypeRuntimeData],
  @TransientParam minOccursArg: => Option[Int],
  @TransientParam maxOccursArg: => Option[Int],
  @TransientParam nameArg: => String,
  @TransientParam targetNamespacePrefixArg: => String,
  @TransientParam thisElementsNamespacePrefixArg: => String,
  @TransientParam isHiddenArg: => Boolean,
  @TransientParam isNillableArg: => Boolean,
  @TransientParam isArrayArg: => Boolean, // can have more than 1 occurrence
  @TransientParam isOptionalArg: => Boolean, // can have only 0 or 1 occurrence
  @TransientParam isRequiredArg: => Boolean, // must have at least 1 occurrence
  /**
   * This is the properly qualified name for recognizing this
   * element.
   *
   * This takes into account xs:schema's elementFormDefault attribute.
   * If 'qualified' then there will be a namespace component.
   * If 'unqualified' the the namespace component will be No_Namespace.
   */
  @TransientParam namedQNameArg: => NamedQName,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam impliedRepresentationArg: => Representation,
  @TransientParam optIgnoreCaseArg: => Option[YesNo],
  @TransientParam optDefaultValueArg: => Option[AnyRef],
  //
  // Unparser-specific arguments
  //
  @TransientParam optTruncateSpecifiedLengthStringArg: => Option[Boolean],
  @TransientParam outputValueCalcExprArg: => Option[CompiledExpression[AnyRef]],
  @TransientParam maybeBinaryFloatRepEvArg: => Maybe[BinaryFloatRepEv],
  @TransientParam maybeByteOrderEvArg: => Maybe[ByteOrderEv],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends TermRuntimeData(encInfoArg, dpathElementCompileInfoArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg,
    defaultBitOrderArg, optIgnoreCaseArg, maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg) {

  lazy val children = childrenArg
  lazy val variableMap = variableMapArg
  lazy val nextElementResolver = nextElementResolverArg
  lazy val childElementResolver = childElementResolverArg
  lazy val encInfo = encInfoArg
  lazy val dpathElementCompileInfo = dpathElementCompileInfoArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val diagnosticDebugName = diagnosticDebugNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val minimizedScope = minimizedScopeArg
  lazy val optPrimType = optPrimTypeArg
  lazy val targetNamespace = targetNamespaceArg
  lazy val thisElementsNamespace = thisElementsNamespaceArg
  lazy val optSimpleTypeRuntimeData = optSimpleTypeRuntimeDataArg
  lazy val minOccurs = minOccursArg
  lazy val maxOccurs = maxOccursArg
  lazy val name = nameArg
  lazy val targetNamespacePrefix = targetNamespacePrefixArg
  lazy val thisElementsNamespacePrefix = thisElementsNamespacePrefixArg
  lazy val isHidden = isHiddenArg
  lazy val isNillable = isNillableArg
  lazy val isArray = isArrayArg
  lazy val isOptional = isOptionalArg
  lazy val isRequired = isRequiredArg
  lazy val namedQName = namedQNameArg
  lazy val impliedRepresentation = impliedRepresentationArg
  lazy val optDefaultValue = optDefaultValueArg
  lazy val optTruncateSpecifiedLengthString = optTruncateSpecifiedLengthStringArg
  lazy val outputValueCalcExpr = outputValueCalcExprArg
  lazy val maybeBinaryFloatRepEv = maybeBinaryFloatRepEvArg
  lazy val maybeByteOrderEv = maybeByteOrderEvArg

  override def preSerialization: Unit = {
    super.preSerialization
    children
    variableMap
    nextElementResolver
    childElementResolver
    encInfo
    dpathElementCompileInfo
    schemaFileLocation
    diagnosticDebugName
    path
    namespaces
    minimizedScope
    optPrimType
    targetNamespace
    thisElementsNamespace
    optSimpleTypeRuntimeData
    minOccurs
    maxOccurs
    name
    targetNamespacePrefix
    thisElementsNamespacePrefix
    isHidden
    isNillable
    isArray
    isOptional
    isRequired
    namedQName
    impliedRepresentation
    optDefaultValue
    optTruncateSpecifiedLengthString
    outputValueCalcExpr
    maybeBinaryFloatRepEv
    maybeByteOrderEv
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

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

sealed abstract class ModelGroupRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam optIgnoreCaseArg: => Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends TermRuntimeData(
    encInfoArg, ciArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg,
    defaultBitOrderArg, optIgnoreCaseArg, maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg) {

  lazy val variableMap = variableMapArg
  lazy val encInfo = encInfoArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val ci = ciArg
  lazy val diagnosticDebugName = diagnosticDebugNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val groupMembers = groupMembersArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
    encInfo
    schemaFileLocation
    ci
    diagnosticDebugName
    path
    namespaces
    groupMembers
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

final class SequenceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam optIgnoreCaseArg: => Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends ModelGroupRuntimeData(variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, diagnosticDebugNameArg, pathArg, namespacesArg, defaultBitOrderArg, groupMembersArg,
    isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, optIgnoreCaseArg,
    maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg)

final class ChoiceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam optIgnoreCaseArg: => Option[YesNo],
  @TransientParam maybeFillByteEvArg: => Maybe[FillByteEv],
  @TransientParam maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
  @TransientParam maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv])
  extends ModelGroupRuntimeData(variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, diagnosticDebugNameArg, pathArg, namespacesArg, defaultBitOrderArg, groupMembersArg,
    isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, optIgnoreCaseArg, maybeFillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg)

final class VariableRuntimeData(
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam externalArg: => Boolean,
  @TransientParam maybeDefaultValueExprArg: => Maybe[CompiledExpression[AnyRef]],
  @TransientParam typeRefArg: => RefQName,
  @TransientParam globalQNameArg: => GlobalQName,
  @TransientParam primTypeArg: => NodeInfo.PrimType,
  @TransientParam tunableArg: => DaffodilTunables)
  extends NonTermRuntimeData(
    null, // no variable map
    schemaFileLocationArg,
    diagnosticDebugNameArg,
    pathArg,
    namespacesArg,
    tunableArg)
  with Serializable {

  lazy val external = externalArg
  lazy val maybeDefaultValueExpr = maybeDefaultValueExprArg
  lazy val typeRef = typeRefArg
  lazy val globalQName = globalQNameArg
  lazy val primType = primTypeArg

  override def preSerialization: Unit = {
    super.preSerialization
    external
    maybeDefaultValueExpr
    typeRef
    globalQName
    primType
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  private lazy val state =
    if (!maybeDefaultValueExpr.isDefined) VariableUndefined
    else VariableDefined

  private lazy val maybeValue: Maybe[AnyRef] =
    if (maybeDefaultValueExpr.isEmpty) Nope
    else {
      val defaultValueExpr = maybeDefaultValueExpr.get
      defaultValueExpr match {
        case constExpr: ConstantExpression[_] => One(constExpr.constant.asInstanceOf[AnyRef])
        case _ => Nope
      }
    }

  def newVariableInstance: Variable = Variable(state, maybeValue, this, maybeDefaultValueExpr)

}
