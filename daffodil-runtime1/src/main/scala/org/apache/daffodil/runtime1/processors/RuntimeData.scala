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

package org.apache.daffodil.runtime1.processors

import java.lang.{ Double => JDouble, Float => JFloat }
import scala.util.matching.Regex
import scala.xml.NamespaceBinding

import org.apache.daffodil.lib.Implicits.ImplicitsSuppressUnusedImportWarning
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.HasSchemaFileLocation
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.schema.annotation.props.gen.VariableDirection
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo
import org.apache.daffodil.lib.util.Delay
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.xml.GlobalQName
import org.apache.daffodil.lib.xml.LocalDeclQName
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.QNameBase
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.lib.xml.StepQName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo
import org.apache.daffodil.runtime1.dsom.DPathElementCompileInfo
import org.apache.daffodil.runtime1.dsom.FacetTypes
import org.apache.daffodil.runtime1.dsom.ImplementsThrowsSDE
import org.apache.daffodil.runtime1.infoset.PartialNextElementResolver;
object NoWarn { ImplicitsSuppressUnusedImportWarning() }
import java.util.regex.Matcher

import org.apache.daffodil.lib.api.UnqualifiedPathStepPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.OKOrError
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveOrUseNilForDefaultOrNull
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

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
  with Serializable {
  def schemaFileLocation: SchemaFileLocation
  def diagnosticDebugName: String
  def path: String

  def variableMap: VariableMap
  override def toString = diagnosticDebugName

  def unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy

  def namespaces: NamespaceBinding
}

object TermRuntimeData {

  private var nextID = 0

  def generateTermID: Int = synchronized {
    val n = nextID
    nextID += 1
    n
  }

}

/**
 * Base runtime data structure for all terms
 *
 * These Delay-type args are part of how we
 * create a structure here which contains some objects that
 * somewhere within themselves, refer back to this structure.
 *
 * A Delay object is part of a functional programming idiom for creating
 * a cyclic structure without using side-effects and therefore ordering-dependency.
 * It is pretty similar to just passing an argument by-name (lazily) but
 * has some additional sophistication to avoid dragging Scala closures
 * around and ending up with objects being not garbage collectable.
 *
 * The relationhip between RuntimeData and PartialNextElementResolver is
 * indeed cyclic. Simplest to think of it in terms of elements.
 * An ERD has a partialNextElementResolver which exists to figure out
 * the ERD of the next element, which could be this same ERD again, or
 * could be some other ERD which can then be followed by an element with
 * this same ERD again. So the cycle can be immediate or flow through
 * many ERDs and partialNextElementResolvers. Turns out other kinds of
 * terms can affect the next element resolver process (there's a stack
 * of them) so partialNextElementResolver ends up on the TermRuntimeData
 * to share the definition of the slot for this polymorphically.
 */
sealed abstract class TermRuntimeData(
  val position: Int,
  partialNextElementResolverDelay: Delay[PartialNextElementResolver],
  val encodingInfo: EncodingRuntimeData,
  val dpathCompileInfo: DPathCompileInfo,
  val isRepresented: Boolean,
  val couldHaveText: Boolean,
  val alignmentValueInBits: Int, // depends ultimately on EncodingEv.isConstant
  val hasNoSkipRegions: Boolean,
  val defaultBitOrder: BitOrder,
  val optIgnoreCase: Option[YesNo],
  val fillByteEv: FillByteEv,
  val maybeCheckByteAndBitOrderEv: Maybe[CheckByteAndBitOrderEv],
  val maybeCheckBitOrderAndCharsetEv: Maybe[CheckBitOrderAndCharsetEv],
) extends RuntimeData {

  /**
   * Cyclic structures require initialization
   */
  lazy val initialize: Unit = initializeFunction()

  protected def initializeFunction(): Unit = {
    partialNextElementResolver
    dpathCompileInfo.initialize
  }

  final def namespaces = dpathCompileInfo.namespaces

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
  def unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy =
    dpathCompileInfo.unqualifiedPathStepPolicy

  lazy val partialNextElementResolver =
    partialNextElementResolverDelay.value

}

sealed class NonTermRuntimeData(
  override val variableMap: VariableMap,
  val schemaFileLocation: SchemaFileLocation,
  val diagnosticDebugName: String,
  val path: String,
  override val namespaces: NamespaceBinding,
  val unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy,
) extends RuntimeData

/**
 * Singleton. If found as the default value, means to use nil as
 * the default value instead of an actual value.
 */
object UseNilForDefault

final class SimpleTypeRuntimeData(
  variableMapArg: VariableMap,
  schemaFileLocationArg: SchemaFileLocation,
  diagnosticDebugNameArg: String,
  pathArg: String,
  namespacesArg: NamespaceBinding,
  val primType: NodeInfo.PrimType,
  val noFacetChecks: Boolean,
  val patternValues: Seq[FacetTypes.FacetValueR],
  val enumerationValues: Option[String],
  val minLength: Option[java.math.BigDecimal],
  val maxLength: Option[java.math.BigDecimal],
  val minInclusive: Option[java.math.BigDecimal],
  val maxInclusive: Option[java.math.BigDecimal],
  val minExclusive: Option[java.math.BigDecimal],
  val maxExclusive: Option[java.math.BigDecimal],
  val totalDigits: Option[java.math.BigDecimal],
  val fractionDigits: Option[java.math.BigDecimal],
  val unionMemberTypes: Seq[SimpleTypeRuntimeData],
  unqualifiedPathStepPolicyArg: UnqualifiedPathStepPolicy,
  val repTypeRuntimeData: Option[SimpleTypeRuntimeData],
  val repValueSet: Option[RepValueSet],
  val optRepPrimType: Option[PrimType],
) extends NonTermRuntimeData(
    variableMapArg,
    schemaFileLocationArg,
    diagnosticDebugNameArg,
    pathArg,
    namespacesArg,
    unqualifiedPathStepPolicyArg,
  ) {

  import org.apache.daffodil.lib.util.OKOrError._

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
            Error(
              "Value '%s' is not one of the union members: %s".format(
                currentElement.dataValueAsString,
                umts.map { umt => umt.diagnosticDebugName }.mkString(", "),
              ),
            )
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
        // The escaping is important here as error messages were impossible to figure out when control chars were involved.
        val patternStrings = e.patternValues
          .map { case (_, r: Regex) => XMLUtils.escape(r.pattern.pattern()) }
          .mkString(",")
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

    // Note: dont check occurs counts // if(!checkMinMaxOccurs(e, pstate.arrayIterationPos)) { return java.lang.Boolean.FALSE }
    OK
  }

  private def checkMinLength(
    diNode: DISimple,
    minValue: java.math.BigDecimal,
    e: ThrowsSDE,
    primType: PrimType,
  ): java.lang.Boolean = {
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

  private def checkMaxLength(
    diNode: DISimple,
    maxValue: java.math.BigDecimal,
    e: ThrowsSDE,
    primType: PrimType,
  ): java.lang.Boolean = {
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

  private def checkMinInc(
    diNode: DISimple,
    minValue: java.math.BigDecimal,
    primType: PrimType,
    e: ThrowsSDE,
  ): Boolean = {
    // we must handle float and double separately because diNode.dataValue
    // could be Inf/Nan, which cannot be converted to BigDecimal
    diNode.dataValue.getAnyRef match {
      case f: JFloat => f.compareTo(minValue.floatValue) >= 0
      case d: JDouble => d.compareTo(minValue.doubleValue) >= 0
      case _ => diNode.dataValueAsBigDecimal.compareTo(minValue) >= 0
    }
  }

  private def checkMinExc(
    diNode: DISimple,
    minValue: java.math.BigDecimal,
    primType: PrimType,
    e: ThrowsSDE,
  ): Boolean = {
    // we must handle float and double separately because diNode.dataValue
    // could be Inf/Nan, which cannot be converted to BigDecimal
    diNode.dataValue.getAnyRef match {
      case f: JFloat => f.compareTo(minValue.floatValue) > 0
      case d: JDouble => d.compareTo(minValue.doubleValue) > 0
      case _ => diNode.dataValueAsBigDecimal.compareTo(minValue) > 0
    }
  }

  private def checkMaxInc(
    diNode: DISimple,
    maxValue: java.math.BigDecimal,
    primType: PrimType,
    e: ThrowsSDE,
  ): Boolean = {
    // we must handle float and double separately because diNode.dataValue
    // could be Inf/Nan, which cannot be converted to BigDecimal
    diNode.dataValue.getAnyRef match {
      case f: JFloat => f.compareTo(maxValue.floatValue) <= 0
      case d: JDouble => d.compareTo(maxValue.doubleValue) <= 0
      case _ => diNode.dataValueAsBigDecimal.compareTo(maxValue) <= 0
    }
  }

  private def checkMaxExc(
    diNode: DISimple,
    maxValue: java.math.BigDecimal,
    primType: PrimType,
    e: ThrowsSDE,
  ): Boolean = {
    // we must handle float and double separately because diNode.dataValue
    // could be Inf/Nan, which cannot be converted to BigDecimal
    diNode.dataValue.getAnyRef match {
      case f: JFloat => f.compareTo(maxValue.floatValue) < 0
      case d: JDouble => d.compareTo(maxValue.doubleValue) < 0
      case _ => diNode.dataValueAsBigDecimal.compareTo(maxValue) < 0
    }
  }

  private def checkTotalDigits(diNode: DISimple, digits: Long): Boolean = {
    // Per http://www.w3.org/TR/xmlschema-2/#rf-totalDigits
    // |i| < 10^totalDigits

    val bd = diNode.dataValueAsBigDecimal.stripTrailingZeros
    val totalDigits =
      if (bd.scale <= 0) bd.precision - bd.scale
      else Math.max(bd.precision, bd.scale)

    totalDigits <= digits
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

/** Primary Runtime data structure for Elements
 *
 * These objects are for things that are generally heavily used everywhere like information for
 * providing diagnostics.
 *
 * These Delay-type args are part of how we
 * create a structure here which contains some objects that
 * somewhere within themselves, refer back to this structure.
 *
 * These structures are inherently cyclic, particularly for ElementRuntimeData (ERD)
 * the PartialNextElementResolver,
 * which is about figuring out the next ERD given incoming name+namespace
 * and context. Hence it by its very nature contains and returns ERDs and
 * so involves cycles with the ERD structure.
 *
 * To construct this cyclic data structure but still be using functional
 * programming, we use these Delay/lazy evaluation tricks.
 */
sealed class ElementRuntimeData(
  positionArg: Int,
  children: Seq[ElementRuntimeData],
  val variableMap: VariableMap,
  partialNextElementResolverDelay: Delay[PartialNextElementResolver],
  val encInfo: EncodingRuntimeData,
  val dpathElementCompileInfo: DPathElementCompileInfo,
  val schemaFileLocation: SchemaFileLocation,
  val diagnosticDebugName: String,
  val path: String,
  val minimizedScope: NamespaceBinding,
  defaultBitOrderArg: BitOrder,
  val optPrimType: Option[PrimType],
  val targetNamespace: NS,
  val optSimpleTypeRuntimeData: Option[SimpleTypeRuntimeData],
  val optComplexTypeModelGroupRuntimeData: Option[ModelGroupRuntimeData],
  val minOccurs: Long,
  val maxOccurs: Long,
  val maybeOccursCountKind: Maybe[OccursCountKind],
  val name: String,
  val targetNamespacePrefix: String,
  val isNillable: Boolean,
  val isArray: Boolean, // can have more than 1 occurrence
  val isOptional: Boolean, // can have only 0 or 1 occurrence
  val isRequiredInUnparseInfoset: Boolean, // must have at least 1 occurrence

  /**
   * This is the properly qualified name for recognizing this
   * element.
   *
   * This takes into account xs:schema's elementFormDefault attribute.
   * If 'qualified' then there will be a namespace component.
   * If 'unqualified' the the namespace component will be No_Namespace.
   */
  val namedQName: NamedQName,
  isRepresentedArg: Boolean,
  couldHaveTextArg: Boolean,
  alignmentValueInBitsArg: Int,
  hasNoSkipRegionsArg: Boolean,
  val impliedRepresentation: Representation,
  optIgnoreCaseArg: Option[YesNo],
  val optDefaultValue: DataValuePrimitiveOrUseNilForDefaultOrNull,
  //
  // Unparser-specific arguments
  //
  val optTruncateSpecifiedLengthString: Option[Boolean],
  val maybeBinaryFloatRepEv: Maybe[BinaryFloatRepEv],
  val maybeByteOrderEv: Maybe[ByteOrderEv],
  fillByteEvArg: FillByteEv,
  maybeCheckByteAndBitOrderEvArg: Maybe[CheckByteAndBitOrderEv],
  maybeCheckBitOrderAndCharsetEvArg: Maybe[CheckBitOrderAndCharsetEv],
  val isQuasiElement: Boolean,
  val runtimeProperties: java.util.Map[String, String],
) extends TermRuntimeData(
    positionArg,
    partialNextElementResolverDelay,
    encInfo,
    dpathElementCompileInfo,
    isRepresentedArg,
    couldHaveTextArg,
    alignmentValueInBitsArg,
    hasNoSkipRegionsArg,
    defaultBitOrderArg,
    optIgnoreCaseArg,
    fillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg,
  ) {

  override def isRequiredScalar = !isArray && isRequiredInUnparseInfoset

  final def childERDs = children

  def isSimpleType = optPrimType.isDefined

  lazy val schemaURIStringsForFullValidation: Seq[String] =
    schemaURIStringsForFullValidation1.distinct
  private def schemaURIStringsForFullValidation1: Seq[String] = (schemaFileLocation.uriString +:
    childERDs.flatMap { _.schemaURIStringsForFullValidation1 })

  def isComplexType = !isSimpleType

  lazy val prefix = this.minimizedScope.getPrefix(namedQName.namespace)

  lazy val prefixedName = {
    if (prefix != null) {
      prefix + ":" + name
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
      Delay(
        'ErrorERDParents,
        getClass().getName,
        Seq[DPathElementCompileInfo](),
      ).force, // parentsArg: => Seq[DPathElementCompileInfo],
      null, // variableMap: => VariableMap,
      Delay(
        'ErrorERD,
        getClass().getName,
        Seq[DPathElementCompileInfo](),
      ).force, // elementChildrenCompileInfoDelay: Delay[Seq[DPathElementCompileInfo]],
      null, // namespaces: scala.xml.NamespaceBinding,
      local, // path: String,
      local, // val name: String,
      false, // val isArray: Boolean,
      LocalDeclQName(None, local, NS(namespaceURI)), // val namedQName: NamedQName,
      None, // val optPrimType: Option[PrimType],
      null, // sfl: SchemaFileLocation,
      null, // override val unqualifiedPathStepPolicy : UnqualifiedPathStepPolicy,
      null, // val sscd: String),
      false, // val isOutputValueCalc: Boolean
      false, // val isDistinguishedRoot: Boolean
    ),
    null, // SchemaFileLocation
    local, // diagnosticDebugName: String,
    local, // pathArg: => String,
    null, // minimizedScopeArg: => NamespaceBinding,
    null, // defaultBitOrderArg: => BitOrder,
    None, // optPrimTypeArg: => Option[PrimType],
    null, // targetNamespaceArg: => NS,
    null, // optSimpleTypeRuntimeDataArg: => Option[SimpleTypeRuntimeData],
    null, // optComplexTypeModelGroupRuntimeDataArg: => Option[ModelGroupRuntimeData],
    0L, // minOccursArg: => Long,
    0L, // maxOccursArg: => Long,
    Nope, // maybeOccursCountKindArg: => Maybe[OccursCountKind],
    local, // nameArg: => String,
    null, // targetNamespacePrefixArg: => String,
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
    Nope, // maybeBinaryFloatRepEvArg: => Maybe[BinaryFloatRepEv],
    Nope, // maybeByteOrderEvArg: => Maybe[ByteOrderEv],
    null, // fillByteEvArg => FillByteEv
    Nope, // maybeCheckByteAndBitOrderEvArg: => Maybe[CheckByteAndBitOrderEv],
    Nope, // maybeCheckBitOrderAndCharsetEvArg: => Maybe[CheckBitOrderAndCharsetEv],
    false, // isQuasiElementArg: => Boolean
    null, // runtimeProperties: java.util.Map[String,String]
  ) {

  override def toString() =
    Misc.getNameFromClass(this) + "(" + this.namedQName.toExtendedSyntax + ")"

}

/**
 * Used when unparsing to indicate that a next element event was detected that is
 * unexpected.
 */
final class UnexpectedElementErrorERD(
  optTRD: Option[TermRuntimeData],
  local: String,
  namespaceURI: String,
  val allPossibleNQNs: Seq[QNameBase],
) extends ErrorERD(local, namespaceURI) {}

/**
 * Used when unparsing to indicate that multiple elements could be next that
 * differ only by namespace. This means for some event sources (like JSON) we
 * don't know which element to create.
 */
final class NamespaceAmbiguousElementErrorERD(
  optTRD: Option[TermRuntimeData],
  local: String,
  namespaceURI: String,
  val allPossibleNQNs: Seq[QNameBase],
) extends ErrorERD(local, namespaceURI) {

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
    val sqn = StepQName(None, name, namedQName.namespace)
    val sqnx = sqn.toExtendedSyntax
    val allPossiblesString =
      allPossibleNQNs.map { _.toExtendedSyntax }.mkString(", ")
    val maybeLoc: Maybe[SchemaFileLocation] = Maybe.toMaybe(optTRD.map { _.schemaFileLocation })
    UnparseError(
      maybeLoc,
      Nope,
      "Found multiple matches for element %s because infoset implementation ignores namespaces. Matches are %s",
      sqnx,
      allPossiblesString,
    )
  }
}

/**
 * Base class for model group runtime data
 *
 * These Delay-type args are part of how we
 * create a structure here which contains some objects that
 * somewhere within themselves, refer back to this structure.
 */
sealed abstract class ModelGroupRuntimeData(
  positionArg: Int,
  partialNextElementResolverDelay: Delay[PartialNextElementResolver],
  val variableMap: VariableMap,
  val encInfo: EncodingRuntimeData,
  val schemaFileLocation: SchemaFileLocation,
  ci: DPathCompileInfo,
  val diagnosticDebugName: String,
  val path: String,
  defaultBitOrderArg: BitOrder,
  val groupMembers: Seq[TermRuntimeData],
  isRepresentedArg: Boolean,
  couldHaveText: Boolean,
  alignmentValueInBitsArg: Int,
  hasNoSkipRegionsArg: Boolean,
  optIgnoreCaseArg: Option[YesNo],
  fillByteEvArg: FillByteEv,
  maybeCheckByteAndBitOrderEvArg: Maybe[CheckByteAndBitOrderEv],
  maybeCheckBitOrderAndCharsetEvArg: Maybe[CheckBitOrderAndCharsetEv],
) extends TermRuntimeData(
    positionArg,
    partialNextElementResolverDelay,
    encInfo,
    ci,
    isRepresentedArg,
    couldHaveText,
    alignmentValueInBitsArg,
    hasNoSkipRegionsArg,
    defaultBitOrderArg,
    optIgnoreCaseArg,
    fillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg,
  ) {

  final override def isRequiredScalar = true
  final override def isArray = false

}

/**
 * These Delay-type args are part of how we
 * create a structure here which contains some objects that
 * somewhere within themselves, refer back to this structure.
 */
final class SequenceRuntimeData(
  positionArg: Int,
  partialNextElementResolverDelay: Delay[PartialNextElementResolver],
  variableMapArg: VariableMap,
  encInfo: EncodingRuntimeData,
  schemaFileLocationArg: SchemaFileLocation,
  ci: DPathCompileInfo,
  diagnosticDebugNameArg: String,
  pathArg: String,
  defaultBitOrderArg: BitOrder,
  groupMembersArg: Seq[TermRuntimeData],
  isRepresentedArg: Boolean,
  couldHaveText: Boolean,
  alignmentValueInBitsArg: Int,
  hasNoSkipRegionsArg: Boolean,
  optIgnoreCaseArg: Option[YesNo],
  fillByteEvArg: FillByteEv,
  maybeCheckByteAndBitOrderEvArg: Maybe[CheckByteAndBitOrderEv],
  maybeCheckBitOrderAndCharsetEvArg: Maybe[CheckBitOrderAndCharsetEv],
) extends ModelGroupRuntimeData(
    positionArg,
    partialNextElementResolverDelay,
    variableMapArg,
    encInfo,
    schemaFileLocationArg,
    ci,
    diagnosticDebugNameArg,
    pathArg,
    defaultBitOrderArg,
    groupMembersArg,
    isRepresentedArg,
    couldHaveText,
    alignmentValueInBitsArg,
    hasNoSkipRegionsArg,
    optIgnoreCaseArg,
    fillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg,
  )

/*
 * These Delay-type args are part of how we
 * create a structure here which contains some objects that
 * somewhere within themselves, refer back to this structure.
 */
final class ChoiceRuntimeData(
  positionArg: Int,
  partialNextElementResolverDelay: Delay[PartialNextElementResolver],
  variableMapArg: VariableMap,
  encInfo: EncodingRuntimeData,
  schemaFileLocationArg: SchemaFileLocation,
  ci: DPathCompileInfo,
  diagnosticDebugNameArg: String,
  pathArg: String,
  defaultBitOrderArg: BitOrder,
  groupMembersArg: Seq[TermRuntimeData],
  isRepresentedArg: Boolean,
  couldHaveText: Boolean,
  alignmentValueInBitsArg: Int,
  hasNoSkipRegionsArg: Boolean,
  optIgnoreCaseArg: Option[YesNo],
  fillByteEvArg: FillByteEv,
  maybeCheckByteAndBitOrderEvArg: Maybe[CheckByteAndBitOrderEv],
  maybeCheckBitOrderAndCharsetEvArg: Maybe[CheckBitOrderAndCharsetEv],
) extends ModelGroupRuntimeData(
    positionArg,
    partialNextElementResolverDelay,
    variableMapArg,
    encInfo,
    schemaFileLocationArg,
    ci,
    diagnosticDebugNameArg,
    pathArg,
    defaultBitOrderArg,
    groupMembersArg,
    isRepresentedArg,
    couldHaveText,
    alignmentValueInBitsArg,
    hasNoSkipRegionsArg,
    optIgnoreCaseArg,
    fillByteEvArg,
    maybeCheckByteAndBitOrderEvArg,
    maybeCheckBitOrderAndCharsetEvArg,
  )

final class VariableRuntimeData(
  schemaFileLocationArg: SchemaFileLocation,
  diagnosticDebugNameArg: String,
  pathArg: String,
  namespacesArg: NamespaceBinding,
  val external: Boolean,
  val direction: VariableDirection,
  maybeDefaultValueExprDelay: Delay[Maybe[CompiledExpression[AnyRef]]],
  val typeRef: RefQName,
  val globalQName: GlobalQName,
  val primType: NodeInfo.PrimType,
  unqualifiedPathStepPolicyArg: UnqualifiedPathStepPolicy,
) extends NonTermRuntimeData(
    null, // no variable map
    schemaFileLocationArg,
    diagnosticDebugNameArg,
    pathArg,
    namespacesArg,
    unqualifiedPathStepPolicyArg,
  ) {

  /**
   * Cyclic structures require initialization
   */
  lazy val initialize: Unit = {
    maybeDefaultValueExpr // demand this

  }

  lazy val maybeDefaultValueExpr: Maybe[CompiledExpression[AnyRef]] =
    maybeDefaultValueExprDelay.value

  def createVariableInstance(): VariableInstance = VariableInstance(rd = this)

}
