package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.LV
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

// A Particle is something that can be repeating.
trait ParticleMixin { self: ElementBase =>

  override lazy val isScalar = minOccurs == 1 && maxOccurs == 1
  lazy val isRecurring = !isScalar

  override lazy val optMinOccurs: Option[Int] = Some(minOccurs)
  override lazy val optMaxOccurs: Option[Int] = Some(maxOccurs)

  /**
   * Determines if the element is optional, as in has zero or one instance only.
   *
   * There are two senses of optional
   * 1) Optional as in "might not be present" but for any reason.
   * Consistent with this is Required meaning must occur (but for any
   * reason. So all the occurrences of an array that has fixed number of
   * occurrences are required, and some of the occurrances of an array
   * that has a variable number of occurrences are optional.
   *
   * 2) Optional is in minOccurs="0" maxOccurs="1".
   *
   * Consistent with (2) is defining array as maxOccurs >= 2, and Required
   * as minOccurs=maxOccurs=1, but there are also special cases for occursCountKind parsed and stopValue
   * since they don't examine min/max occurs - they are only used for validation
   * in those occursCountKinds.
   *
   * The DFDL spec is not entirely consistent here either I don't believe.
   */
  lazy val isOptional = {
    // minOccurs == 0
    (optMinOccurs, optMaxOccurs) match {
      case (Some(1), Some(1)) => false // scalars are not optional
      case (Some(0), max) => {
        // now we must check on occursCountKind.
        // if parsed or stopValue then we consider it an array
        if (occursCountKind == OccursCountKind.Parsed ||
          occursCountKind == OccursCountKind.StopValue) {
          // we disregard the min/max occurs
          false
        } else {
          max match {
            case Some(1) => true
            case None => true
            case Some(_) => false
          }
        }
      }
      case _ => false
    }
  }

  override lazy val isArray = {
    // maxOccurs > 1 || maxOccurs == -1
    /**
     * Determines if the element is an array, as in can have more than one
     * instance.
     */
    if (isOptional) false
    else {
      val UNBOUNDED = -1
      (optMinOccurs, optMaxOccurs) match {
        case (None, None) => false
        case (Some(1), Some(1)) => false
        case (_, Some(n)) if n > 1 => true
        case (_, Some(UNBOUNDED)) => true
        /**
         * This next case is for occursCountKinds parsed and stopValue.
         * These only use min/maxOccurs for validation, so anything
         * with these occursCountKinds is an array (so long as it isn't
         * scalar)
         */
        case (_, Some(1)) if (occursCountKind == OccursCountKind.Parsed ||
          occursCountKind == OccursCountKind.StopValue) => true
        case _ => false
      }
    }
  }

  lazy val minOccurs = {
    val min = (self.xml \ "@minOccurs").text.toString
    min match {
      case "" => 1
      case _ => min.toInt
    }
  }

  lazy val maxOccurs = {
    val max = (self.xml \ "@maxOccurs").text.toString
    max match {
      case "unbounded" => -1
      case "" => 1
      case _ => max.toInt
    }
  }

  lazy val isFixedOccurrences = {
    // TODO optimizations to take scope into consideration. E.g.,
    // We could be in a context where the value of our occursCount expression
    // will always be a constant. 
    occursCountKind == OccursCountKind.Fixed
  }

  /**
   * Does this node have statically required instances.
   */
  lazy val hasStaticallyRequiredInstances = hasStaticallyRequiredInstances_.value
  private val hasStaticallyRequiredInstances_ = LV('hasStaticallyRequiredInstances) {
    val res =
      if (!isRepresented) false // if there's no rep, then it's not statically required.
      else if (isScalar) true
      else if (isFixedOccurrences) true
      else if (minOccurs > 0) true
      else false
    res
  }

  override lazy val isKnownRequiredElement = isKnownRequiredElement_.value
  private val isKnownRequiredElement_ = LV('isKnownRequiredElement) {
    if (isScalar) true
    else if (isFixedOccurrences) true
    else false
  }

  lazy val hasStopValue = hasStopValue_.value
  private val hasStopValue_ = LV('hasStopValue) {
    val sv = isRecurring && occursCountKind == OccursCountKind.StopValue
    // Don't check things like this aggressively. If we need occursStopValue then someone will ask for it.
    schemaDefinitionUnless(!(sv && occursStopValue == ""), "Property occursCountKind='stopValue' requires a non-empty occursStopValue property.")
    schemaDefinitionUnless(!sv, "occursCountKind='stopValue' is not implemented.")
    sv
  }
}

/**
 * Note about DSOM design versus say XSOM or Apache XSD library.
 *
 * Some XSD object models have a single Element class, and distinguish local/global and element references
 * based on attributes of the instances.
 *
 * Our approach is to provide common behaviors on base classes or traits/mixins, and to have distinct
 * classes for each instance type.
 */

/**
 * Shared by all forms of elements, local or global or element reference.
 */
abstract class ElementBase(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends Term(xmlArg, parent, position)
  with AnnotatedMixin
  with Element_AnnotationMixin
  with NillableMixin
  with DFDLStatementMixin
  with ElementBaseGrammarMixin
  with ElementRuntimeValuedPropertiesMixin
  with StringTextMixin
  with NumberTextMixin
  with CalendarTextMixin
  with BooleanTextMixin
  with TextNumberFormatMixin {

  requiredEvaluations(typeDef)
  requiredEvaluations(isSimpleType)
  requiredEvaluations(if (hasPattern) patternValues)
  requiredEvaluations(if (hasEnumeration) enumerationValues)
  requiredEvaluations(if (hasMinLength) minLength)
  requiredEvaluations(if (hasMaxLength) maxLength)
  requiredEvaluations(if (hasMinInclusive) minInclusive)
  requiredEvaluations(if (hasMaxInclusive) maxInclusive)
  requiredEvaluations(if (hasMinExclusive) minExclusive)
  requiredEvaluations(if (hasMaxExclusive) maxExclusive)
  requiredEvaluations(if (hasTotalDigits) totalDigits)
  requiredEvaluations(if (hasFractionDigits) fractionDigits)
  requiredEvaluations(runtimeData)

  def name: String

  def inputValueCalcOption: PropertyLookupResult
  def isNillable: Boolean
  def isSimpleType: Boolean
  def isComplexType: Boolean
  def elementComplexType: ComplexTypeBase
  def elementSimpleType: SimpleTypeBase
  def typeDef: TypeBase

  lazy val simpleType = {
    Assert.usage(isSimpleType)
    typeDef.asInstanceOf[SimpleTypeBase]
  }

  lazy val optPrimType: Option[PrimType] = Misc.boolToOpt(isSimpleType, primType) // .typeRuntimeData)

  def isScalar: Boolean
  def isOptional: Boolean

  // override in Particle
  lazy val optMinOccurs: Option[Int] = None
  lazy val optMaxOccurs: Option[Int] = None

  def elementRef: Option[ElementRef]

  override lazy val dpathCompileInfo = dpathElementCompileInfo

  lazy val dpathElementCompileInfo: DPathElementCompileInfo = {
    val eci = new DPathElementCompileInfo(
      enclosingElement.map { _.dpathElementCompileInfo },
      variableMap,
      namespaces,
      path,
      slotIndexInParent,
      name,
      isArray,
      namedQName,
      optPrimType,
      schemaFileLocation,
      elementChildrenCompileInfo)
    eci
  }

  override lazy val runtimeData: RuntimeData = elementRuntimeData
  override lazy val termRuntimeData: TermRuntimeData = elementRuntimeData

  lazy val defaultValue =
    if (isDefaultable) {
      val value = Infoset.convertToInfosetRepType(
        primType, // .typeRuntimeData, 
        defaultValueAsString, this)
      Some(value)
    } else None

  final lazy val elementRuntimeData: ElementRuntimeData = {
    val ee = enclosingElement
    val optERD = ee.map { _.elementRuntimeData }
    createElementRuntimeData(optERD)
  }

  final lazy val erd = elementRuntimeData

  /**
   * Everything needed at runtime about the element
   * in order to compile expressions using it, (for debug)
   * and issue proper diagnostics in error situations.
   */
  private def createElementRuntimeData(parent: => Option[ElementRuntimeData]): ElementRuntimeData = {

    //
    // I got sick of initialization time problems, so this mutual recursion
    // defines the tree of ERDs. 
    //
    // This works because of deferred arguments and lazy evaluation
    //
    lazy val childrenERDs: Seq[ElementRuntimeData] =
      elementChildren.map { _.elementRuntimeData }
    val newERD: ElementRuntimeData = new ElementRuntimeData(
      parent,
      childrenERDs,
      schemaSet.variableMap,
      dpathElementCompileInfo,
      schemaFileLocation,
      prettyName,
      path,
      namespaces,
      defaultBitOrder,
      optPrimType,
      targetNamespace,
      Misc.boolToOpt(hasPattern, patternValues),
      Misc.boolToOpt(hasEnumeration, enumerationValues),
      Misc.boolToOpt(hasMinLength, minLength),
      Misc.boolToOpt(hasMaxLength, maxLength),
      Misc.boolToOpt(hasMinInclusive, minInclusive),
      Misc.boolToOpt(hasMaxInclusive, maxInclusive),
      Misc.boolToOpt(hasMinExclusive, minExclusive),
      Misc.boolToOpt(hasMaxExclusive, maxExclusive),
      Misc.boolToOpt(hasTotalDigits, totalDigits),
      Misc.boolToOpt(hasFractionDigits, fractionDigits),
      optMinOccurs,
      optMaxOccurs,
      name,
      targetNamespacePrefix,
      isHidden,
      nChildSlots,
      slotIndexInParent,
      isNillable,
      defaultValue,
      isArray,
      isOptional,
      namedQName,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      impliedRepresentation)
    newERD
  }

  /**
   *  This QName should contain the prefix from the element reference
   */
  def namedQName: NamedQName

  /**
   * Each distinctly named child gets a slot.
   *
   * In the case where you have several elements with the same name and type,
   * but separated by other non-optional elements, then those are considered
   * to be different slots.
   *
   * Also, an xs:choice of elements, each child element gets its own slot. We
   * are not attempting to multiplex slots so that they are cleverly being shared.
   *
   * If a choice had many many disjoint children, then probably a map should be
   * used, not an array (as the infoset representation) so as to avoid wasting
   * all the slot space in these objects. This would stil be constant-time access,
   * just with a larger constant overhead. It's a time/space tradeoff.
   */
  lazy val nChildSlots: Int = {
    if (isSimpleType) 0
    else elementChildren.length
  }

  lazy val slotIndexInParent: Int = {
    if (!nearestEnclosingElement.isDefined) 0
    else {
      val elemParent = nearestEnclosingElementNotRef.get
      val realElement = this.referredToComponent // because we might be an ElementRef
      val realChildren = elemParent.elementChildren.map { _.referredToComponent }
      val pos = realChildren.indexOf(realElement)
      Assert.invariant(pos >= 0)
      pos
    }
  }

  /**
   * Direct element children of a complex element.
   *
   * Include both represented and non-represented elements.
   */
  lazy val elementChildren: Seq[ElementBase] = {
    this.typeDef match {
      case ct: ComplexTypeBase => {
        ct.modelGroup.group.elementChildren.asInstanceOf[Seq[ElementBase]]
      }
      case _ => Nil
    }
  }

  final lazy val elementChildrenCompileInfo = elementChildren.map { _.dpathElementCompileInfo }

  override lazy val isRepresented = inputValueCalcOption.isInstanceOf[NotFound]

  override lazy val impliedRepresentation = {
    val rep = if (isSimpleType) {
      primType match {
        case PrimType.HexBinary => Representation.Binary
        case PrimType.String => Representation.Text
        case _ => representation
      }
    } else {
      representation
    }
    rep
  }

  override lazy val couldHaveText: Boolean = {
    hasDelimiters ||
      (isSimpleType && impliedRepresentation == Representation.Text) ||
      (isComplexType && elementComplexType.modelGroup.couldHaveText)
  }

  override lazy val termChildren: Seq[Term] = {
    if (isSimpleType) Nil
    else Seq(elementComplexType.modelGroup.group)
  }

  lazy val isParentUnorderedSequence: Boolean = {
    parent match {
      case s: Sequence if !s.isOrdered => true
      case _ => false
    }
  }

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLElement(newDFDLAnnotationXML("element"), this)

  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLElement]

  def getImplicitAlignmentInBits(thePrimType: PrimType, theRepresentation: Representation): Int = {
    theRepresentation match {
      case Representation.Text =>
        thePrimType match {
          case PrimType.HexBinary => Assert.impossible("type xs:hexBinary with representation='text'")
          case _ => knownEncodingAlignmentInBits
        }
      case Representation.Binary =>
        thePrimType match {
          case PrimType.String => Assert.impossible("type xs:string with representation='binary'")
          case PrimType.Double | PrimType.Long | PrimType.UnsignedLong => 64
          case PrimType.Float | PrimType.Int | PrimType.UnsignedInt | PrimType.Boolean => 32
          case PrimType.Short | PrimType.UnsignedShort => 16
          case PrimType.Integer | PrimType.Decimal | PrimType.Byte | PrimType.UnsignedByte | PrimType.NonNegativeInteger => 8
          case PrimType.DateTime | PrimType.Date | PrimType.Time =>
            binaryCalendarRep match {
              case BinaryCalendarRep.BinaryMilliseconds => 64
              case BinaryCalendarRep.BinarySeconds => 32
              case _ => schemaDefinitionError("Implicit Alignment: binaryCalendarRep was %s but we expected BinarySeconds or BinaryMilliseconds.", binaryCalendarRep)
            }
          case PrimType.HexBinary => 8
        }
    }
  }

  lazy val implicitAlignmentInBits: Int = getImplicitAlignmentInBits(primType, impliedRepresentation)

  lazy val alignmentValueInBits: Int = {
    alignment match {
      case AlignmentType.Implicit => {
        if (this.isComplexType) {
          val ct = this.elementComplexType
          ct.alignmentValueInBits
        } else implicitAlignmentInBits
      }
      case align: Int => {
        val alignInBits = this.alignmentUnits match {
          case AlignmentUnits.Bits => align
          case AlignmentUnits.Bytes => 8 * align
        }
        if (this.isSimpleType) {
          impliedRepresentation match {
            case Representation.Text => {
              if (isRepresented && (alignInBits % implicitAlignmentInBits) != 0)
                SDE("The given alignment (%s bits) must be a multiple of the encoding specified alignment (%s bits) for %s when representation='text'. Encoding: %s",
                  alignInBits, implicitAlignmentInBits, primType.name, this.knownEncodingName)
            }
            case _ => /* Non textual data, no need to compare alignment to encoding's expected alignment */
          }
        }
        alignInBits
      }
    }
  }

  /**
   * Tells us if we have a specific length.
   *
   * Keep in mind that 80 characters in length can be anywhere from 80 to 320 bytes
   * depending on the character encoding. So fixed length doesn't mean in bytes.
   * it means in dfdl:lengthUnits units, which could be characters, and those can
   * be fixed or variable width.
   */
  lazy val isFixedLength = {
    lengthKind == LengthKind.Explicit && length.isConstant
  }

  lazy val hasSpecifiedLength = {
    isFixedLength ||
      lengthKind == LengthKind.Pattern ||
      lengthKind == LengthKind.Prefixed ||
      lengthKind == LengthKind.Implicit
  }

  lazy val fixedLength = {
    if (isFixedLength) length.constantAsLong else -1 // shouldn't even be asking for this if not isFixedLength 
  }

  /**
   * Nil Lit = literal nil, as opposed to value nil that uses a reserved value
   */
  lazy val isDefinedNilLit: Boolean = {
    val res = isNillable &&
      (nilKind == NilKind.LiteralValue ||
        nilKind == NilKind.LiteralCharacter)
    res
  }

  lazy val isDefinedNilValue: Boolean = {
    val res = (isNillable && nilKind == NilKind.LogicalValue)
    res
  }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */

  lazy val NVDP = NilValueDelimiterPolicy
  lazy val EVDP = EmptyValueDelimiterPolicy

  lazy val hasNilValueInitiator = initTermTestExpression(initiator, nilValueDelimiterPolicy, NVDP.Both, NVDP.Initiator)
  lazy val hasNilValueTerminator = initTermTestExpression(terminator, nilValueDelimiterPolicy, NVDP.Both, NVDP.Terminator)

  lazy val hasEmptyValueInitiator = initTermTestExpression(initiator, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Initiator)
  lazy val hasEmptyValueTerminator = initTermTestExpression(terminator, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Terminator)

  // See how this function takes the prop: => Any that is pass by name (aka lazy pass). 
  // That allows us to not require the property to exist at all if
  // expr.isKnownNotEmpty turns out to be false. 
  def initTermTestExpression(expr: CompiledExpression, prop: => Any, true1: Any, true2: Any): Boolean = {
    // changed from a match on a 2-tuple to if-then-else logic because we don't even want to ask for 
    // prop's value at all unless the first test is true.
    if (expr.isKnownNonEmpty)
      if (prop == true1 || prop == true2) true
      else false
    else false
  }

  /**
   * Means the element is in a context where there is a separator (from some enclosing sequence)
   * expected after it.
   *
   * Abstract here because implementations are different for local vs. global things.
   */
  def hasSep: Boolean

  /**
   * check if there are delimiters such that there is a concept of something that we can call 'empty'
   */
  lazy val emptyIsAnObservableConcept = emptyIsAnObservableConcept_.value
  private val emptyIsAnObservableConcept_ = LV('emptyIsAnObservableConcept) {
    val res = if ((hasSep ||
      hasEmptyValueInitiator ||
      hasEmptyValueTerminator) &&
      lengthKind != LengthKind.Implicit) {
      // fixed length things can't be empty (assuming static length 0 isn't allowed.) 
      false
    } else true
    res
  }

  //  /**
  //   * everything that we need to look for when deciding how to terminate a data region
  //   * based on scanning
  //   */
  //  lazy val inScopeTerminatingMarkup = {
  //    // our own terminator is one thing
  //    // the separator of an enclosing group, if we're not last.
  //    // the terminator of an enclosing group, if we are last
  //    // the terminator of an enclosing element
  //    // recursively outward.
  //    //
  //    // or another way to think of it is
  //    // a sequence member has terminating markup, which is its separator for any item but the last, (last too if postfix), and the sequence terminator for the
  //    // last member. Plus any inscope terminating markup from what it is encapsulated in.
  //    // 
  //    // an element has its terminator
  //    //
  //    // Note: if we are potentially the last item (not required, but no downstream required siblings)
  //    Assert.notYetImplemented("inScopeTerminatingMarkup")
  //  }

  lazy val hasExpressionsInTerminatingMarkup: Boolean = {
    this.allTerminatingMarkup.filter { case (delimValue, _, _) => !delimValue.isConstant }.length > 0
  }

  // 11/1/2012 - moved to base since needed by patternValue
  lazy val isPrimType = typeDef.isInstanceOf[PrimType]

  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  lazy val hasPattern: Boolean = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasPattern
    } else { false }
  }
  lazy val hasEnumeration: Boolean = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasEnumeration
    } else { false }
  }

  lazy val hasMinLength = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinLength
    } else { false }
  }

  lazy val hasMaxLength = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxLength
    } else { false }
  }

  lazy val hasMinInclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinInclusive
    } else { false }
  }

  lazy val hasMaxInclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxInclusive
    } else { false }
  }

  lazy val hasMinExclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinExclusive
    } else { false }
  }

  lazy val hasMaxExclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxExclusive
    } else { false }
  }

  lazy val hasTotalDigits = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasTotalDigits
    } else { false }
  }

  lazy val hasFractionDigits = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasFractionDigits
    } else { false }
  }

  lazy val patternValues: Seq[FacetValueR] = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasPattern) {
        val pt = st.primitiveType
        if (pt != PrimType.String) SDE("Pattern is only allowed to be applied to string and types derived from string.")
        st.patternValues
      } else SDE("Pattern was not found in this context.")
    } else SDE("Pattern was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  lazy val enumerationValues: String = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasEnumeration) st.enumerationValues
      else SDE("Enumeration was not found in this context.")
    } else SDE("Enumeration was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  /**
   * Compute minLength and maxLength together to share error-checking
   * and case dispatch that would otherwise have to be repeated.
   */
  lazy val (minLength: java.math.BigDecimal, maxLength: java.math.BigDecimal) = computeMinMaxLength
  // TODO: why are we using java.math.BigDecimal, when scala has a much 
  // nicer decimal class?
  val zeroBD = new java.math.BigDecimal(0)
  val unbBD = new java.math.BigDecimal(-1) // TODO: should this be a tunable limit?

  private def computeMinMaxLength: (java.math.BigDecimal, java.math.BigDecimal) = {
    schemaDefinitionUnless(isSimpleType, "Facets minLength and maxLength are allowed only on types string and hexBinary.")
    elementSimpleType match {
      case prim: PrimType => {
        schemaDefinitionWhen((prim == PrimType.String || prim == PrimType.HexBinary) && lengthKind == LengthKind.Implicit,
          "Facets minLength and maxLength must be defined for type %s with lengthKind='implicit'",
          prim.name)
        //
        // We handle text numbers by getting a stringValue first, then
        // we convert to the number type. 
        // 
        // This means we cannot check and SDE here on incorrect simple type.
        return (zeroBD, unbBD)
      }
      case st: SimpleTypeDefBase => {
        val pt = st.primitiveType
        val typeOK = pt == PrimType.String || pt == PrimType.HexBinary
        schemaDefinitionWhen(!typeOK && (hasMinLength || hasMaxLength),
          "Facets minLength and maxLength are not allowed on types derived from type %s.\nThey are allowed only on typed derived from string and hexBinary.",
          pt.name)
        val res = (hasMinLength, hasMaxLength, lengthKind) match {
          case (true, true, LengthKind.Implicit) => {
            schemaDefinitionUnless(
              st.minLengthValue.compareTo(st.maxLengthValue) == 0,
              "The minLength and maxLength must be equal for type %s with lengthKind='implicit'. Values were minLength of %s, maxLength of %s.",
              pt.name, st.minLengthValue, st.maxLengthValue)
            (st.minLengthValue, st.maxLengthValue)
          }
          case (true, true, _) => {
            schemaDefinitionWhen(
              st.minLengthValue.compareTo(st.maxLengthValue) > 0,
              // always true, so we don't bother to specify the type in the message.
              "The minLength facet value must be less than or equal to the maxLength facet value. Values were minLength of %s, maxLength of %s.",
              st.minLengthValue, st.maxLengthValue)
            (st.minLengthValue, st.maxLengthValue)
          }
          case (_, _, LengthKind.Implicit) => SDE("When lengthKind='implicit', both minLength and maxLength facets must be specified.")
          case (false, true, _) => (zeroBD, st.maxLengthValue)
          case (false, false, _) => (zeroBD, unbBD)
          case (true, false, _) => (st.minLengthValue, unbBD)
          case _ => Assert.impossible()
        }
        res
      }
      case _ => Assert.invariantFailed("should only be a PrimType or SimpleTypeDefBase")
    }
  }

  // TODO: see code above that computes minLength, maxLength
  // simultaneously to avoid redundant check code.
  // 
  // Same thing applies to the other paired facets where there is lots of 
  // common logic associated with checking.
  lazy val minInclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasMinInclusive && st.hasMinExclusive) SDE("MinInclusive and MinExclusive cannot be specified for the same simple type.")
      if (st.hasMinInclusive && st.hasMaxExclusive) {
        val res = st.minInclusiveValue.compareTo(st.maxExclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxExclusive(%s).", st.minInclusiveValue, st.maxExclusiveValue)
      }
      if (st.hasMinInclusive && st.hasMaxInclusive) {
        val res = st.minInclusiveValue.compareTo(st.maxInclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxInclusive(%s).", st.minInclusiveValue, st.maxInclusiveValue)
      }
      if (st.hasMinInclusive) st.minInclusiveValue
      else SDE("MinInclusive was not found in this context.")
    } else SDE("MinInclusive was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  lazy val maxInclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasMaxInclusive && st.hasMaxExclusive) SDE("MaxInclusive and MaxExclusive cannot be specified for the same simple type.")
      if (st.hasMaxInclusive && st.hasMinExclusive) {
        val res = st.minExclusiveValue.compareTo(st.maxInclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxInclusive(%s)", st.minExclusiveValue, st.maxInclusiveValue)
      }
      if (st.hasMaxInclusive && st.hasMinInclusive) {
        val res = st.minInclusiveValue.compareTo(st.maxInclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxInclusive(%s)", st.minInclusiveValue, st.maxInclusiveValue)
      }
      if (st.hasMaxInclusive) st.maxInclusiveValue
      else SDE("MaxInclusive was not found in this context.")
    } else SDE("MaxInclusive was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  lazy val minExclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasMinInclusive && st.hasMinExclusive) SDE("MinInclusive and MinExclusive cannot be specified for the same simple type.")
      if (st.hasMaxInclusive && st.hasMinExclusive) {
        val res = st.minExclusiveValue.compareTo(st.maxInclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxInclusive(%s)", st.minExclusiveValue, st.maxInclusiveValue)
      }
      if (st.hasMaxExclusive && st.hasMinExclusive) {
        val res = st.minExclusiveValue.compareTo(st.maxExclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxExclusive(%s)", st.minExclusiveValue, st.maxExclusiveValue)
      }
      if (st.hasMinExclusive) st.minExclusiveValue
      else SDE("MinExclusive was not found in this context.")
    } else SDE("MinExclusive was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  lazy val maxExclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasMaxExclusive && st.hasMaxInclusive) SDE("MaxExclusive and MaxInclusive cannot be specified for the same simple type.")
      if (st.hasMaxExclusive && st.hasMinInclusive) {
        val res = st.minInclusiveValue.compareTo(st.maxExclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxExclusive(%s)", st.minInclusiveValue, st.maxExclusiveValue)
      }
      if (st.hasMaxExclusive && st.hasMinExclusive) {
        val res = st.minExclusiveValue.compareTo(st.maxExclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxExclusive(%s)", st.minExclusiveValue, st.maxExclusiveValue)
      }
      if (st.hasMaxExclusive) st.maxExclusiveValue
      else SDE("MaxExclusive was not found in this context.")
    } else SDE("MaxExclusive was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  lazy val totalDigits: java.math.BigDecimal = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasTotalDigits) {
        // Can only be applied to decimal or any of the integer types, and
        // types derived from them
        val isDerivedFromDecimal = NodeInfo.isXDerivedFromY(st.primitiveType.name, "decimal")
        val isDerivedFromInteger = NodeInfo.isXDerivedFromY(st.primitiveType.name, "integer")
        if (isDerivedFromDecimal || isDerivedFromInteger) st.totalDigitsValue
        else {
          SDE("TotalDigits facet can only be applied to decimal or any of the integer types, and types derived from them. Restriction base is %s", st.primitiveType.name)
        }
      } else SDE("TotalDigits was not found in this context.")
    } else SDE("TotalDigits was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  lazy val fractionDigits: java.math.BigDecimal = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasFractionDigits) {
        // Can only be applied to decimal
        val isDerivedFromDecimal = NodeInfo.isXDerivedFromY(st.primitiveType.name, "decimal")
        if (isDerivedFromDecimal) {
          if (st.hasTotalDigits) {
            val res = st.fractionDigitsValue.compareTo(st.totalDigitsValue)
            if (res > 0) SDE("FractionDigits facet must not exceed TotalDigits.")
          }
          st.fractionDigitsValue
        } else {
          SDE("FractionDigits facet can only be applied to decimal. Restriction base is %s", st.primitiveType.name)
        }
      } else SDE("FractionDigits was not found in this context.")
    } else SDE("FractionDigits was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  lazy val allFacets: Seq[FacetValue] = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.combinedBaseFacets
    } else scala.collection.mutable.Seq.empty
  }

  /**
   * Does the element have a default value?
   */
  def isDefaultable: Boolean
  def defaultValueAsString: String

  /**
   * Combine our statements with those of the ref that is referencing us (if there is one), and
   * those of our simpleType (if we're a simple type element)
   *
   * The order here is important. The statements from type come first, then from declaration, then from
   * reference.
   */
  lazy val statements: Seq[DFDLStatement] =
    stForStatements.map { _.statements }.getOrElse(Nil) ++
      localStatements ++
      elementRef.map { _.statements }.getOrElse(Nil)

  lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    stForStatements.map { _.newVariableInstanceStatements }.getOrElse(Nil) ++
      localNewVariableInstanceStatements ++
      elementRef.map { _.newVariableInstanceStatements }.getOrElse(Nil)

  lazy val (discriminatorStatements, assertStatements) =
    checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)

  private lazy val combinedAsserts: Seq[DFDLAssert] =
    stForStatements.map { _.assertStatements }.getOrElse(Nil) ++
      localAssertStatements ++
      elementRef.map { _.assertStatements }.getOrElse(Nil)

  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] =
    stForStatements.map { _.discriminatorStatements }.getOrElse(Nil) ++
      localDiscriminatorStatements ++
      elementRef.map { _.discriminatorStatements }.getOrElse(Nil)

  lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs =
      stForStatements.map { _.setVariableStatements }.getOrElse(Nil) ++
        localSetVariableStatements ++
        elementRef.map { _.setVariableStatements }.getOrElse(Nil)
    checkDistinctVariableNames(combinedSvs)
  }

  private lazy val stForStatements = typeDef match {
    case st: SimpleTypeDefBase => Some(st)
    case _ => None
  }

}

/**
 * Common to local element decls and element references
 */
trait LocalElementMixin
  extends ParticleMixin
  with LocalComponentMixin
  with LocalElementGrammarMixin { self: LocalElementBase =>

  lazy val hasSep = hasSep_.value
  private val hasSep_ = LV('hasSep) {
    nearestEnclosingSequence match {
      case None => false
      case Some(es) => {
        val res =
          es.separator.isKnownNonEmpty
        res
      }
    }
  }

  lazy val isDeclaredLastInSequence = isDeclaredLastInSequence_.value
  private val isDeclaredLastInSequence_ = LV('isDeclaredLastInSequence) {
    val es = nearestEnclosingSequence
    // how do we determine what child node we are? We search. 
    // TODO: better structure for O(1) answer to this.
    es match {
      case None => Assert.invariantFailed("We are not in a sequence therefore isDeclaredLastInSequence is an invalid question.")
      case Some(s) => {
        val members = s.groupMembersNoRefs
        if (members.last eq thisTermNoRefs) true // we want object identity comparison here, not equality. 
        else false
      }
    }
  }

  lazy val lengthKnownToBeGreaterThanZero = lengthKnownToBeGreaterThanZero_.value
  private val lengthKnownToBeGreaterThanZero_ = LV('lengthKnownToBeGreaterThanZero) {
    val pt = {
      if (isPrimType) typeDef.asInstanceOf[PrimType]
      else {
        val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
        st.primitiveType
      }
    }
    val res = lengthKind match {
      case LengthKind.Explicit => (isFixedLength && (fixedLength > 0))
      case LengthKind.Prefixed => false
      case LengthKind.Pattern => lengthPattern.r.findFirstIn("") match {
        case None => true
        case Some(s) => false
      }
      case LengthKind.Delimited => (pt == PrimType.String)
      case LengthKind.Implicit => false
      case LengthKind.EndOfParent => false
    }
    res
  }

  override lazy val hasKnownRequiredSyntax = hasKnownRequiredSyntax_.value
  private val hasKnownRequiredSyntax_ = LV('hasKnownRequiredSyntax) {
    if ((minOccurs > 0) || isScalar || isFixedOccurrences) {
      if (emptyValueDelimiterPolicy == EmptyValueDelimiterPolicy.None) true
      else if (emptyIsAnObservableConcept) true
      else {
        val pt = {
          if (isPrimType) typeDef.asInstanceOf[PrimType]
          else {
            val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
            st.primitiveType
          }
        }
        ((pt == PrimType.String) || (pt == PrimType.HexBinary) && lengthKnownToBeGreaterThanZero)
      }
    } else false
  }

  lazy val isLastDeclaredRequiredElementOfSequence = isLastDeclaredRequiredElementOfSequence_.value
  private val isLastDeclaredRequiredElementOfSequence_ = LV('isLastDeclaredRequiredElementOfSequence) {
    if (hasKnownRequiredSyntax) {
      val es = nearestEnclosingSequence
      es match {
        case None => true
        case Some(s) =>
          if (s.groupMembers.filter(_.hasKnownRequiredSyntax).last eq this) true
          else false
      }
      // Since we can't determine at compile time, return true so that we can continue processing.
      // Runtime checks will make final determination.
    } else true
  }

  lazy val separatorSuppressionPolicy = separatorSuppressionPolicy_.value
  private val separatorSuppressionPolicy_ = LV('separatorSuppressionPolicy) {
    nearestEnclosingSequence match {
      case Some(ssp) => ssp.separatorSuppressionPolicy
      //
      // There is no enclosing sequence (could be just a big nest of choices I suppose)
      // In that case, there still has to be a value for this.
      //
      // Strictly speaking, it's a bug to require this property in a situation where there
      // are no sequences at all. However, that's so unlikely to be the case that it's 
      // not worth paying attention to. In any real format, the root element will be
      // a sequence.
      //
      case None => schemaDocument.separatorSuppressionPolicy
    }
  }
}

abstract class LocalElementBase(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ElementBase(xmlArg, parent, position)
  with LocalElementMixin {

  requiredEvaluations(checkForAlignmentAmbiguity)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private def lcm(a: Int, b: Int): Int = math.abs(a * b) / gcd(a, b)
  private def isXAMultipleOfY(x: Int, y: Int): Boolean = (x % y) == 0

  def isAlignmentCompatible(current: Int, next: Int): Boolean = {
    isXAMultipleOfY(current, next)
  }

  /**
   * Changed to a warning - DFDL WG decided to make this check optional, but it
   * is still useful as a warning.
   *
   * Turns out that MIL STD 2045 header format needs to pad out to a byte boundary
   * at the end of the structure. An optional, non-byte aligned field precedes
   * the end of the structure; hence, putting a zero-length byte-aligned field
   * at the end was crashing into this error. I couldn't think of a work-around,
   * so changed this into a warning.
   *
   * The old requirement was:
   *   To avoid ambiguity when parsing, optional elements and variable-occurrence arrays
   *   where the minimum number of occurrences is zero cannot have alignment properties
   *   different from the items that follow them. It is a schema definition error otherwise.
   *
   * Part of the required evaluations for LocalElementBase.
   */
  def checkForAlignmentAmbiguity: Unit = {
    if (isOptional) {
      this.couldBeNext.filterNot(m => m == thisTermNoRefs).foreach { that =>
        val isSame = this.alignmentValueInBits == that.alignmentValueInBits
        if (!isSame) {
          this.SDW("%s is an optional element or a variable-occurrence array and its alignment (%s) is not the same as %s's alignment (%s).",
            this.toString, this.alignmentValueInBits, that.toString, that.alignmentValueInBits)
        }
      }
    }
  }

  lazy val couldBeNext: Seq[Term] = couldBeNext_.value
  private val couldBeNext_ = LV('couldBeNext) {
    val es = this.nearestEnclosingSequence
    val eus = this.nearestEnclosingUnorderedSequenceBeforeSequence
    val ec = this.nearestEnclosingChoiceBeforeSequence

    val enclosingUnorderedGroup = {
      (ec, eus) match {
        case (None, None) => None
        case (Some(choice), _) => Some(choice)
        case (None, Some(uoSeq)) => Some(uoSeq)
      }
    }
    val listOfNextTerm = (enclosingUnorderedGroup, es) match {
      case (None, None) => Seq.empty
      case (Some(unorderedGroup), _) => {
        // We're in a choice or unordered sequence
        //
        // List must be all of our peers since (as well as our self)
        // we could be followed by any of them plus
        // whatever follows the unordered group.
        val peersCouldBeNext = unorderedGroup.groupMembersNoRefs

        val termsUntilFirstRequiredTerm = peersCouldBeNext ++ unorderedGroup.couldBeNext
        termsUntilFirstRequiredTerm
      }
      case (None, Some(oSeq)) => {
        // We're in an ordered sequence

        val termsUntilFirstRequiredTerm =
          isDeclaredLastInSequence match {
            case true => oSeq.couldBeNext
            case false => {

              val members = oSeq.groupMembersNoRefs

              val nextMember =
                members.dropWhile(m => m != thisTermNoRefs).filterNot(m => m == thisTermNoRefs).headOption

              val nextMembers =
                nextMember match {
                  case Some(e: LocalElementBase) if e.isOptional => Seq(e) ++ e.couldBeNext
                  case Some(e: LocalElementBase) => Seq(e)
                  case Some(gb: GroupBase) => Seq(gb.group)
                  case None => Assert.impossibleCase
                }
              nextMembers
            }
          }
        termsUntilFirstRequiredTerm
      }
    }
    listOfNextTerm
  }

}

/**
 * Note ElementRef isn't a first class citizen with the other schema components.
 * It gets bypassed in that most things here just delegate to the GlobalElementDecl
 * that this references.
 *
 * Most of the framework expects to be handling elements via the ElementBase abstract
 * class. That class is responsible for testing and reaching back over to an ElementRef.
 *
 * So for example, to find out if an element has a property, an Element has to consider
 * that the property might be expressed on an element ref (if there is one), the element
 * itself, or a simpleType def or a base simple type def. Element does this. ElementRef
 * doesn't.
 */
class ElementRef(xmlArg: Node, parent: ModelGroup, position: Int)
  extends LocalElementBase(xmlArg, parent, position)
  with ElementReferenceGrammarMixin
  with HasRefMixin
  with NamedMixin {

  requiredEvaluations(referencedElement)

  override def findPropertyOption(pname: String): PropertyLookupResult = {
    val res = referencedElement.findPropertyOption(pname)
    res
  }

  lazy val nonDefaultPropertySources = referencedElement.nonDefaultPropertySources
  lazy val defaultPropertySources = referencedElement.defaultPropertySources

  lazy val elementRef = None

  override lazy val referredToComponent = referencedElement

  override lazy val namedQName = referencedElement.namedQName

  // Need to go get the Element we are referencing
  private[dsom] lazy val referencedElement = referencedElement_.value // optionReferencedElement.get
  private val referencedElement_ = LV('referencedElement) {
    val ged = this.schemaSet.getGlobalElementDecl(namespace, localName)
    val res = ged match {
      case None => SDE("Referenced element not found: %s.", this.ref)
      case Some(x) => x.forElementRef(this)
    }
    res
  }

  // These will just delegate to the referenced element declaration
  lazy val isNillable = referencedElement.isNillable
  lazy val isSimpleType = referencedElement.isSimpleType
  lazy val isComplexType = referencedElement.isComplexType
  lazy val elementComplexType = referencedElement.elementComplexType
  lazy val elementSimpleType = referencedElement.elementSimpleType
  lazy val isDefaultable: Boolean = referencedElement.isDefaultable
  lazy val defaultValueAsString = referencedElement.defaultValueAsString

  lazy val (ns, localName) = {
    val qname = resolveQName(ref)
    qname
  }

  override lazy val namespace = ns

  /**
   * valueOrElse....not just .value because when trying to get a diagnostic message out about
   * something, but then you get another failure just trying to get the
   * name of the thing that was causing the original diagnostic, so you
   * end up getting a completely insrutable situation.
   *
   * So I made key things that are part of diagnostic messages have this
   * "always creates some value" behavior.
   *
   * Historic note:
   * I am hoping this problem will be less now. Some of it was because
   * we were failing validation, but then still running the rest of
   * the compiler which would then have errors it was not designed
   * to cope with like xs:element with no name or ref attribute.
   * Which would cause the above situation where just trying to get
   * the name was failing.
   */
  override lazy val name = nameFromRef
  private lazy val nameFromRef = nameFromRef_.valueOrElse("?name?")
  private val nameFromRef_ = LV('nameFromRef) { localName }

  // TODO: perhaps many members of ElementRef are unused. 
  // Consider removing some. Although consider that
  // some have to be here because of abstract bases or traits requiring them
  // even if they aren't called.
  lazy val typeDef = referencedElement.typeDef

  // Element references can have minOccurs and maxOccurs, and annotations, but nothing else.
  lazy val inputValueCalcOption = referencedElement.inputValueCalcOption // can't have ivc on element reference
  //  lazy val scalarDefaultable = referencedElement.scalarDefaultable
  //  lazy val scalarNonDefault = referencedElement.scalarNonDefault

  //TODO: refactor and use shared code for creating resolved set of annotations for an annotation point.
  override lazy val statements = localStatements
  override lazy val newVariableInstanceStatements = localNewVariableInstanceStatements
  override lazy val assertStatements = localAssertStatements
  override lazy val discriminatorStatements = localDiscriminatorStatements
  override lazy val setVariableStatements = localSetVariableStatements

}

/**
 * Shared by all element declarations local or global
 */
trait ElementDeclMixin
  extends ElementDeclGrammarMixin
  with OverlapCheckMixin { self: ElementBase =>

  lazy val eRefNonDefault: Option[ChainPropProvider] = elementRef.map { _.nonDefaultFormatChain }
  lazy val eRefDefault: Option[ChainPropProvider] = elementRef.map { _.defaultFormatChain }

  lazy val sTypeNonDefault: Seq[ChainPropProvider] = self.typeDef match {
    case st: SimpleTypeDefBase => st.nonDefaultPropertySources
    case _ => Seq()
  }
  lazy val sTypeDefault: Seq[ChainPropProvider] = self.typeDef match {
    case st: SimpleTypeDefBase => st.defaultPropertySources
    case _ => Seq()
  }

  /**
   * This and the partner defaultPropertySources are what ElementBase reaches back to get from
   * the ElementRef in order to have the complete picture of all the properties in effect for
   * that ElementBase.
   */
  lazy val nonDefaultPropertySources = nonDefaultPropertySources_.value
  private val nonDefaultPropertySources_ = LV('nonDefaultPropertySources) {
    val seq = (eRefNonDefault.toSeq ++ Seq(this.nonDefaultFormatChain) ++ sTypeNonDefault).distinct
    checkNonOverlap(seq)
    seq
  }

  lazy val defaultPropertySources = defaultPropertySources_.value
  private val defaultPropertySources_ = LV('defaultPropertySources) {
    val seq = (eRefDefault.toSeq ++ Seq(this.defaultFormatChain) ++ sTypeDefault).distinct
    seq
  }

  override def prettyName = "element." + name

  lazy val immediateType = immediateType_.value
  private val immediateType_ = LV('immediateType) {
    val st = xml \ "simpleType"
    val ct = xml \ "complexType"
    val nt = typeName
    if (st.length == 1)
      Some(new LocalSimpleTypeDef(st(0), self))
    else if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), self))
    else {
      Assert.invariant(nt != "")
      None
    }
  }

  lazy val typeName = getAttributeOption("type")

  lazy val namedTypeQName = namedTypeQName_.value
  private val namedTypeQName_ = LV('namedTypeQName) {
    typeName match {
      case Some(tname) => QName.resolveRef(tname, namespaces)
      case None => None
    }
  }

  lazy val namedTypeDef = namedTypeDef_.value
  private val namedTypeDef_ = LV('namedTypeDef) {
    namedTypeQName match {
      case None => None
      case Some(RefQName(_, localpart, ns)) => {

        val ss = schemaSet
        val prim = ss.getPrimType(ns, localpart)
        //
        if (prim != None) prim
        else {
          val gstd = ss.getGlobalSimpleTypeDef(ns, localpart)
          val gctd = ss.getGlobalComplexTypeDef(ns, localpart)
          val res = (gstd, gctd) match {
            case (Some(gstdFactory), None) => Some(gstdFactory.forElement(this))
            case (None, Some(gctdFactory)) => Some(gctdFactory.forElement(this))
            // Note: Validation of the DFDL Schema doesn't necessarily check referential integrity
            // or other complex constraints like conflicting names.
            // So we check it here explicitly.
            case (None, None) => schemaDefinitionError("No type definition found for '%s' (%s).", typeName.get, ns)
            case (Some(_), Some(_)) => schemaDefinitionError("Both a simple and a complex type definition found for " + typeName.get + ".")
          }
          res
        }
      }
    }
  }

  lazy val typeDef = typeDef_.value
  private val typeDef_ = LV('typeDef) {
    (immediateType, namedTypeDef) match {
      case (Some(ty), None) => ty
      case (None, Some(ty)) => ty
      // Note: Schema validation should find this for us, but referential integrity checks like this
      // might not be done, so we check explicitly for this.
      case _ => SDE("Must have one of an immediate type or a named type but not both")
    }
  }

  lazy val isSimpleType = isSimpleType_.value
  private val isSimpleType_ = LV('isSimpleType) {
    typeDef match {
      case _: SimpleTypeBase => true
      case _: ComplexTypeBase => false
      case _ => Assert.invariantFailed("Must be either SimpleType or ComplexType")
    }
  }

  lazy val isComplexType = !isSimpleType

  lazy val defaultValueAsString = (xml \ "@default").text

  lazy val hasDefaultValue: Boolean = defaultValueAsString != ""

  lazy val isNillable = (xml \ "@nillable").text == "true"

  lazy val elementComplexType = {
    Assert.usage(isComplexType)
    typeDef.asInstanceOf[ComplexTypeBase]
  }

  lazy val elementSimpleType = {
    Assert.usage(isSimpleType)
    typeDef.asInstanceOf[SimpleTypeBase]
  }

  /**
   * We require that there be a concept of empty if we're going to be able to default something
   * and we are going to require that we can tell this statically. I.e., we're not going to defer this to runtime
   * just in case the delimiters are being determined at runtime.
   *
   * That is to say, if a delimiter is an expression, then we're assuming that means
   * at runtime it will not evaluate to empty string (so you can specify the delimiter
   * at runtime, but you cannot turn on/off the whole delimited format at runtime.)
   */

  lazy val isDefaultable = isDefaultable_.value
  private val isDefaultable_ = LV('isDefaultable) {
    defaultValueAsString match {
      case "" => false // allowed for type string.
      case _ if (emptyIsAnObservableConcept) => true
      case _ => false
    }
  }
}

class LocalElementDecl(xmlArg: Node, parent: ModelGroup, position: Int)
  extends LocalElementBase(xmlArg, parent, position)
  with ElementFormDefaultMixin
  with ElementDeclMixin {
  lazy val elementRef = None

  requiredEvaluations(minOccurs, maxOccurs)

  override def namedQName = {
    val isQualified = elementFormDefault == "qualified"
    QName.createLocal(name, targetNamespace, isQualified, namespaces)
  }
}

/**
 * Factory to create an instance of a global element declaration
 * either to be the root of the data, or when referenced from an
 * element reference, in which case a backpointer from the global element decl
 * instance will point back to the element reference.
 *
 * This backpointer is needed in order to determine some attributes that refer
 * outward to what something is contained within. E.g., nearestEnclosingSequence
 * is an attribute that might be the sequence containing the element reference
 * that is referencing this global element declaration.
 */
class GlobalElementDeclFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponent(xmlArg, schemaDocumentArg) with NamedMixin {
  def forRoot() = asRoot // cache. Not a new one every time.
  lazy val asRoot = new GlobalElementDecl(xml, schemaDocument, None)

  def forElementRef(eRef: ElementRef) = new GlobalElementDecl(xml, schemaDocument, Some(eRef))

}

/**
 * A global element decl uses LocalElementBase because it behaves like a local
 * element when you consider that except for the root case, it has to be combined
 * with an ElementRef that references it. The ElementRef can carry the things
 * like min/maxOccurs and such that aren't allowed on a GlobalElementDecl. The
 * combination of an ElementRef plus its GlobalElementDecl behaves like a LocalElementDecl.
 */
class GlobalElementDecl(xmlArg: Node, schemaDocumentArg: SchemaDocument, val elementRef: Option[ElementRef])
  extends LocalElementBase(xmlArg, schemaDocumentArg, 0)
  with GlobalComponentMixin
  with ElementDeclMixin
  with GlobalElementDeclGrammarMixin {

  requiredEvaluations(document)

  override lazy val maxOccurs = elementRef match {
    case Some(er) => er.maxOccurs
    case None => 1
  }

  override lazy val minOccurs = elementRef match {
    case Some(er) => er.minOccurs
    case None => 1
  }

  lazy val isRoot = elementRef == None

  override lazy val isHidden = if (isRoot) false else elementRef.get.isHidden

  override lazy val enclosingComponent = elementRef.flatMap { _.enclosingComponent }

  override lazy val referringComponent: Option[SchemaComponent] = elementRef

  // GlobalElementDecls need to have access to elementRef's local properties.

  // We inherit the requirement for these attributes from Term. It all gets
  // too complicated in DSOM if you try to make GlobalElementDecl share with the other
  // element structures but not be a Term.
  //
  // But a GlobalElementDecl isn't really a Term except in a degenerate sense
  // that the root element is sort of a Term.
  //
  // In other words, we shouldn't be treating this as a term.
  //

  /**
   * global elements combined with element references referring to them can
   * be multiple occurring (aka arrays) hence, we have to have things
   * that take root and referenced situation into account.
   */
  override lazy val isScalar = minOccurs == 1 && maxOccurs == 1

}

