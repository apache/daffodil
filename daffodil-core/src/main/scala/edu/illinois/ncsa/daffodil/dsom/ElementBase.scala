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
import edu.illinois.ncsa.daffodil.grammar.ElementBaseGrammarMixin

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
  
  
  lazy val thisElementsNamespace: NS = this.namedQName.namespace
  lazy val thisElementsNamespacePrefix: String = this.namespaces.getPrefix(thisElementsNamespace.toString)

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
      thisElementsNamespace,
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
      thisElementsNamespacePrefix,
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
