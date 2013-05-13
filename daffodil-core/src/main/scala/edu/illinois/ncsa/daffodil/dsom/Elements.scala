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
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.LV
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

// A Particle is something that can be repeating.
trait ParticleMixin { self: ElementBase =>

  override lazy val isScalar = minOccurs == 1 && maxOccurs == 1
  lazy val isRecurring = !isScalar

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

  requiredEvaluations(typeDef, isSimpleType,
    if (hasPattern) patternValues,
    if (hasEnumeration) enumerationValues,
    if (hasMinLength) minLength,
    if (hasMaxLength) maxLength,
    if (hasMinInclusive) minInclusive,
    if (hasMaxInclusive) maxInclusive,
    if (hasMinExclusive) minExclusive,
    if (hasMaxExclusive) maxExclusive,
    if (hasTotalDigits) totalDigits,
    if (hasFractionDigits) fractionDigits)

  def name: String

  lazy val schemaComponentID = Infoset.addComponent(this)

  def inputValueCalcOption: PropertyLookupResult
  def isNillable: Boolean
  def isSimpleType: Boolean
  def isComplexType: Boolean
  def elementComplexType: ComplexTypeBase
  def elementSimpleType: SimpleTypeBase
  def typeDef: TypeBase
  def isScalar: Boolean

  def elementRef: Option[ElementRef]

  override lazy val isRepresented = inputValueCalcOption.isInstanceOf[NotFound]

  lazy val isScannable: Boolean = {
    representation match {
      case Representation.Text => {
        if (isSimpleType) true
        else elementComplexType.isScannable
      }
      case Representation.Binary => false
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

  def getImplicitAlignmentInBits(thePrimType: PrimType.Type, theRepresentation: Representation): Int = {
    theRepresentation match {
      case Representation.Text =>
        thePrimType match {
          case PrimType.HexBinary => schemaDefinitionError("Implicit Alignment is not allowed for HexBinary with representation='text'. Use representation='binary' instead.")
          case _ => knownEncodingAlignmentInBits
        }
      case Representation.Binary =>
        thePrimType match {
          case PrimType.String => schemaDefinitionError("Implicit Alignment is not allowed for String with representation='binary'. Use representation='text' instead.")
          case PrimType.Double | PrimType.Long | PrimType.ULong => 64
          case PrimType.Float | PrimType.Int | PrimType.UInt | PrimType.Boolean => 32
          case PrimType.Short | PrimType.UShort => 16
          case PrimType.Integer | PrimType.Decimal | PrimType.Byte | PrimType.UByte | PrimType.NonNegativeInteger => 8
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

  lazy val implicitAlignmentInBits: Int = getImplicitAlignmentInBits(primType.myPrimitiveType, representation)

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
          representation match {
            case Representation.Text => {
              if ((alignInBits % implicitAlignmentInBits) != 0)
                SDE("The given alignment (%s bits) must be a multiple of the encoding specified alignment (%s bits) for (%s) when representation='text'. Encoding: %s",
                  alignInBits, implicitAlignmentInBits, primType.myPrimitiveType, this.knownEncodingName)
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

  lazy val facetMaxLength = {
    if (hasSpecifiedLength) maxLength.longValue() else -1L
  }

  // if it is of simple type, then facets like length, maxLength, minLength are
  // attributes of the simple type def. You can't put them directly on an element.

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

  /**
   * everything that we need to look for when deciding how to terminate a data region
   * based on scanning
   */
  lazy val inScopeTerminatingMarkup = {
    // our own terminator is one thing
    // the separator of an enclosing group, if we're not last.
    // the terminator of an enclosing group, if we are last
    // the terminator of an enclosing element
    // recursively outward.
    //
    // or another way to think of it is
    // a sequence member has terminating markup, which is its separator for any item but the last, (last too if postfix), and the sequence terminator for the
    // last member. Plus any inscope terminating markup from what it is encapsulated in.
    // 
    // an element has its terminator
    //
    // Note: if we are potentially the last item (not required, but no downstream required siblings)
    Assert.notYetImplemented("inScopeTerminatingMarkup")
  }

  lazy val hasExpressionsInTerminatingMarkup: Boolean = {
    this.allTerminatingMarkup.filter { case (delimValue, _, _) => !delimValue.isConstant }.length > 0
  }

  // 11/1/2012 - moved to base since needed by patternValue
  lazy val isPrimitiveType = typeDef.isInstanceOf[PrimitiveType]

  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  lazy val hasPattern: Boolean = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasPattern
    } else { false }
  }
  lazy val hasEnumeration: Boolean = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasEnumeration
    } else { false }
  }

  lazy val hasMinLength = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinLength
    } else { false }
  }

  lazy val hasMaxLength = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxLength
    } else { false }
  }

  lazy val hasMinInclusive = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinInclusive
    } else { false }
  }

  lazy val hasMaxInclusive = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxInclusive
    } else { false }
  }

  lazy val hasMinExclusive = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinExclusive
    } else { false }
  }

  lazy val hasMaxExclusive = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxExclusive
    } else { false }
  }

  lazy val hasTotalDigits = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasTotalDigits
    } else { false }
  }

  lazy val hasFractionDigits = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasFractionDigits
    } else { false }
  }

  lazy val patternValues: Seq[FacetValueR] = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasPattern) {
        val pt = st.primitiveType.myPrimitiveType
        if (pt != PrimType.String) SDE("Pattern is only allowed to be applied to string and types derived from string.")
        st.patternValues
      } else SDE("Pattern was not found in this context.")
    } else SDE("Pattern was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val enumerationValues: String = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasEnumeration) st.enumerationValues
      else SDE("Enumeration was not found in this context.")
    } else SDE("Enumeration was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  //TODO: refactor minLength and maxLength calculation. 
  // 
  // It would be much clearer as a match-case 
  // on a pair of (hasMinLength, hasMaxLength), and should compute
  // lazy val (minLength: BigDecimal, maxLength: BigDecimal) = {
  //   schemaDefinitionUnless(isSimpleType && !isPrimitiveType && pt == PrimType.String || pt == PrimType.HexBinary), "Facets minLength and maxLength only apply to simpleTypes derived from xs:string or xs:hexBinary")
  //   (hasMinLength, hasMaxLength) match {
  //       case (true, true) if (isImplicitString) => // check for equal
  //       case (true, true) if (st.minLengthValue <= st.maxLengthValue) => (st.minLengthValue, st.maxLengthValue)
  //       case (false, true) if (isImplicitString) => (st.maxLengthValue, st.maxLengthValue)
  //       case (false, true) => (0, st.maxLengthValue)
  //       case (_, false) if (isImplicitString) => SDE("maxLength is required for implicit length strings.")
  //       case (false, false) => (0, -1) // -1 means unbounded right? 
  //       and so on.
  // Right now, it's much too hard to fix a bug in here.
  // I'd have to fix this in two places right now.
  //
  // This same comment applies below to many of these other facet pairs
  // which can be computed together instead of a separate lazy val for
  // each. 
  // 
  lazy val minLength: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
      // Facets cannot be applied to primitive types
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      val pt = st.primitiveType.myPrimitiveType
      val lk = this.lengthKind
      if (lk == LengthKind.Implicit && pt == PrimType.String) {
        // minLength and maxLength must be equal
        if (st.hasMinLength && st.hasMaxLength) {
          val res = st.minLengthValue.compareTo(st.maxLengthValue)
          if (res != 0) SDE("When LengthKind.Implicit for string, min and maxLength must be equal.")
        } else SDE("When LengthKind.Implicit for string, min and maxLength must be specified and equal.")
      }
      if (st.hasMinLength) {
        // May only apply to string/hexBinary
        if ((pt != PrimType.String) && (pt != PrimType.HexBinary)) SDE("MinLength facet can only be applied to string or hexBinary.")
        if (st.hasMaxLength) {
          val res = st.minLengthValue.compareTo(st.maxLengthValue)
          if (res > 0) SDE("MinLength facet must be <= MaxLength facet.")
        }
        st.minLengthValue
      } else SDE("MinLength was not found in this context.")

    } else SDE("MinLength was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val maxLength: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      val pt = st.primitiveType.myPrimitiveType
      val lk = this.lengthKind
      if (lk == LengthKind.Implicit && pt == PrimType.String) {
        // minLength and maxLength must be equal
        if (st.hasMinLength && st.hasMaxLength) {
          val res = st.minLengthValue.compareTo(st.maxLengthValue)
          if (res != 0) SDE("When LengthKind.Implicit for string, min and maxLength must be equal.")
        } else SDE("When LengthKind.Implicit for string, min and maxLength must be specified and equal.")
      }
      if (st.hasMaxLength) {
        // May only apply to string/hexBinary
        if ((pt != PrimType.String) && (pt != PrimType.HexBinary)) SDE("MaxLength facet can only be applied to string or hexBinary.")
        if (st.hasMinLength) {
          val res = st.minLengthValue.compareTo(st.maxLengthValue)
          if (res > 0) SDE("MinLength facet must be <= MaxLength facet.")
        }
        st.maxLengthValue
      } else SDE("MaxLength was not found in this context.")

    } else SDE("MaxLength was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  // TODO: see comment above about refactoring minLength, maxLength
  // Same thing applies to the other paired facets where there is lots of 
  // common logic associated with checking.
  lazy val minInclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
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
    } else SDE("MinInclusive was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val maxInclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
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
    } else SDE("MaxInclusive was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val minExclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
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
    } else SDE("MinExclusive was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val maxExclusive: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
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
    } else SDE("MaxExclusive was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val totalDigits: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasTotalDigits) {
        // Can only be applied to decimal or any of the integer types, and
        // types derived from them
        val isDerivedFromDecimal = st.isXDerivedFromY(st.primitiveType.name, "decimal")
        val isDerivedFromInteger = st.isXDerivedFromY(st.primitiveType.name, "integer")
        if (isDerivedFromDecimal || isDerivedFromInteger) st.totalDigitsValue
        else {
          SDE("TotalDigits facet can only be applied to decimal or any of the integer types, and types derived from them. Restriction base is %s", st.primitiveType.name)
        }
      } else SDE("TotalDigits was not found in this context.")
    } else SDE("TotalDigits was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val fractionDigits: java.math.BigDecimal = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasFractionDigits) {
        // Can only be applied to decimal
        val isDerivedFromDecimal = st.isXDerivedFromY(st.primitiveType.name, "decimal")
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
    } else SDE("FractionDigits was asked for when isSimpleType(%s) and isPrimitiveType(%s)", isSimpleType, isPrimitiveType)
  }

  lazy val allFacets: Seq[FacetValue] = {
    if (isSimpleType && !isPrimitiveType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.combinedBaseFacets
    } else scala.collection.mutable.Seq.empty
  }

  /**
   * Does the element have a default value?
   */
  def isDefaultable: Boolean

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
      case None => true
      case Some(s) =>
        if (s.groupMembers.last eq this) true // we want object identity comparison here, not equality. 
        else false
    }
  }

  lazy val isLastRequiredElementOfSequence: Boolean = Assert.notYetImplemented()

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
  with LocalElementMixin

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

  // Need to go get the Element we are referencing
  private[dsom] lazy val referencedElement = referencedElement_.value // optionReferencedElement.get
  private val referencedElement_ = LV('referencedElement) {
    this.schemaSet.getGlobalElementDecl(namespace, localName) match {
      case None => SDE("Referenced element not found: %s.", this.ref)
      case Some(x) => x.forElementRef(this)
    }
  }

  // These will just delegate to the referenced element declaration
  lazy val isNillable = referencedElement.isNillable
  lazy val isSimpleType = referencedElement.isSimpleType
  lazy val isComplexType = referencedElement.isComplexType
  lazy val elementComplexType = referencedElement.elementComplexType
  lazy val elementSimpleType = referencedElement.elementSimpleType
  lazy val isDefaultable: Boolean = referencedElement.isDefaultable

  lazy val qname = resolveQName(ref)
  lazy val (ns, localName) = qname
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
      case Some(tname) => Some(XMLUtils.QName(xml, tname, schemaDocument))
      case None => None
    }
  }

  lazy val namedTypeDef = namedTypeDef_.value
  private val namedTypeDef_ = LV('namedTypeDef) {
    namedTypeQName match {
      case None => None
      case Some((ns, localpart)) => {

        val ss = schemaSet
        val prim = ss.getPrimitiveType(localpart)
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

  lazy val elementComplexType = typeDef.asInstanceOf[ComplexTypeBase]
  lazy val elementSimpleType = typeDef.asInstanceOf[SimpleTypeBase]

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

  override lazy val enclosingComponent: Option[SchemaComponent] = elementRef

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

