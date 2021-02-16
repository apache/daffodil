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

import org.apache.daffodil.equality._
import org.apache.daffodil.processors._
import org.apache.daffodil.schema.annotation.props._
import org.apache.daffodil.xml._
import org.apache.daffodil.grammar.ElementBaseGrammarMixin
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.util.Misc

import scala.xml.NamespaceBinding
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.api.WarnID
import java.lang.{ Integer => JInt }

import org.apache.daffodil.dsom.walker.ElementBaseView
import org.apache.daffodil.infoset.DataValue
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveOrUseNilForDefaultOrNull

/**
 * Note about DSOM design versus say XSOM or Apache XSD library.
 *
 * Some XSD object models have a single Element class, and distinguish local/global and element references
 * based on attributes of the instances.
 *
 * Our approach is to provide common behaviors on base classes or traits/mixins, and to have distinct
 * classes for each instance type.
 *
 * This base is shared by all forms of elements, local or global or element reference.
 */
trait ElementBase
  extends Term
  with ElementLikeMixin
  with LocalElementMixin
  with Element_AnnotationMixin
  with NillableMixin
  with ElementBaseGrammarMixin
  with ElementRuntimeValuedPropertiesMixin
  with StringTextMixin
  with NumberTextMixin
  with CalendarTextMixin
  with BooleanTextMixin
  with TextNumberFormatMixin
  with EmptyElementParsePolicyMixin
  with TextStandardBaseMixin
  with OverlapCheckMixin
  with ElementBaseView {

  override final def eBase = this

  lazy val init: Unit = {
    schemaSet.elementBaseInstanceCount += 1
  }

  requiredEvaluationsIfActivated(init)
  requiredEvaluationsIfActivated(typeDef)
  requiredEvaluationsIfActivated(isSimpleType)
  requiredEvaluationsIfActivated(if (hasPattern) patternValues)
  requiredEvaluationsIfActivated(if (hasEnumeration) enumerationValues)
  requiredEvaluationsIfActivated(if (hasMinLength) minLength)
  requiredEvaluationsIfActivated(if (hasMaxLength) maxLength)
  requiredEvaluationsIfActivated(if (hasMinInclusive) minInclusive)
  requiredEvaluationsIfActivated(if (hasMaxInclusive) maxInclusive)
  requiredEvaluationsIfActivated(if (hasMinExclusive) minExclusive)
  requiredEvaluationsIfActivated(if (hasMaxExclusive) maxExclusive)
  requiredEvaluationsIfActivated(if (hasTotalDigits) totalDigits)
  requiredEvaluationsIfActivated(if (hasFractionDigits) fractionDigits)
  requiredEvaluationsIfActivated(checkForAlignmentAmbiguity)
  requiredEvaluationsIfActivated(checkFloating)

  override def name: String

  final lazy val inputValueCalcOption = findPropertyOption("inputValueCalc", expressionAllowed = true)

  final lazy val outputValueCalcOption = {
    val optOVC = findPropertyOption("outputValueCalc", expressionAllowed = true)
    schemaDefinitionWhen(optOVC.isDefined && isOptional, "dfdl:outputValueCalc cannot be defined on optional elements.")
    schemaDefinitionWhen(optOVC.isDefined && isArray, "dfdl:outputValueCalc cannot be defined on array elements.")
    schemaDefinitionWhen(optOVC.isDefined && isComplexType, "dfdl:outputValueCalc cannot be defined on complexType elements.")
    // This should be an SDE, but is very useful for unit tests to be able to specify OVC on a single global element
    // self.schemaDefinitionWhen(optOVC.isDefined && self.isInstanceOf[GlobalElementDecl], "dfdl:outputValueCalc cannot be defined on global elements.")

    optOVC
  }

  def isNillable: Boolean
  def isSimpleType: Boolean
  def isComplexType: Boolean

  def simpleType: SimpleTypeBase

  def complexType: ComplexTypeBase

  def optSimpleType: Option[SimpleTypeBase]
  def optComplexType: Option[ComplexTypeBase]

  /**
   * Irrespective of whether the type of this element is immediate or
   * primitive, or reached via a type reference, this is the typeDef
   * of the type.
   */
  def typeDef: TypeBase

  /**
   * The DPathElementInfo objects referenced within an IVC
   * that calls dfdl:contentLength( thingy )
   */
  override final protected lazy val calcContentParserReferencedElementInfos =
    if (this.inputValueCalcOption.isDefined)
      ivcCompiledExpression.contentReferencedElementInfos
    else
      ReferencedElementInfos.None

  /**
   * The DPathElementInfo objects referenced within an OVC
   * that calls dfdl:contentLength( thingy )
   */
  override final protected lazy val calcContentUnparserReferencedElementInfos =
    if (this.outputValueCalcOption.isDefined)
      ovcCompiledExpression.contentReferencedElementInfos
    else
      ReferencedElementInfos.None

  /**
   * The DPathElementInfo objects referenced within an IVC
   * that calls dfdl:valueLength( thingy )
   */
  override final protected lazy val calcValueParserReferencedElementInfos =
    if (this.inputValueCalcOption.isDefined) {
      val ivcCE = ivcCompiledExpression
      val setERs = ivcCE.valueReferencedElementInfos
      setERs
    } else
      ReferencedElementInfos.None

  /**
   * The DPathElementInfo objects referenced within an IVC
   * that calls dfdl:valueLength( thingy )
   */
  override final protected lazy val calcValueUnparserReferencedElementInfos =
    if (this.outputValueCalcOption.isDefined)
      ovcCompiledExpression.valueReferencedElementInfos
    else
      ReferencedElementInfos.None

  /**
   *  Tells us if, for this element, we need to capture its content length
   *  at parse runtime, or we can ignore that.
   */
  final lazy val shouldCaptureParseContentLength: Boolean = {
    val isReferenced =
      if (this.isInstanceOf[PrefixLengthQuasiElementDecl]) false
      else {
        val setElems = schemaSet.root.contentLengthParserReferencedElementInfos
        setElems.contains(this.dpathElementCompileInfo)
      }
    isReferenced
  }

  /**
   * None for complex types, Some(primType) for simple types.
   */
  final lazy val optPrimType: Option[PrimType] = Misc.boolToOpt(isSimpleType, primType) // .typeRuntimeData)

  /**
   * An array element is required if its index is less than the minOccurs of the
   * array. For an array with a fixed number of elements, all elements are required.
   */
  def isArrayWithAtLeastOneRequiredArrayElement: Boolean

  private def nsBindingsToSet(nsb: NamespaceBinding): Set[(String, NS)] = {
    if (nsb == scala.xml.TopScope) Set()
    else {
      val parentBindings = nsBindingsToSet(nsb.parent)
      val res = parentBindings.+((nsb.prefix, NS(nsb.uri)))
      res
    }
  }

  private lazy val thisElementsRequiredNamespaceBindings: Set[(String, NS)] = LV('thisElementsRequiredNamespaceBindings) {
    val childrenRequiredNSBindings =
      this.elementChildren.flatMap { _.thisElementsRequiredNamespaceBindings }.toSet

    val myRequiredNSBinding = Set((namedQName.prefixOrNull, namedQName.namespace))
    val nilNSBinding = {
      if (!isNillable) Set()
      else {
        //
        // Nillable, so we need a binding for xsi:nil='true' case.
        //
        val xsiNS = XMLUtils.XSI_NAMESPACE
        val xsiPrefix = namespaces.getPrefix(xsiNS.toString)
        if (xsiPrefix != null) {
          Set((xsiPrefix, xsiNS))
        } else {
          Set(("xsi", xsiNS))
        }
      }
    }
    val allBindings = childrenRequiredNSBindings ++ myRequiredNSBinding ++ nilNSBinding

    // allBindings now contains bindings for this element and its children. The
    // purpose of this is to kindof bubble up required namespace bindings. For
    // example, we need the xsi namespace if an element is nillable, but we do
    // not want to redefine that namespace on every nillable element. By adding
    // the required namespaces of the children, we can bubble up such bindings
    // so that they are only declared once at the top of an infoset. However,
    // there is an issues with bubbling up. That is, children could have
    // conflicting namespace bindings either with each other or with this
    // element. So after all namespace bindings are combined, we need to filter
    // out conflicting namespace bindings.
    //
    // Note however, that if myRequiredNSBinding conflicts with a child, then it
    // (along with anything it conflicts with) would be filtered out. But it is
    // required for this element, so we need to add it back in.

    // Creates a Map[prefix, Set[NS]]. Duplicate NS's will be removed from the
    // Set, since it's a Set
    val bindingsGroupedByPrefix = allBindings.groupBy { _._1 }.mapValues { _.map { _._2 } }

    // Any Set with size > 1 has different namespaces for the same prefix, filter them out
    val bindingsNoConflictsMap = bindingsGroupedByPrefix.filter { case (prefix, bindings) => bindings.size == 1 }

    // Create a Map[prefix, NS] now that conflicts are removed
    val bindingsSingleNSMap = bindingsNoConflictsMap.mapValues { _.head }

    // Convert back to a set
    val bindings = bindingsSingleNSMap.toSet

    // Add back in myRequiredNSBinding. This is a Set, so if it already exist
    // the duplicate will just be ignored
    val res = bindings ++ myRequiredNSBinding

    res
  }.value

  private lazy val emptyNSPairs: Set[(String, NS)] = nsBindingsToSet(scala.xml.TopScope)

  private lazy val myOwnNSPairs: Set[(String, NS)] = thisElementsRequiredNamespaceBindings

  private lazy val myParentNSPairs: Set[(String, NS)] = LV('myParentNSPairs) {
    val ee: Option[ElementBase] = enclosingElements.headOption // FIXME: DAFFODIL-2282 works only if there is no difference among usages.
    ee match {
      case None => emptyNSPairs
      case Some(ee) => ee.myOwnNSPairs
    }
  }.value

  private lazy val myUniquePairs: Set[(String, NS)] = {
    val res = myOwnNSPairs -- myParentNSPairs // FIXME: DAFFODIL-2282 works only if there is no difference among usages.
    res
  }

  // FIXME: DAFFODIL-2282 works only if there is no difference among usages.
  private def pairsToNSBinding(pairs: Set[(String, NS)], parentNS: NamespaceBinding): NamespaceBinding = {
    if (pairs.isEmpty) parentNS
    else {
      val (pre, ns) = pairs.head
      val t = pairs.tail
      val parentNSBinding = pairsToNSBinding(t, parentNS)
      val uri = if (ns.optURI.isDefined) ns.optURI.get.toString else null
      val res = NamespaceBinding(pre, uri, parentNSBinding)
      res
    }
  }

  private lazy val parentMinimizedScope = {
    val ee = enclosingElements.headOption // FIXME: bug DAFFODIL-2282 doesn't work unless all are same.
    ee.map { _.minimizedScope }.getOrElse(scala.xml.TopScope)
  }

  /**
   * To be properly constructed, scala's xml Elems must share the scope (namespace bindings) of the enclosing
   * parent element, except when it adds more of its own bindings, in which case the tail is supposed to be shared.
   */
  final protected lazy val minimizedScope: NamespaceBinding = LV('minimizedScope) {
    val uniquePairs =
      if (this.isInstanceOf[Root]) {
        // If this is the root element and it contains xmlns="", then remove
        // it. xmlns="" is implied at the root. Note that we only do this for
        // the root because if a child has xmlns="", that implies the parent
        // had set the default namespace to something else, so we need to keep
        // the child xmlns="" to override that.
        myUniquePairs.filterNot {
          case (prefix, ns) =>
            val res =
              prefix == null &&
                ns.isNoNamespace
            res
        }
      } else {
        myUniquePairs
      }

    pairsToNSBinding(uniquePairs, parentMinimizedScope)
  }.value

  /**
   * Is either None, Some(primTypeValue) or Some(UseNilForDefault), which is a
   * singleton object indicating that the item is nillable, and useNilForDefault was true.
   *
   * The value will always be of the matching primitive types for the element, and
   * directly usable as the value of a simple-type element.
   *
   * When a value is used, it is created from the XSD default or fixed attribute of the
   * element declaration, and that string cannot contain DFDL entities of any kind,
   * nor any PUA-remapped characters. This insures the default/fixed value can still be
   * used for ordinary XML-schema validation outside of Daffodil/DFDL.
   */
  final lazy val defaultValue: DataValuePrimitiveOrUseNilForDefaultOrNull = {
    if (isDefaultable && (isScalar || isArrayWithAtLeastOneRequiredArrayElement)) {
      val dv =
        if (isNillable && useNilForDefault =:= YesNo.Yes) {
          DataValue.UseNilForDefault
        } else {
          //
          // Note: no remapping PUA chars or otherwise messing with the text of the default value
          // because this must be a regular XSD default value so that Xerces validation
          // will work.
          //
          val str = defaultValueAsString
          val value = try {
            primType.fromXMLString(str)
          } catch {
            case ipd: InvalidPrimitiveDataException =>
              SDE("Invalid default value: %s", ipd.getMessage)
          }
          value
        }
      dv
    } else DataValue.NoValue
  }

  lazy val unparserInfosetElementDefaultingBehavior: UnparserInfo.InfosetEventBehavior = {
    import UnparserInfo._
    if (!isRepresented) MustExist
    else if (isOutputValueCalc) Computed
    else if (isOptional) Optional
    else if (isArray && !isArrayWithAtLeastOneRequiredArrayElement) Optional
    else MustExist
  }

  lazy val canBeAbsentFromUnparseInfoset: Boolean = {
    import UnparserInfo._
    unparserInfosetElementDefaultingBehavior !=:= MustExist
  }

  lazy val isQuasiElement: Boolean = false //overriden by RepTypeQuasiElementDecl

  final protected lazy val optTruncateSpecifiedLengthString =
    Option(truncateSpecifiedLengthString =:= YesNo.Yes)
  // because of the way text numbers are unparsed, we don't know that
  // the string is for a text number. So we need this property for numbers and other text
  // simple types also.
  //    if (isSimpleType && simpleType.primType.isInstanceOf[NodeInfo.String.Kind]) {
  //      Option(truncateSpecifiedLengthString =:= YesNo.Yes)
  //    } else None // don't need this property for non-strings

  /**
   *  This QName should contain the prefix from the element reference
   */
  def namedQName: NamedQName

  /**
   * Direct element children of a complex element.
   *
   * Include both represented and non-represented elements.
   */
  final lazy val elementChildren: Seq[ElementBase] = LV('elementChildren) {
    this.typeDef match {
      case ct: ComplexTypeBase => {
        ct.group.elementChildren
      }
      case _ => Nil
    }
  }.value

  final lazy val elementChildrenCompileInfo =
    elementChildren.map {
      _.dpathElementCompileInfo
    }

  final lazy val isOutputValueCalc = outputValueCalcOption.isInstanceOf[Found]

  final override lazy val impliedRepresentation = {
    val rep = if (isSimpleType) {
      primType match {
        case PrimType.HexBinary => Representation.Binary
        case PrimType.String => Representation.Text
        case PrimType.AnyURI => objectKind match {
          case ObjectKindType.Bytes => Representation.Binary
          case ObjectKindType.Chars => Representation.Text
        }
        case _ => representation
      }
    } else {
      representation
    }
    rep match {
      case Representation.Binary =>
        if (isComplexType || (primType != PrimType.HexBinary && primType != PrimType.AnyURI)) byteOrderEv // ensure defined
      case _ =>
        encodingRaw // ensure defined
    }
    rep
  }

  final override lazy val couldHaveText: Boolean = {
    hasDelimiters ||
      (isSimpleType && impliedRepresentation == Representation.Text) ||
      (isComplexType && complexType.group.couldHaveText)
  }

  final override lazy val termChildren: Seq[Term] = {
    if (isSimpleType) Nil
    else
      Seq(complexType.group)
  }

  final lazy val isParentUnorderedSequence: Boolean = {
    optLexicalParent.exists { lp =>
      lp match {
        case s: SequenceTermBase if !s.isOrdered => true
        case _ => false
      }
    }
  }

  private def getImplicitAlignmentInBits(thePrimType: PrimType, theRepresentation: Representation): Int = {
    (theRepresentation, thePrimType) match {
      case (Representation.Text, PrimType.AnyURI) => this.subsetError("Property value objectKind='chars' is not supported.")
      case (Representation.Text, PrimType.HexBinary) => Assert.impossible("type xs:hexBinary with representation='text'")
      case (Representation.Text, _) => knownEncodingAlignmentInBits
      case (Representation.Binary, PrimType.String) => Assert.impossible("type xs:string with representation='binary'")
      // Boolean, Float, Double, and HexBinary do not require binaryNumberRep to be defined
      case (Representation.Binary, PrimType.Float | PrimType.Boolean) => 32
      case (Representation.Binary, PrimType.Double) => 64
      case (Representation.Binary, PrimType.HexBinary) => 8
      case (Representation.Binary, PrimType.AnyURI) => 8
      // Handle 64 bit types
      case (Representation.Binary, PrimType.Long | PrimType.UnsignedLong) =>
        binaryNumberRep match {
          case BinaryNumberRep.Packed | BinaryNumberRep.Bcd | BinaryNumberRep.Ibm4690Packed => 8
          case _ => 64
        }
      // Handle 32 bit types
      case (Representation.Binary, PrimType.Int | PrimType.UnsignedInt | PrimType.Boolean) =>
        binaryNumberRep match {
          case BinaryNumberRep.Packed | BinaryNumberRep.Bcd | BinaryNumberRep.Ibm4690Packed => 8
          case _ => 32
        }
      // Handle 16 bit types
      case (Representation.Binary, PrimType.Short | PrimType.UnsignedShort) =>
        binaryNumberRep match {
          case BinaryNumberRep.Packed | BinaryNumberRep.Bcd | BinaryNumberRep.Ibm4690Packed => 8
          case _ => 16
        }
      // Handle 8 bit types
      case (Representation.Binary, PrimType.Integer | PrimType.Decimal | PrimType.Byte |
        PrimType.UnsignedByte | PrimType.NonNegativeInteger) => 8
      // Handle date types
      case (Representation.Binary, PrimType.DateTime | PrimType.Date | PrimType.Time) =>
        binaryCalendarRep match {
          case BinaryCalendarRep.BinaryMilliseconds => 64
          case BinaryCalendarRep.BinarySeconds => 32
          case _ => schemaDefinitionError("Implicit Alignment: binaryCalendarRep was %s but we expected BinarySeconds or BinaryMilliseconds.", binaryCalendarRep)
        }
    }
  }

  private lazy val implicitAlignmentInBits: Int = getImplicitAlignmentInBits(primType, impliedRepresentation)

  final lazy val alignmentValueInBits: JInt = {
    //
    // get the alignment, measured in bits based on the alignment property, units, and type (when applicable)
    //
    val alignInBits: JInt =
      alignment match {
        case AlignmentType.Implicit => {
          if (this.isComplexType) this.complexType.modelGroup.alignmentValueInBits
          else implicitAlignmentInBits
        }
        case align: JInt => {
          val alignInBits: JInt = this.alignmentUnits match {
            case AlignmentUnits.Bits => align
            case AlignmentUnits.Bytes => 8 * align
          }
          alignInBits
        }
      }
    //
    // Do checking of interactions of alignment with the rest of the representation
    //
    if ((alignment ne AlignmentType.Implicit) && this.isSimpleType) {
      //
      // For explicitly aligned simple types there are specific checks having to do with
      // how explicit alignment interacts with text characters, or with binary packed decimal - as text chars
      // and packed decimal digits come with alignment constraints of their own.
      //
      impliedRepresentation match {
        case Representation.Text => {
          //
          // If they have text representation, alignment and the text encoding alignment must be compared.
          //
          if (isRepresented && (alignInBits % implicitAlignmentInBits) != 0)
            SDE(
              "The given alignment (%s bits) must be a multiple of the encoding specified alignment (%s bits) for %s when representation='text'. Encoding: %s",
              alignInBits, implicitAlignmentInBits, primType.name, this.knownEncodingName)
        }
        case Representation.Binary => {
          //
          // if they have binary representation we must worry about packed digits, which require 4-bit alignment.
          //
          primType match {
            case PrimType.Float | PrimType.Double | PrimType.Boolean | PrimType.HexBinary => /* Non textual data, no need to compare alignment to encoding's expected alignment */
            case _ => binaryNumberRep match {
              case BinaryNumberRep.Packed | BinaryNumberRep.Bcd | BinaryNumberRep.Ibm4690Packed => {
                if ((alignInBits % 4) != 0)
                  SDE(
                    "The given alignment (%s bits) must be a multiple of 4 for %s when using packed binary formats",
                    alignInBits, primType.name)
              }
              case _ => /* Since this is non-textual data, no need to compare alignment to encoding's expected alignment */
            }
          }
        }
      }
    } // end if explicit alignment and simple type
    //
    // Now regardless of type, check for whether the initiator interacts badly with
    // the alignment.
    //
    if (hasInitiator) {
      // Check for case where explicit alignment property and
      // mandatory text alignment of initiator
      // are not compatible.
      val textAlign = knownEncodingAlignmentInBits
      // the explicit alignment must be a multiple of the textAlign
      if (textAlign < alignInBits || textAlign % alignInBits != 0)
        SDW(
          WarnID.AlignmentAndInitiatorTextAlignmentNotCompatible,
          "Initiator text may leave the element incorrectly aligned. The text encoding of initiator characters is %s bits, " +
            "but the element alignment requires %s bits. Suggest consider whether both dfdl:initiator and dfdl:alignment should be specified for this element.",
          textAlign, alignInBits)
    }
    //
    // Having done the checks, just return the answer
    //
    alignInBits
  }

  /**
   * Tells us if we have a specific length.
   *
   * Keep in mind that 80 characters in length can be anywhere from 80 to 320 bytes
   * depending on the character encoding. So fixed length doesn't mean in bytes.
   * it means in dfdl:lengthUnits units, which could be characters, and those can
   * be fixed or variable width.
   */
  final lazy val isFixedLength = {
    (lengthKind =:= LengthKind.Explicit && lengthEv.isConstant) ||
      isImplicitLengthString
    // FIXME: there are lots of other cases where things are fixed length
    // e.g., implicit length hexBinary uses maxLength for length in bytes
    // e.g., implicit length fixed-precision binary numbers (byte, short, int, long and unsigned thereof)
    // In general the things in this file about fixed length seem to miss hexBinary.
    // Also, things like packed and zoned decimal are usually fixed length, sometimes delimited.
  }

  final lazy val isImplicitLengthString = isSimpleType && primType =:= PrimType.String && lengthKind =:= LengthKind.Implicit

  final lazy val fixedLengthValue: Long = {
    // FIXME: this calculation only good for string type.
    // We need this for every type and representation however.
    // The answer should be in lengthUnits, and -1 should mean
    // lengthUnits is bytes/bits, but the fixed length is given in characters
    // of a variable-width charset, so greater than 0, but we don't know
    // exactly.
    Assert.usage(isFixedLength)
    if (lengthKind =:= LengthKind.Explicit) lengthEv.optConstant.get
    else {
      Assert.invariant(lengthKind =:= LengthKind.Implicit)
      // it's a string with implicit length. get from facets
      schemaDefinitionUnless(this.hasMaxLength, "String with dfdl:lengthKind='implicit' must have an XSD maxLength facet value.")
      val ml = this.maxLength
      ml.longValue()
    }
  }

  final def hasFixedLengthOf(n: Int): Boolean = {
    // FIXME: needs to work in lengthUnits. If length units is bytes/bits
    // and encoding is variable-width charset, what should this return?
    // (Perhaps should be usage error?)
    if (!isFixedLength) false
    else {
      val fl = fixedLengthValue
      n.toLong =#= fl
    }
  }

  final lazy val isLengthAlwaysNonZero = {
    Assert.usage(isRepresented)
    if (hasFixedLengthOf(0)) false
    else if (isFixedLength) true
    else false
  }

  final lazy val fixedLength = {
    if (isFixedLength) lengthEv.optConstant.get.longValue() else -1L
    // FIXME: shouldn't even be asking for this if not isFixedLength
    // try changing to Assert.usage(isFixedLength)
    // FIXME: needs to return fixed length in lengthUnits.
    // as for many of the other methods associated with length,
    // if lengthUnits is bits/bytes and the element is textual, and
    // the encoding is unknown, or known to be variable-width, what to return?
    // (perhaps usage error, you must ask about textual data with unknown or
    // known variable-width charset in lengthUnits characters ??)
  }

  // FIXME: bless this method. Deprecate and remove other less reliable things.
  final lazy val maybeFixedLengthInBits: MaybeULong = {
    if (optRepTypeElement.isDefined) {
      optRepTypeElement.get.maybeFixedLengthInBits
    } else {
      if (isRepresented && isFixedLength) {
        val bitsMultiplier = lengthUnits match {
          case LengthUnits.Bits => 1
          case LengthUnits.Bytes => 8
          case LengthUnits.Characters => if (knownEncodingIsFixedWidth) knownEncodingWidthInBits else -1
        }
        if (bitsMultiplier > 0) {
          MaybeULong(fixedLengthValue * bitsMultiplier)
        } else {
          MaybeULong.Nope
        }
      } else {
        MaybeULong.Nope
      }
    }
  }

  /**
   * Nil Lit = literal nil, as opposed to value nil that uses a reserved value
   */
  private lazy val isDefinedNilLit: Boolean = {
    val res = isNillable &&
      (nilKind == NilKind.LiteralValue ||
        nilKind == NilKind.LiteralCharacter)
    res
  }

  private lazy val isDefinedNilValue: Boolean = {
    val res = (isNillable && nilKind == NilKind.LogicalValue)
    res
  }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */

  private def NVDP = NilValueDelimiterPolicy
  private def EVDP = EmptyValueDelimiterPolicy

  protected final lazy val hasNilValueInitiator = hasNonEmptyDelimiter(initiatorParseEv, nilValueDelimiterPolicy, NVDP.Both, NVDP.Initiator)
  protected final lazy val hasNilValueTerminator = hasNonEmptyDelimiter(terminatorParseEv, nilValueDelimiterPolicy, NVDP.Both, NVDP.Terminator)

  /**
   * We need the nil values in raw form for diagnostic messages.
   *
   * We need the nil values in cooked forms of two kinds. For parsing, and for unparsing.
   *
   * The difference is due to for unparsing the %NL; is treated specially
   * because it must be computed based on dfdl:outputNewLine.
   */
  final lazy val cookedNilValuesForParse = cookedNilValue(forUnparse = false)
  final lazy val rawNilValuesForParse = rawNilValueList(forUnparse = false)

  final lazy val cookedNilValuesForUnparse = cookedNilValue(forUnparse = true)
  final lazy val rawNilValuesForUnparse = rawNilValueList(forUnparse = false)

  final lazy val hasESNilValue = rawNilValuesForParse.contains("%ES;")

  /**
   * Determines if the nil representation is one that has non-zero-length
   * syntax.
   *
   * Returnes true if the nil representation *always* has some non-zero length.
   *
   * Returns false if a zero-length representation is the nil representation, or one of the
   * accepted nil representations. This enables one to quickly recognize that
   * a nilled element should be produced, and that one need not (in the runtime)
   * entertain considerations of required/optional, separator suppression, etc.
   *
   * Since dfdl:nilValue property takes a list of nil representations
   * (for dfdl:nilKind 'literalValue'), it is possible for a given element to
   * have both zero-length and non-zero-length representations which cause
   * a nilled element to be created by the parser.
   */
  final lazy val hasNilValueRequiredSyntax = isNillable &&
    ((isDefinedNilLit && (hasNilValueInitiator || hasNilValueTerminator)) ||
      (isDefinedNilLit && !hasESNilValue) ||
      (isDefinedNilValue && (hasInitiator || hasTerminator)) ||
      // below is the case of string or hexbinary and nilKind logicalValue. A logical value of ES can
      // cause a nil value to be created.
      (isDefinedNilValue && (isSimpleType && (simpleType.primType =:= PrimType.String || simpleType.primType =:= PrimType.HexBinary) && !hasESNilValue)))

  final lazy val hasEmptyValueInitiator = hasNonEmptyDelimiter(initiatorParseEv, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Initiator)
  final lazy val hasEmptyValueTerminator = hasNonEmptyDelimiter(terminatorParseEv, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Terminator)

  // See how this function takes the prop: => Any that is pass by name (aka lazy pass).
  // That allows us to not require the property to exist at all if
  // expr.isConstantEmptyString turns out to be true.
  private def hasNonEmptyDelimiter(expr: DelimiterParseEv, prop: => Any, true1: Any, true2: Any): Boolean = {
    // changed from a match on a 2-tuple to if-then-else logic because we don't even want to ask for
    // prop's value at all unless the first test is false.
    if (expr.isConstantEmptyString)
      false
    else
      prop == true1 || prop == true2
  }

  /**
   * check length and if there are delimiters such that there is a concept of something that we can call 'empty'
   *
   * Empty is observable so long as one can
   * have zero length followed by a separator, or zero length between an
   * initiator and terminator (as required for empty by emptyValueDelimiterPolicy)
   *
   * The concept of Empty here subsumes "empty triggers default values (when defined)"
   * From DFDL Spec (2014 draft) section 9.4.2, and
   * zero-length so that separator suppression applies, from DFDL Spec 2014 draft Section 14.2.1)
   *
   * It applies in this case to elements only. There is a concept of zero-length for model groups as well
   * but is defined elsewhere.
   */
  final lazy val isEmptyAnObservableConcept: Boolean = {
    if (!isRepresented) false
    else if (this.isLengthAlwaysNonZero) false
    else if (isSimpleType) {
      primType match {
        case NodeInfo.String | NodeInfo.HexBinary => true // can be zero-length so empty, regardless of EVDP and init/term
        case _ => {
          // for other simple types, they can be empty only if they are defaultable
          // because otherwise they always require some representation.
          // but this calculation is used by the isDefaultable predicate to decide
          // whether an element can have an empty rep so that it could be defaulted.
          false
        }
      }
    } else {
      Assert.invariant(isComplexType)
      val isZLP = !complexType.modelGroup.hasKnownRequiredSyntax
      val res = isZLP
      res
    }
  }

  /**
   * True if the element can be empty, and there is no syntax to indicate
   * empty-string or empty-hexBinary values, i.e., they do not require initiator nor terminator)
   * so they are just zero-length.
   *
   * False if the type can't be empty.
   */
  final lazy val hasEmptyValueZLSyntax =
    !hasEmptyValueInitiator && !hasEmptyValueTerminator && isSimpleType &&
      (simpleType.primType =:= PrimType.String ||
        simpleType.primType =:= PrimType.HexBinary)

  import org.apache.daffodil.dsom.FacetTypes._

  private lazy val hasPattern: Boolean = typeDef.optRestriction.exists(_.hasPattern)
  private lazy val hasEnumeration: Boolean = typeDef.optRestriction.exists(_.hasEnumeration)
  protected lazy val hasMinLength = typeDef.optRestriction.exists(_.hasMinLength)
  protected lazy val hasMaxLength = typeDef.optRestriction.exists(_.hasMaxLength)
  private lazy val hasMinInclusive = typeDef.optRestriction.exists(_.hasMinInclusive)
  private lazy val hasMaxInclusive = typeDef.optRestriction.exists(_.hasMaxInclusive)
  private lazy val hasMinExclusive = typeDef.optRestriction.exists(_.hasMinExclusive)
  private lazy val hasMaxExclusive = typeDef.optRestriction.exists(_.hasMaxExclusive)
  private lazy val hasTotalDigits = typeDef.optRestriction.exists(_.hasTotalDigits)
  private lazy val hasFractionDigits = typeDef.optRestriction.exists(_.hasFractionDigits)

  final lazy val patternValues: Seq[FacetValueR] = {
    Assert.invariant(hasPattern)
    typeDef.optRestriction.map { r =>
      schemaDefinitionUnless(
        r.primType == PrimType.String,
        "Pattern is only allowed to be applied to string and types derived from string.")
      r.patternValues
    }.getOrElse(Nil)
  }

  private lazy val enumerationValues: Option[String] = {
    Assert.invariant(hasEnumeration)
    typeDef.optRestriction.flatMap { _.enumerationValues }
  }

  /**
   * Compute minLength and maxLength together to share error-checking
   * and case dispatch that would otherwise have to be repeated.
   */
  final lazy val (minLength: java.math.BigDecimal, maxLength: java.math.BigDecimal) = computeMinMaxLength
  // TODO: why are we using java.math.BigDecimal, when scala has a much
  // nicer decimal class?
  private val zeroBD = new java.math.BigDecimal(0)
  private val unbBD = new java.math.BigDecimal(-1) // TODO: should this be a tunable limit?

  private def computeMinMaxLength: (java.math.BigDecimal, java.math.BigDecimal) = {
    schemaDefinitionUnless(isSimpleType, "Facets minLength and maxLength are allowed only on types string and hexBinary.")
    typeDef match {
      case prim: PrimitiveType => {
        val pt = prim.primType
        schemaDefinitionWhen(
          (pt == PrimType.String || pt == PrimType.HexBinary) && lengthKind == LengthKind.Implicit,
          "Facets minLength and maxLength must be defined for type %s with lengthKind='implicit'",
          pt.name)
        //
        // We handle text numbers by getting a stringValue first, then
        // we convert to the number type.
        //
        // This means we cannot check and SDE here on incorrect simple type.
        (zeroBD, unbBD)
      }
      case st: SimpleTypeDefBase if st.optRepTypeElement.isDefined => (st.optRepTypeElement.get.minLength, st.optRepTypeElement.get.maxLength)
      case st: SimpleTypeDefBase if st.optRestriction.isDefined => {
        val r = st.optRestriction.get
        val pt = st.primType
        val typeOK = pt == PrimType.String || pt == PrimType.HexBinary
        schemaDefinitionWhen(
          !typeOK && (hasMinLength || hasMaxLength),
          "Facets minLength and maxLength are not allowed on types derived from type %s.\nThey are allowed only on typed derived from string and hexBinary.",
          pt.name)
        val res = (hasMinLength, hasMaxLength, lengthKind) match {
          case (true, true, LengthKind.Implicit) => {
            schemaDefinitionUnless(
              r.minLengthValue.compareTo(r.maxLengthValue) == 0,
              "The minLength and maxLength must be equal for type %s with lengthKind='implicit'. Values were minLength of %s, maxLength of %s.",
              pt.name, r.minLengthValue, r.maxLengthValue)
            (r.minLengthValue, r.maxLengthValue)
          }
          case (true, true, _) => {
            schemaDefinitionWhen(
              r.minLengthValue.compareTo(r.maxLengthValue) > 0,
              // always true, so we don't bother to specify the type in the message.
              "The minLength facet value must be less than or equal to the maxLength facet value. Values were minLength of %s, maxLength of %s.",
              r.minLengthValue, r.maxLengthValue)
            (r.minLengthValue, r.maxLengthValue)
          }
          case (_, _, LengthKind.Implicit) => SDE("When lengthKind='implicit', both minLength and maxLength facets must be specified.")
          case (false, true, _) => (zeroBD, r.maxLengthValue)
          case (false, false, _) => (zeroBD, unbBD)
          case (true, false, _) => (r.minLengthValue, unbBD)
          case _ => Assert.impossible()
        }
        res
      }
      case st: SimpleTypeDefBase => {
        Assert.invariant(st.optRestriction.isEmpty)
        (zeroBD, unbBD)
      }
    }
  }

  // TODO: see code above that computes minLength, maxLength
  // simultaneously to avoid redundant check code.
  //
  // Same thing applies to the other paired facets where there is lots of
  // common logic associated with checking.
  private lazy val minInclusive: java.math.BigDecimal = {
    Assert.usage(hasMinInclusive)
    typeDef.optRestriction.map { r =>
      if (r.hasMinExclusive) SDE("MinInclusive and MinExclusive cannot be specified for the same simple type.")
      if (r.hasMaxExclusive) {
        val res = r.minInclusiveValue.compareTo(r.maxExclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxExclusive(%s).", r.minInclusiveValue, r.maxExclusiveValue)
      }
      if (r.hasMaxInclusive) {
        val res = r.minInclusiveValue.compareTo(r.maxInclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxInclusive(%s).", r.minInclusiveValue, r.maxInclusiveValue)
      }
      r.minInclusiveValue
    }.get
  }

  private lazy val maxInclusive: java.math.BigDecimal = {
    Assert.usage(hasMaxInclusive)
    typeDef.optRestriction.map { r =>
      if (r.hasMaxExclusive) SDE("MaxInclusive and MaxExclusive cannot be specified for the same simple type.")
      if (r.hasMinExclusive) {
        val res = r.minExclusiveValue.compareTo(r.maxInclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxInclusive(%s)", r.minExclusiveValue, r.maxInclusiveValue)
      }
      if (r.hasMinInclusive) {
        val res = r.minInclusiveValue.compareTo(r.maxInclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxInclusive(%s)", r.minInclusiveValue, r.maxInclusiveValue)
      }
      r.maxInclusiveValue
    }.get
  }

  private lazy val minExclusive: java.math.BigDecimal = {
    Assert.usage(hasMinExclusive)
    typeDef.optRestriction.map { r =>
      if (r.hasMinInclusive) SDE("MinInclusive and MinExclusive cannot be specified for the same simple type.")
      if (r.hasMaxInclusive) {
        val res = r.minExclusiveValue.compareTo(r.maxInclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxInclusive(%s)", r.minExclusiveValue, r.maxInclusiveValue)
      }
      if (r.hasMaxExclusive) {
        val res = r.minExclusiveValue.compareTo(r.maxExclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxExclusive(%s)", r.minExclusiveValue, r.maxExclusiveValue)
      }
      r.minExclusiveValue
    }.get
  }

  private lazy val maxExclusive: java.math.BigDecimal = {
    Assert.invariant(hasMaxExclusive)
    typeDef.optRestriction.map { r =>
      if (r.hasMaxInclusive) SDE("MaxExclusive and MaxInclusive cannot be specified for the same simple type.")
      if (r.hasMinInclusive) {
        val res = r.minInclusiveValue.compareTo(r.maxExclusiveValue)
        if (res > 0) SDE("MinInclusive(%s) must be less than or equal to MaxExclusive(%s)", r.minInclusiveValue, r.maxExclusiveValue)
      }
      if (r.hasMinExclusive) {
        val res = r.minExclusiveValue.compareTo(r.maxExclusiveValue)
        if (res > 0) SDE("MinExclusive(%s) must be less than or equal to MaxExclusive(%s)", r.minExclusiveValue, r.maxExclusiveValue)
      }
      r.maxExclusiveValue
    }.get
  }

  private lazy val totalDigits: java.math.BigDecimal = {
    Assert.usage(hasTotalDigits)
    val st = simpleType
    typeDef.optRestriction.map { r =>
      // Can only be applied to decimal or any of the integer types, and
      // types derived from them
      val isDerivedFromDecimal = st.primType.isSubtypeOf(NodeInfo.Decimal)
      val isDerivedFromInteger = st.primType.isSubtypeOf(NodeInfo.Integer)
      if (isDerivedFromDecimal || isDerivedFromInteger) r.totalDigitsValue
      else {
        SDE("TotalDigits facet can only be applied to decimal or any of the integer types, and types derived from them. Restriction base is %s", st.primType.name)
      }
    }.get
  }

  private lazy val fractionDigits: java.math.BigDecimal = {
    Assert.usage(hasFractionDigits)
    typeDef.optRestriction.map { r =>
      // Can only be applied to decimal
      val isDerivedFromDecimal = r.primType.isSubtypeOf(NodeInfo.Decimal)
      if (isDerivedFromDecimal) {
        if (r.hasTotalDigits) {
          val res = r.fractionDigitsValue.compareTo(r.totalDigitsValue)
          if (res > 0) SDE("FractionDigits facet must not exceed TotalDigits.")
        }
        r.fractionDigitsValue
      } else {
        SDE("FractionDigits facet can only be applied to decimal. Restriction base is %s", r.primType.name)
      }
    }.get
  }

  /**
   * Does the element have a default value?
   */
  def defaultValueAsString: String
  def hasDefaultValue: Boolean

  /**
   * We require that there be a concept of empty if we're going to be able to default something
   * and we are going to require that we can tell this statically. I.e., we're not going to defer this to runtime
   * just in case the delimiters are being determined at runtime.
   *
   * That is to say, if a delimiter is an expression, then we're assuming that means
   * at runtime it will not evaluate to empty string (so you can specify the delimiter
   * at runtime, but you cannot turn on/off the whole delimited format at runtime.)
   */
  final lazy val isDefaultable: Boolean = LV('isDefaultable) {
    if (isSimpleType) {
      if (!isRepresented) false
      else if (!hasDefaultValue) false
      else {
        if (!isEmptyAnObservableConcept)
          SDW(WarnID.NoEmptyDefault, "Element with no empty representation. XSD default='%s' can only be used when unparsing.", defaultValueAsString)
        schemaDefinitionWhen(isOptional, "Optional elements cannot have default values but default='%s' was found.", defaultValueAsString)
        if (isArray && !isArrayWithAtLeastOneRequiredArrayElement) {
          (minOccurs, occursCountKind) match {
            case (_, OccursCountKind.Parsed) |
              (_, OccursCountKind.StopValue) =>
              SDE(
                "XSD default='%s' can never be used since an element with dfdl:occursCountKind='%s' has no required occurrences.",
                defaultValueAsString, occursCountKind)
            case (0, _) => SDE(
              "XSD default='%s' can never be used since an element with XSD minOccurs='0' has no required occurrences.",
              defaultValueAsString)
            case _ => // ok
          }
        }
        Assert.invariant(hasDefaultValue)
        !isOptional &&
          (isScalar ||
            isArrayWithAtLeastOneRequiredArrayElement)
      }
    } else {
      // TODO: Implement complex element defaulting
      // JIRA issue DFDL-1277
      //
      // a complex element is defaultable
      // recursively if everything in it is defaultable
      // and everything in it has no required representation
      // (e.g., no required delimiters, no alignment, no skip, etc.)
      // furthermore, even the defaultable things inside must satisfy
      // a stricter criterion. They must have emptyValueDelimiterPolicy='none'
      // if delimiters are defined and they could be empty (which is implied if they are defaultable)
      false
    }
  }.value

  final lazy val defaultParseUnparsePolicy = optionParseUnparsePolicy.getOrElse(ParseUnparsePolicy.Both)

  /**
   * This function ensures that all children have a compatable
   * parseUnparsePolicy with the root. In other words, if the root policy is
   * 'Both', all children must also be 'Both'. If the root policy is 'Parse' or
   * 'Unparse', then all children must have either the same policy, or must be
   * 'Both'.
   *
   * If the context is None, then that means the policy was determined by the
   * user (e.g. a tunable), rather than by using the default value of the root
   * element
   */
  final def checkParseUnparsePolicyCompatibility(context: Option[ElementBase], policy: ParseUnparsePolicy): Unit = {
    elementChildren.foreach { child =>
      val childPolicy = child.defaultParseUnparsePolicy
      val isCompatible = policy == childPolicy || childPolicy == ParseUnparsePolicy.Both
      if (!isCompatible) {
        if (context.isDefined) {
          context.get.SDE("Child element '%s' with dfdlx:parseUnparsePolicy='%s' is not compatible with root elements dfdlx:parseUnparsePolicy='%s'", child, childPolicy, policy)
        } else {
          SDE("Element '%s' with dfdlx:parseUnparsePolicy='%s' is not compatible with user supplied dfdlx:parseUnparsePolicy='%s'", child, childPolicy, policy)
        }
      }

      // recursively check children
      child.checkParseUnparsePolicyCompatibility(context, policy)
    }
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
   * Part of the required evaluations for ElementBase.
   */
  private lazy val checkForAlignmentAmbiguity: Unit = {
    if (isOptional) {
      this.laterSiblings.filterNot(m => m == this).foreach { that =>
        val isSame = this.alignmentValueInBits == that.alignmentValueInBits
        if (!isSame) {
          this.SDW(WarnID.AlignmentNotSame, "%s is an optional element or a variable-occurrence array and its alignment (%s) is not the same as %s's alignment (%s).",
            this.toString, this.alignmentValueInBits, that.toString, that.alignmentValueInBits)
        }
      }
    }
  }

  private lazy val optionFloating = findPropertyOption("floating")

  private lazy val checkFloating = (optionFloating.isDefined, tunable.requireFloatingProperty) match {
    case (false, false) => SDW(WarnID.FloatingError, "Property 'dfdl:floating' is required but not defined.")
    case (false, true) => floating
    case (_, _) => this.subset((floating eq YesNo.No), "Property value floating='yes' is not supported.")
  }

}
