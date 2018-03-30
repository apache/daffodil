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

import scala.xml.NamespaceBinding
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar._
import org.apache.daffodil.schema.annotation.props._
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.xml._
import org.apache.daffodil.util.Misc
import org.apache.daffodil.processors._
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.grammar.ElementBaseGrammarMixin
import org.apache.daffodil.equality._

import org.apache.daffodil.processors.UseNilForDefault
import org.apache.daffodil.util.MaybeULong
import java.lang.{ Integer => JInt }
import org.apache.daffodil.processors._
import org.apache.daffodil.infoset.SiblingResolver
import org.apache.daffodil.infoset.ResolverType
import org.apache.daffodil.infoset.SeveralPossibilitiesForNextElement
import org.apache.daffodil.infoset.NoNextElement
import org.apache.daffodil.infoset.OnlyOnePossibilityForNextElement
import org.apache.daffodil.infoset.NextElementResolver
import org.apache.daffodil.infoset.ChildResolver
import org.apache.daffodil.api.WarnID

/**
 * Note about DSOM design versus say XSOM or Apache XSD library.
 *
 * Some XSD object models have a single Element class, and distinguish local/global and element references
 * based on attributes of the instances.
 *
 * Our approach is to provide common behaviors on base classes or traits/mixins, and to have distinct
 * classes for each instance type.
 */

object ElementBase {
  var count = 0
}

/**
 * Shared by all forms of elements, local or global or element reference.
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
  with OverlapCheckMixin {

  override final def eBase = this

  ElementBase.count += 1 // how many elements in this schema.

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
  requiredEvaluations(erd.preSerialization)
  requiredEvaluations(checkForAlignmentAmbiguity)

  def name: String

  final lazy val inputValueCalcOption = findPropertyOption("inputValueCalc")

  final lazy val outputValueCalcOption = {
    val optOVC = findPropertyOption("outputValueCalc")
    schemaDefinitionWhen(optOVC.isDefined && isOptional, "dfdl:outputValueCalc cannot be defined on optional elements.")
    schemaDefinitionWhen(optOVC.isDefined && isArray, "dfdl:outputValueCalc cannot be defined on array elements.")
    // This should be an SDE, but is very useful for unit tests to be able to specify OVC on a single global element
    // self.schemaDefinitionWhen(optOVC.isDefined && self.isInstanceOf[GlobalElementDecl], "dfdl:outputValueCalc cannot be defined on global elements.")

    optOVC
  }

  def isNillable: Boolean
  def isSimpleType: Boolean
  def isComplexType: Boolean

  def simpleType: SimpleTypeBase

  def complexType: ComplexTypeBase

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
  override final protected def calcContentParserReferencedElementInfos =
    if (this.inputValueCalcOption.isDefined)
      ivcCompiledExpression.contentReferencedElementInfos
    else
      ReferencedElementInfos.None

  /**
   * The DPathElementInfo objects referenced within an OVC
   * that calls dfdl:contentLength( thingy )
   */
  override final protected def calcContentUnparserReferencedElementInfos =
    if (this.outputValueCalcOption.isDefined)
      ovcCompiledExpression.contentReferencedElementInfos
    else
      ReferencedElementInfos.None

  /**
   * The DPathElementInfo objects referenced within an IVC
   * that calls dfdl:valueLength( thingy )
   */
  override final protected def calcValueParserReferencedElementInfos =
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
  override final protected def calcValueUnparserReferencedElementInfos =
    if (this.outputValueCalcOption.isDefined)
      ovcCompiledExpression.valueReferencedElementInfos
    else
      ReferencedElementInfos.None

  /**
   *  Tells us if, for this element, we need to capture its content length
   *  at parse runtime, or we can ignore that.
   */
  final lazy val isReferencedByContentLengthParserExpressions: Boolean =
    // TODO: should be for all root elements that are compiled in this
    // schema compilation, such time as we allow more than one root element.
    // Applies to all code that references the rootElementRef in this trait.
    rootElementRef.get.contentLengthParserReferencedElementInfos.contains(this.dpathElementCompileInfo)

  /**
   * Tells us if, for this element, we need to capture its content length
   *  at unparse runtime, or we can ignore that.
   */
  final lazy val isReferencedByContentLengthUnparserExpressions: Boolean =
    rootElementRef.get.contentLengthUnparserReferencedElementInfos.contains(this.dpathElementCompileInfo)

  /**
   * Tells us if, for this element, we need to capture its value length
   *  at parse runtime, or we can ignore that.
   */
  final lazy val isReferencedByValueLengthParserExpressions: Boolean = {
    val setElems = rootElementRef.get.valueLengthParserReferencedElementInfos
    //    if (this eq rootElement.get)
    //      println("PARSER these are referenced by valueCalc: " + setElems.toString)
    val res = setElems.contains(this.dpathElementCompileInfo)
    res
  }
  /**
   * Tells us if, for this element, we need to capture its value length
   *  at unparse runtime, or we can ignore that.
   */
  final lazy val isReferencedByValueLengthUnparserExpressions: Boolean = {
    val setElems = rootElementRef.get.valueLengthUnparserReferencedElementInfos
    //    if (this eq rootElement.get)
    //      println("UNPARSER these are referenced by valueCalc: " + setElems.toString)
    val isInExprs = setElems.contains(this.dpathElementCompileInfo)
    val res = isInExprs ||
      otherwiseShouldCaptureValueRegionLength
    res
  }

  /**
   * Besides being referenced by the dfdl:valueLength function,
   * We need the valueLength to be computed for unparser pad/fill, to check
   * excess length, and for alignmentFills.
   *
   * TBD: why for alignment fills? Don't see using it in the code. Try without this?
   */
  private lazy val otherwiseShouldCaptureValueRegionLength: Boolean = {
    val pad = this.shouldAddPadding
    val fill = this.shouldAddFill
    val len = this.shouldCheckExcessLength
    val alg = !this.isKnownToBeAligned // alignment fill uses the value length.
    val res = pad || fill || len || alg
    res
  }

  /**
   * None for complex types, Some(primType) for simple types.
   */
  final lazy val optPrimType: Option[PrimType] = Misc.boolToOpt(isSimpleType, primType) // .typeRuntimeData)

  /**
   * scalar means minOccurs=1 and maxOccurs=1
   */
  def isScalar: Boolean

  /**
   * An array element is required if its index is less than the minOccurs of the
   * array. For an array with a fixed number of elements, all elements are requried.
   */
  def isRequiredArrayElement: Boolean

  final override lazy val dpathCompileInfo = dpathElementCompileInfo

  /**
   * This is the compile info for this element. Since this might be an
   * element ref, we optionally carry the compile info for the referenced
   * element in that case.
   */
  lazy val dpathElementCompileInfo: DPathElementCompileInfo = {
    val ee = enclosingElement
    val eci = new DPathElementCompileInfo(
      ee.map {
        _.dpathElementCompileInfo
      },
      variableMap,
      elementChildrenCompileInfo,
      namespaces,
      slashPath,
      name,
      isArray,
      namedQName,
      optPrimType,
      schemaFileLocation,
      tunable)
    eci
  }

  private lazy val thisElementsNamespace: NS = this.namedQName.namespace
  private lazy val thisElementsNamespacePrefix: String = this.namespaces.getPrefix(thisElementsNamespace.toString)

  private def nsBindingsToSet(nsb: NamespaceBinding): Set[(String, NS)] = {
    if (nsb == scala.xml.TopScope) Set()
    else {
      val parentBindings = nsBindingsToSet(nsb.parent)
      val res = parentBindings.+((nsb.prefix, NS(nsb.uri)))
      res
    }
  }

  private lazy val thisElementsRequiredNamespaceBindings: Set[(String, NS)] = {
    val childrenRequiredNSBindings =
      this.elementChildren.flatMap { _.thisElementsRequiredNamespaceBindings }.toSet

    val myRequiredNSBinding = Set((thisElementsNamespacePrefix, thisElementsNamespace))
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
  }

  private lazy val emptyNSPairs = nsBindingsToSet(scala.xml.TopScope)

  private lazy val myOwnNSPairs: Set[(String, NS)] = thisElementsRequiredNamespaceBindings
  private lazy val myParentNSPairs = enclosingElement match {
    case None => emptyNSPairs
    case Some(parent) => parent.myOwnNSPairs
  }

  private lazy val myUniquePairs: Set[(String, NS)] = {
    val res = myOwnNSPairs -- myParentNSPairs
    res
  }

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

  private lazy val parentMinimizedScope = enclosingElement.map { _.minimizedScope }.getOrElse(scala.xml.TopScope)

  /**
   * To be properly constructed, scala's xml Elems must share the scope (namespace bindings) of the enclosing
   * parent element, except when it adds more of its own bindings, in which case the tail is supposed to be shared.
   */
  private lazy val minimizedScope: NamespaceBinding = {
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
  }

  override lazy val runtimeData: RuntimeData = elementRuntimeData
  override lazy val termRuntimeData: TermRuntimeData = elementRuntimeData

  final lazy val defaultValue = {
    if (isDefaultable && (isScalar || isRequiredArrayElement)) {
      val dv =
        if (isNillable && useNilForDefault =:= YesNo.Yes) {
          UseNilForDefault // singleton object indicator
        } else {
          //
          // Note: no remapping PUA chars or otherwise messing with the text of the default value
          // because this must be a regular XSD default value so that Xerces validation
          // will work.
          //
          val str = defaultValueAsString
          val value = primType.fromXMLString(str)
          value
        }
      Some(dv)
    } else None
  }

  /**
   * The NextElementResolver is used to determine what infoset event comes next, and "resolves" which is to say
   * determines the ElementRuntimeData for that infoset event. This can be used to construct the initial
   * infoset from a stream of XML events.
   */
  final def computeNextElementResolver(possibles: Seq[ElementBase], resolverType: ResolverType): NextElementResolver = {
    //
    // Annoying, but scala's immutable Map is not covariant in its first argument
    // the way one would normally expect a collection to be.
    // So Map[NamedQName, ElementRuntimeData] is not a subtype of Map[QNameBase, ElementRuntimeData]
    // So we need a cast upward to Map[QNameBase,ElementRuntimeData]
    //
    val eltMap = possibles.map {
      e => (e.namedQName, e.elementRuntimeData)
    }.toMap.asInstanceOf[Map[QNameBase, ElementRuntimeData]]
    val resolver = eltMap.size match {
      case 0 => new NoNextElement(schemaFileLocation, resolverType)
      case 1 => new OnlyOnePossibilityForNextElement(schemaFileLocation, eltMap.values.head, resolverType)
      case _ => {
        val groupedByName = possibles.groupBy(_.namedQName.local)
        var hasNamesDifferingOnlyByNS = false
        groupedByName.foreach {
          case (_, sameNamesEB) =>
            if (sameNamesEB.length > 1) {
              SDW(WarnID.NamespaceDifferencesOnly, "Neighboring QNames differ only by namespaces. Infoset representations that do not support namespacess cannot differentiate between these elements and may fail to unparse. QNames are: %s",
                sameNamesEB.map(_.namedQName.toExtendedSyntax).mkString(", "))
              hasNamesDifferingOnlyByNS = true
            }
        }
        new SeveralPossibilitiesForNextElement(schemaFileLocation, eltMap, resolverType, hasNamesDifferingOnlyByNS)
      }
    }
    resolver
  }

  lazy val unparserInfosetElementDefaultingBehavior: UnparserInfo.InfosetEventBehavior = {
    import UnparserInfo._
    //if (isScalar && isDefaultable) ScalarDefaultable
    //else if (isArray && isDefaultable) ArrayDefaultable
    if (!isRepresented) MustExist
    else if (isOutputValueCalc) Computed
    else if (isOptional) Optional
    else if (isArray && !isRequiredArrayElement) Optional
    else MustExist
  }

  lazy val canBeAbsentFromUnparseInfoset: Boolean = {
    import UnparserInfo._
    unparserInfosetElementDefaultingBehavior !=:= MustExist
  }

  //  private lazy val mustBeAbsentFromUnparseInfoset: Boolean = {
  //    isOutputValueCalc
  //  }

  final lazy val nextElementResolver: NextElementResolver = {
    computeNextElementResolver(possibleNextChildElementsInInfoset, SiblingResolver)
  }

  final lazy val childElementResolver: NextElementResolver =
    computeNextElementResolver(possibleFirstChildElementsInInfoset, ChildResolver)

  final def erd = elementRuntimeData // just an abbreviation

  final lazy val elementRuntimeData: ElementRuntimeData = LV('elementRuntimeData) {
    computeElementRuntimeData
  }.value

  protected def computeElementRuntimeData(): ElementRuntimeData = {
    
    lazy val childrenERDs: Seq[ElementRuntimeData] =
      elementChildren.map { _.elementRuntimeData }

    val newERD: ElementRuntimeData = new ElementRuntimeData(
      childrenERDs,
      schemaSet.variableMap,
      nextElementResolver,
      childElementResolver,
      encodingInfo,
      dpathElementCompileInfo,
      schemaFileLocation,
      diagnosticDebugName,
      path,
      namespaces,
      minimizedScope,
      defaultBitOrder,
      optPrimType,
      targetNamespace,
      thisElementsNamespace,
      optSimpleTypeRuntimeData,
      optMinOccurs,
      optMaxOccurs,
      name,
      targetNamespacePrefix,
      thisElementsNamespacePrefix,
      isHidden,
      isNillable,
      isArray, // can have more than 1 occurrence
      isOptional, // can have exactly 0 or 1 occurrence
      isRequired, // must have at least 1 occurrence
      namedQName,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      impliedRepresentation,
      optIgnoreCase,
      defaultValue,
      //
      // unparser specific items
      //
      optTruncateSpecifiedLengthString,
      if (isOutputValueCalc) Some(ovcCompiledExpression) else None,
      maybeBinaryFloatRepEv,
      maybeByteOrderEv,
      maybeFillByteEv,
      maybeCheckByteAndBitOrderEv,
      maybeCheckBitOrderAndCharset)
    newERD
  }

  private lazy val optTruncateSpecifiedLengthString =
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
  final lazy val elementChildren: Seq[ElementBase] = {
    this.typeDef match {
      case ct: ComplexTypeBase => {
        ct.group.elementChildren
      }
      case _ => Nil
    }
  }

  final lazy val elementChildrenCompileInfo =
    elementChildren.map {
      _.dpathElementCompileInfo
    }

  final override lazy val isRepresented = {
    val isRep = inputValueCalcOption.isInstanceOf[NotFound]
    if (!isRep) {
      if (isOptional) {
        SDE("inputValueCalc property can not appear on optional elements")
      }
      if (!isScalar) {
        SDE("inputValueCalc property can not appear on array elements")
      }
    }
    isRep
  }

  final lazy val isOutputValueCalc = outputValueCalcOption.isInstanceOf[Found]

  final override lazy val impliedRepresentation = {
    val rep = if (isSimpleType) {
      primType match {
        case PrimType.HexBinary => Representation.Binary
        case PrimType.String => Representation.Text
        case _ => representation
      }
    } else {
      representation
    }
    rep match {
      case Representation.Binary =>
        byteOrderEv // ensure defined
      case _ =>
        charsetEv // ensure defined
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
    else Seq(complexType.group)
  }

  final lazy val isParentUnorderedSequence: Boolean = {
    parent match {
      case s: SequenceTermBase if !s.isOrdered => true
      case _ => false
    }
  }

  private def getImplicitAlignmentInBits(thePrimType: PrimType, theRepresentation: Representation): Int = {
    (theRepresentation, thePrimType) match {
      case (Representation.Text, PrimType.HexBinary) => Assert.impossible("type xs:hexBinary with representation='text'")
      case (Representation.Text, _) => knownEncodingAlignmentInBits
      case (Representation.Binary, PrimType.Float | PrimType.Boolean) => 32
      case (Representation.Binary, PrimType.Double) => 64
      case (Representation.Binary, _) => binaryNumberRep match {
        case BinaryNumberRep.Packed | BinaryNumberRep.Bcd | BinaryNumberRep.Ibm4690Packed => 8
        case _ => thePrimType match {
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
  }

  private lazy val implicitAlignmentInBits: Int = getImplicitAlignmentInBits(primType, impliedRepresentation)

  final lazy val alignmentValueInBits: JInt = {
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
        if (this.isSimpleType) {
          impliedRepresentation match {
            case Representation.Text => {
              if (isRepresented && (alignInBits % implicitAlignmentInBits) != 0)
                SDE("The given alignment (%s bits) must be a multiple of the encoding specified alignment (%s bits) for %s when representation='text'. Encoding: %s",
                  alignInBits, implicitAlignmentInBits, primType.name, this.knownEncodingName)
            }
            case Representation.Binary => primType match {
              case PrimType.Float | PrimType.Double | PrimType.Boolean => /* Non textual data, no need to compare alignment to encoding's expected alignment */
              case _ => binaryNumberRep match {
                case BinaryNumberRep.Packed | BinaryNumberRep.Bcd | BinaryNumberRep.Ibm4690Packed => {
                  if ((alignInBits % 4) != 0)
                    SDE("The given alignment (%s bits) must be a multiple of 4 for %s when using packed binary formats",
                      alignInBits, primType.name)
                }
                case _ => /* Non textual data, no need to compare alignment to encoding's expected alignment */
              }
            }
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
  final lazy val isFixedLength = {
    (lengthKind =:= LengthKind.Explicit && lengthEv.isConstant) ||
      isImplicitLengthString
    // TODO: there are lots of other cases where things are fixed length
    // e.g., implicit length hexBinary uses maxLength for length in bytes
    // e.g., implicit length fixed-precision binary numbers (byte, short, int, long and unsigned thereof)
    // In general the things in this file about fixed length seem to miss hexBinary.
  }

  final def isImplicitLengthString = isSimpleType && primType =:= PrimType.String && lengthKind =:= LengthKind.Implicit

  final lazy val fixedLengthValue: Long = {
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

  final def hasFixedLengthOf(n: Int) = {
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
    if (isFixedLength) lengthEv.optConstant.get.longValue() else -1L // shouldn't even be asking for this if not isFixedLength
  }

  final lazy val maybeFixedLengthInBits = {
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

  private lazy val hasNilValueInitiator = initTermTestExpression(initiatorParseEv, nilValueDelimiterPolicy, NVDP.Both, NVDP.Initiator)
  private lazy val hasNilValueTerminator = initTermTestExpression(terminatorParseEv, nilValueDelimiterPolicy, NVDP.Both, NVDP.Terminator)

  /**
   * We need the nil values in raw form for diagnostic messages.
   *
   * We need the nil values in cooked forms of two kinds. For parsing, and for unparsing.
   *
   * The difference is due to for unparsing the %NL; is treated specially
   * because it must be computed based on dfdl:outputNewLine.
   */
  lazy val cookedNilValuesForParse = cookedNilValue(forUnparse = false)
  lazy val rawNilValuesForParse = rawNilValueList(forUnparse = false)

  lazy val cookedNilValuesForUnparse = cookedNilValue(forUnparse = true)
  lazy val rawNilValuesForUnparse = rawNilValueList(forUnparse = false)

  lazy val hasESNilValue = rawNilValuesForParse.contains("%ES;")

  final lazy val hasNilValueRequiredSyntax = isNillable &&
    ((isDefinedNilLit && (hasNilValueInitiator || hasNilValueTerminator)) ||
      (isDefinedNilLit && !hasESNilValue) ||
      (isDefinedNilValue && (hasInitiator || hasTerminator)) ||
      // below is the case of string or hexbinary and nilKind logicalValue. A logical value of ES can
      // cause a nil value to be created.
      (isDefinedNilValue && (isSimpleType && (simpleType.primType =:= PrimType.String || simpleType.primType =:= PrimType.HexBinary) && !hasESNilValue)))

  final lazy val hasEmptyValueInitiator = initTermTestExpression(initiatorParseEv, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Initiator)
  final lazy val hasEmptyValueTerminator = initTermTestExpression(terminatorParseEv, emptyValueDelimiterPolicy, EVDP.Both, EVDP.Terminator)

  // See how this function takes the prop: => Any that is pass by name (aka lazy pass).
  // That allows us to not require the property to exist at all if
  // expr.isKnownNotEmpty turns out to be false.
  private def initTermTestExpression(expr: DelimiterParseEv, prop: => Any, true1: Any, true2: Any): Boolean = {
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
   * check length and if there are delimiters such that there is a concept of something that we can call 'empty'
   *
   * Empty is observable so long as one can
   * have zero length followed by a separator, or zero length between an
   * initiator and terminator (as required for empty by emptyValueDelimiterPolicy)
   */
  final def emptyIsAnObservableConcept = LV('emptyIsAnObservableConcept) {
    if (this.isLengthAlwaysNonZero) false
    else {
      val res = if (hasSep || //FIXME: not sufficient unless it's a postfix separator, or we know there will be some other terminating markup after.
        hasEmptyValueInitiator ||
        hasEmptyValueTerminator) {
        true
      } else false
      res
    }
  }.value

  import org.apache.daffodil.dsom.FacetTypes._

  private lazy val hasPattern: Boolean = typeDef.optRestriction.map { _.hasPattern }.getOrElse(false)
  private lazy val hasEnumeration: Boolean = typeDef.optRestriction.map { _.hasEnumeration }.getOrElse(false)
  protected lazy val hasMinLength = typeDef.optRestriction.map { _.hasMinLength }.getOrElse(false)
  protected lazy val hasMaxLength = typeDef.optRestriction.map { _.hasMaxLength }.getOrElse(false)
  private lazy val hasMinInclusive = typeDef.optRestriction.map { _.hasMinInclusive }.getOrElse(false)
  private lazy val hasMaxInclusive = typeDef.optRestriction.map { _.hasMaxInclusive }.getOrElse(false)
  private lazy val hasMinExclusive = typeDef.optRestriction.map { _.hasMinExclusive }.getOrElse(false)
  private lazy val hasMaxExclusive = typeDef.optRestriction.map { _.hasMaxExclusive }.getOrElse(false)
  private lazy val hasTotalDigits = typeDef.optRestriction.map { _.hasTotalDigits }.getOrElse(false)
  private lazy val hasFractionDigits = typeDef.optRestriction.map { _.hasFractionDigits }.getOrElse(false)

  final lazy val patternValues: Seq[FacetValueR] = {
    Assert.invariant(hasPattern)
    typeDef.optRestriction.map { r =>
      schemaDefinitionUnless(r.primType == PrimType.String,
        "Pattern is only allowed to be applied to string and types derived from string.")
      r.patternValues
    }.getOrElse(Nil)
  }

  private lazy val enumerationValues: Option[String] = {
    Assert.invariant(hasEnumeration)
    typeDef.optRestriction.flatMap { _.enumerationValues }
    //    .getOrElse {
    //      Assert.invariantFailed("must have an enumeration value")
    //    }
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
        schemaDefinitionWhen((pt == PrimType.String || pt == PrimType.HexBinary) && lengthKind == LengthKind.Implicit,
          "Facets minLength and maxLength must be defined for type %s with lengthKind='implicit'",
          pt.name)
        //
        // We handle text numbers by getting a stringValue first, then
        // we convert to the number type.
        //
        // This means we cannot check and SDE here on incorrect simple type.
        return (zeroBD, unbBD)
      }
      case st: SimpleTypeDefBase if st.optRestriction.isDefined => {
        val r = st.optRestriction.get
        val pt = st.primType
        val typeOK = pt == PrimType.String || pt == PrimType.HexBinary
        schemaDefinitionWhen(!typeOK && (hasMinLength || hasMaxLength),
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
      val isDerivedFromDecimal = NodeInfo.isXDerivedFromY(st.primType.name, "decimal")
      val isDerivedFromInteger = NodeInfo.isXDerivedFromY(st.primType.name, "integer")
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
      val isDerivedFromDecimal = NodeInfo.isXDerivedFromY(r.primType.name, "decimal")
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

  private lazy val optSimpleTypeRuntimeData: Option[SimpleTypeRuntimeData] =
    typeDef match {
      case _: PrimitiveType => None
      case _: ComplexTypeBase => None
      case s: SimpleTypeDefBase =>
        Some(s.simpleTypeRuntimeData)
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
        if (!emptyIsAnObservableConcept)
          SDW(WarnID.NoEmptyDefault, "Element with no empty representation. XSD default='%s' can only be used when unparsing.", defaultValueAsString)
        schemaDefinitionWhen(isOptional, "Optional elements cannot have default values but default='%s' was found.", defaultValueAsString)
        if (isArray && !isRequiredArrayElement) {
          (optMinOccurs, occursCountKind) match {
            case (_, OccursCountKind.Parsed) |
              (_, OccursCountKind.StopValue) =>
              SDE("XSD default='%s' can never be used since an element with dfdl:occursCountKind='%s' has no required occurrences.",
                defaultValueAsString, occursCountKind)
            case (Some(0), _) => SDE("XSD default='%s' can never be used since an element with XSD minOccurs='0' has no required occurrences.",
              defaultValueAsString)
            case _ => // ok
          }
        }
        Assert.invariant(hasDefaultValue)
        !isOptional &&
          (isScalar ||
            isRequiredArrayElement)
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

  protected final def possibleFirstChildTerms: Seq[Term] = termChildren

  protected final def couldBeLastElementInModelGroup: Boolean = LV('couldBeLastElementInModelGroup) {
    val couldBeLast = enclosingTerm match {
      case None => true
      case Some(s: SequenceTermBase) if s.isOrdered => {
        !possibleNextSiblingTerms.exists {
          case e: ElementBase => e.isRequired
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
      }
      case _ => true
    }
    couldBeLast
  }.value

  final lazy val nextParentElements: Seq[ElementBase] = {
    if (enclosingTerm.isDefined && couldBeLastElementInModelGroup) {
      enclosingTerm.get.possibleNextChildElementsInInfoset
    } else {
      Nil
    }
  }

  final lazy val defaultParseUnparsePolicy = optionParseUnparsePolicy.getOrElse(ParseUnparsePolicy.Both)

  // This function ensures that all children have a compatable
  // parseUnparsePolicy with the root. In other words, if the root policy is
  // 'Both', all children must also be 'Both'. If the root policy is 'Parse' or
  // 'Unparse', then all children must have either the same policy, or must be
  // 'Both'.
  //
  // If the context is None, then that means the policy was determined by the
  // user (e.g. a tunable), rather than by using the default value of the root
  // element
  final def checkParseUnparsePolicyCompatibility(context: Option[ElementBase], policy: ParseUnparsePolicy): Unit = {
    elementChildren.foreach { child =>
      val childPolicy = child.defaultParseUnparsePolicy
      val isCompatible = policy == childPolicy || childPolicy == ParseUnparsePolicy.Both
      if (!isCompatible) {
        if (context.isDefined) {
          context.get.SDE("Child element '%s' with daf:parseUnparsePolicy='%s' is not compatible with root elements daf:parseUnparsePolicy='%s'", child, childPolicy, policy)
        } else {
          SDE("Element '%s' with daf:parseUnparsePolicy='%s' is not compatible with user supplied daf:parseUnparsePolicy='%s'", child, childPolicy, policy)
        }
      }

      // recursively check children
      child.checkParseUnparsePolicyCompatibility(context, policy)
    }
  }

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

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
  private def checkForAlignmentAmbiguity: Unit = {
    if (isOptional) {
      this.possibleNextTerms.filterNot(m => m == this).foreach { that =>
        val isSame = this.alignmentValueInBits == that.alignmentValueInBits
        if (!isSame) {
          this.SDW(WarnID.AlignmentNotSame, "%s is an optional element or a variable-occurrence array and its alignment (%s) is not the same as %s's alignment (%s).",
            this.toString, this.alignmentValueInBits, that.toString, that.alignmentValueInBits)
        }
      }
    }
  }

}
