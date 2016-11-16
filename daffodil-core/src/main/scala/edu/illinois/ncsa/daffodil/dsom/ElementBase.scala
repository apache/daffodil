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
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.grammar.ElementBaseGrammarMixin
import edu.illinois.ncsa.daffodil.processors.unparsers.NextElementResolver
import edu.illinois.ncsa.daffodil.processors.unparsers.SeveralPossibilitiesForNextElement
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.processors.unparsers.NoNextElement
import edu.illinois.ncsa.daffodil.processors.unparsers.OnlyOnePossibilityForNextElement
import edu.illinois.ncsa.daffodil.processors.unparsers.ChildResolver
import edu.illinois.ncsa.daffodil.processors.unparsers.SiblingResolver
import edu.illinois.ncsa.daffodil.processors.unparsers.ResolverType
import edu.illinois.ncsa.daffodil.processors.UseNilForDefault
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.lang.{ Integer => JInt }
import edu.illinois.ncsa.daffodil.processors._

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
abstract class ElementBase(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends Term(xmlArg, parent, position)
  with Element_AnnotationMixin
  with NillableMixin
  with DFDLStatementMixin
  with ElementBaseGrammarMixin
  with ElementRuntimeValuedPropertiesMixin
  with StringTextMixin
  with NumberTextMixin
  with CalendarTextMixin
  with BooleanTextMixin
  with TextNumberFormatMixin
  with RealTermMixin {

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

  def name: String

  def inputValueCalcOption: PropertyLookupResult
  def outputValueCalcOption: PropertyLookupResult
  def isNillable: Boolean
  def isSimpleType: Boolean
  def isComplexType: Boolean
  def elementComplexType: ComplexTypeBase
  def elementSimpleType: SimpleTypeBase
  def typeDef: TypeBase

  def isRequired = true // overridden in particle mixin.

  override final protected def calcContentParserReferencedElementInfos =
    if (this.inputValueCalcOption.isDefined)
      ivcCompiledExpression.contentReferencedElementInfos
    else
      ReferencedElementInfos.None

  override final protected def calcContentUnparserReferencedElementInfos =
    if (this.outputValueCalcOption.isDefined)
      ovcCompiledExpression.contentReferencedElementInfos
    else
      ReferencedElementInfos.None

  override final protected def calcValueParserReferencedElementInfos =
    if (this.inputValueCalcOption.isDefined) {
      val ivcCE = ivcCompiledExpression
      val setERs = ivcCE.valueReferencedElementInfos
      setERs
    } else
      ReferencedElementInfos.None

  override final protected def calcValueUnparserReferencedElementInfos =
    if (this.outputValueCalcOption.isDefined)
      ovcCompiledExpression.valueReferencedElementInfos
    else
      ReferencedElementInfos.None

  final lazy val isReferencedByContentLengthParserExpressions: Boolean =
    rootElement.get.contentLengthParserReferencedElementInfos.contains(this.dpathElementCompileInfo)

  final lazy val isReferencedByContentLengthUnparserExpressions: Boolean =
    rootElement.get.contentLengthUnparserReferencedElementInfos.contains(this.dpathElementCompileInfo)

  final lazy val isReferencedByValueLengthParserExpressions: Boolean = {
    val setElems = rootElement.get.valueLengthParserReferencedElementInfos
    //    if (this eq rootElement.get)
    //      println("PARSER these are referenced by valueCalc: " + setElems.toString)
    val res = setElems.contains(this.dpathElementCompileInfo)
    res
  }

  final lazy val isReferencedByValueLengthUnparserExpressions: Boolean = {
    val setElems = rootElement.get.valueLengthUnparserReferencedElementInfos
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

  final lazy val simpleType = {
    Assert.usage(isSimpleType)
    typeDef.asInstanceOf[SimpleTypeBase]
  }

  final lazy val optPrimType: Option[PrimType] = Misc.boolToOpt(isSimpleType, primType) // .typeRuntimeData)

  def isScalar: Boolean
  def isRequiredArrayElement: Boolean

  // override in Particle
  lazy val optMinOccurs: Option[Int] = None
  lazy val optMaxOccurs: Option[Int] = None

  def elementRef: Option[ElementRef]

  final override lazy val dpathCompileInfo = dpathElementCompileInfo

  lazy val dpathElementCompileInfo: DPathElementCompileInfo = {
    val eci = new DPathElementCompileInfo(
      enclosingElement.map { _.dpathElementCompileInfo },
      variableMap,
      namespaces,
      slashPath,
      slotIndexInParent,
      name,
      isArray,
      namedQName,
      optPrimType,
      schemaFileLocation,
      elementChildrenCompileInfo)
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

  //  private lazy val containsNoNamespaceElement: Boolean = {
  //    thisElementsNamespace.isNoNamespace ||
  //      elementChildren.exists { _.containsNoNamespaceElement }
  //  }
  //
  //  private def bindingsHaveDefaultNamespaceBinding(pairs: Set[(String, NS)]) = {
  //    val map = pairs.toMap
  //    val optDefaultNS = map.get(null)
  //    val res = optDefaultNS.isDefined
  //    res
  //  }

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
      val res = NamespaceBinding(pre, ns.toString, parentNSBinding)
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
      if (enclosingComponent.isEmpty) {
        // If this is the root element and it contains xmlns="", then remove
        // it. xmlns="" is implied at the root. Note that we only do this for
        // the root because if a child has xmlns="", that implies the parent
        // had set the default namespace to something else, so we need to keep
        // the child xmlns="" to override that.
        myUniquePairs.filterNot { case (prefix, ns) => prefix == null && ns.isNoNamespace }
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
          val value = Infoset.convertToInfosetRepType(
            primType,
            str, this)
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
      case _ => new SeveralPossibilitiesForNextElement(schemaFileLocation, eltMap, resolverType)
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

  lazy val elementRuntimeData: ElementRuntimeData = LV('elementRuntimeData) {
    val ee = enclosingElement
    //
    // Must be lazy below, because we are defining the elementRuntimeData in terms of
    // the elementRuntimeData of its enclosing element. This backpointer must be
    // constructed lazily so that we first connect up all the erds to their children,
    // and only subsequently ask for these parents to be elaborated.
    //
    lazy val optERD = ee.map { enc =>
      Assert.invariant(this != enc)
      enc.elementRuntimeData
    }
    lazy val maybeTRD = this.enclosingTerm.map { enc =>
      Assert.invariant(this != enc)
      enc.termRuntimeData
    }
    createElementRuntimeData(optERD, maybeTRD)
  }.value

  def erd = elementRuntimeData // just an abbreviation

  /**
   * Everything needed at runtime about the element
   * in order to compile expressions using it, (for debug)
   * and issue proper diagnostics in error situations.
   */
  private def createElementRuntimeData(parent: => Option[ElementRuntimeData],
    parentTerm: => Maybe[TermRuntimeData]): ElementRuntimeData = {

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
      parentTerm,
      childrenERDs,
      schemaSet.variableMap,
      nextElementResolver,
      childElementResolver,
      encodingInfo,
      dpathElementCompileInfo,
      schemaFileLocation,
      prettyName,
      path,
      namespaces,
      minimizedScope,
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
      false, // !isReferencedByExpressions, // assume it is always to be referenced by expressions
      optTruncateSpecifiedLengthString,
      if (isOutputValueCalc) Some(ovcCompiledExpression) else None)
    newERD
  }

  private lazy val optTruncateSpecifiedLengthString =
    Option(truncateSpecifiedLengthString =:= YesNo.Yes)
  // because of the way text numbers are unparsed, we don't know that
  // the string is for a text number. So we need this property for numbers and other text
  // simple types also.
  //    if (isSimpleType && simpleType.primitiveType.isInstanceOf[NodeInfo.String.Kind]) {
  //      Option(truncateSpecifiedLengthString =:= YesNo.Yes)
  //    } else None // don't need this property for non-strings

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
   * all the slot space in these objects. This would still be constant-time access,
   * just with a larger constant overhead. It's a time/space tradeoff.
   */
  final lazy val nChildSlots: Int = {
    if (isSimpleType) 0
    else elementChildren.length
  }

  final lazy val slotIndexInParent: Int = {
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
  final lazy val elementChildren: Seq[ElementBase] = {
    this.typeDef match {
      case ct: ComplexTypeBase => {
        ct.group.elementChildren.asInstanceOf[Seq[ElementBase]]
      }
      case _ => Nil
    }
  }

  final lazy val elementChildrenCompileInfo = elementChildren.map { _.dpathElementCompileInfo }

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
    rep
  }

  final override lazy val couldHaveText: Boolean = {
    hasDelimiters ||
      (isSimpleType && impliedRepresentation == Representation.Text) ||
      (isComplexType && elementComplexType.group.couldHaveText)
  }

  final override lazy val termChildren: Seq[Term] = {
    if (isSimpleType) Nil
    else Seq(elementComplexType.group)
  }

  final lazy val isParentUnorderedSequence: Boolean = {
    parent match {
      case s: Sequence if !s.isOrdered => true
      case _ => false
    }
  }

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => Some(new DFDLElement(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final def emptyFormatFactory = new DFDLElement(newDFDLAnnotationXML("element"), this)

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLElement]

  private def getImplicitAlignmentInBits(thePrimType: PrimType, theRepresentation: Representation): Int = {
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

  private lazy val implicitAlignmentInBits: Int = getImplicitAlignmentInBits(primType, impliedRepresentation)

  final lazy val alignmentValueInBits: JInt = {
    alignment match {
      case AlignmentType.Implicit => {
        if (this.isComplexType) {
          val ct = this.elementComplexType
          ct.alignmentValueInBits
        } else implicitAlignmentInBits
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
      (isDefinedNilValue && (isSimpleType && (simpleType.primitiveType =:= PrimType.String || simpleType.primitiveType =:= PrimType.HexBinary) && !hasESNilValue)))

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

  // 11/1/2012 - moved to base since needed by patternValue
  final lazy val isPrimType = typeDef.isInstanceOf[PrimType]

  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  private lazy val hasPattern: Boolean = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasPattern
    } else { false }
  }
  private lazy val hasEnumeration: Boolean = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasEnumeration
    } else { false }
  }

  protected lazy val hasMinLength = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinLength
    } else { false }
  }

  protected lazy val hasMaxLength = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxLength
    } else { false }
  }

  private lazy val hasMinInclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinInclusive
    } else { false }
  }

  private lazy val hasMaxInclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxInclusive
    } else { false }
  }

  private lazy val hasMinExclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMinExclusive
    } else { false }
  }

  private lazy val hasMaxExclusive = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasMaxExclusive
    } else { false }
  }

  private lazy val hasTotalDigits = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasTotalDigits
    } else { false }
  }

  private lazy val hasFractionDigits = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      st.hasFractionDigits
    } else { false }
  }

  final lazy val patternValues: Seq[FacetValueR] = {
    if (isSimpleType && !isPrimType) {
      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
      if (st.hasPattern) {
        val pt = st.primitiveType
        if (pt != PrimType.String) SDE("Pattern is only allowed to be applied to string and types derived from string.")
        st.patternValues
      } else SDE("Pattern was not found in this context.")
    } else SDE("Pattern was asked for when isSimpleType(%s) and isPrimType(%s)", isSimpleType, isPrimType)
  }

  private lazy val enumerationValues: String = {
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
  final lazy val (minLength: java.math.BigDecimal, maxLength: java.math.BigDecimal) = computeMinMaxLength
  // TODO: why are we using java.math.BigDecimal, when scala has a much
  // nicer decimal class?
  private val zeroBD = new java.math.BigDecimal(0)
  private val unbBD = new java.math.BigDecimal(-1) // TODO: should this be a tunable limit?

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
  private lazy val minInclusive: java.math.BigDecimal = {
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

  private lazy val maxInclusive: java.math.BigDecimal = {
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

  private lazy val minExclusive: java.math.BigDecimal = {
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

  private lazy val maxExclusive: java.math.BigDecimal = {
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

  private lazy val totalDigits: java.math.BigDecimal = {
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

  private lazy val fractionDigits: java.math.BigDecimal = {
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

  //  private lazy val allFacets: Seq[FacetValue] = {
  //    if (isSimpleType && !isPrimType) {
  //      val st = elementSimpleType.asInstanceOf[SimpleTypeDefBase]
  //      st.combinedBaseFacets
  //    } else scala.collection.mutable.Seq.empty
  //  }

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

  protected final def possibleFirstChildTerms: Seq[Term] = termChildren

  protected final def couldBeLastElementInModelGroup: Boolean = LV('couldBeLastElementInModelGroup) {
    val couldBeLast = enclosingTerm match {
      case None => true
      case Some(s: Sequence) if s.isOrdered => {
        !possibleNextSiblingTerms.exists {
          case e: ElementBase => !e.isOptional || e.isRequiredArrayElement
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
      }
      case _ => true
    }
    couldBeLast
  }.value

  final lazy val nextParentElements: Seq[ElementBase] = {
    if (enclosingTerm.isDefined && couldBeLastElementInModelGroup) {
      enclosingTerm.get.asInstanceOf[ModelGroup].possibleNextChildElementsInInfoset
    } else {
      Nil
    }
  }

  protected final lazy val defaultParseUnparsePolicy = optionParseUnparsePolicy.getOrElse(ParseUnparsePolicy.Both)

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

}
