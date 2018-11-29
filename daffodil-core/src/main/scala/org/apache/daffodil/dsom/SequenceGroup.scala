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

import scala.xml.Elem
import scala.xml.Node
import scala.xml._
import org.apache.daffodil.schema.annotation.props.gen.Sequence_AnnotationMixin
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicyMixin
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.schema.annotation.props.gen.SequenceKind
import org.apache.daffodil.Implicits.ns2String
import org.apache.daffodil.grammar.SequenceGrammarMixin
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.processors.LayerTransformerEv
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.processors.SeparatorParseEv
import org.apache.daffodil.processors.ModelGroupRuntimeData
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.processors.SeparatorUnparseEv
import org.apache.daffodil.exceptions.Assert

/**
 * Base for anything sequence-like.
 *
 * Sequences, group refs to sequences, and the implied sequences
 * that are choice branches, are all instances.
 */
abstract class SequenceTermBase(
  final override val xml: Node,
  final override val parent: SchemaComponent,
  final override val position: Int)
  extends ModelGroup
  with SequenceGrammarMixin {

  def separatorSuppressionPolicy: SeparatorSuppressionPolicy

  def sequenceKind: SequenceKind

  def separatorPosition: SeparatorPosition

  def isLayered: Boolean

  def separatorParseEv: SeparatorParseEv

  def separatorUnparseEv: SeparatorUnparseEv

  def sequenceRuntimeData: SequenceRuntimeData

  def layerLengthUnits: LayerLengthUnits

  def isOrdered: Boolean

  def maybeLayerTransformerEv: Maybe[LayerTransformerEv]

  def checkHiddenSequenceIsDefaultableOrOVC: Unit

}

/**
 * Base for anything sequence-like that actually
 * has the sequence properties. So actual sequences, group refs to them,
 * but NOT implied sequences inside choice branches.
 */
abstract class SequenceGroupTermBase(
  xml: Node,
  parent: SchemaComponent,
  position: Int)
  extends SequenceTermBase(xml, parent, position)
  with Sequence_AnnotationMixin
  with SequenceRuntimeValuedPropertiesMixin
  with SeparatorSuppressionPolicyMixin
  with LayeringRuntimeValuedPropertiesMixin {

  requiredEvaluations(checkIfValidUnorderedSequence)
  requiredEvaluations(modelGroupRuntimeData.preSerialization)

  protected def apparentXMLChildren: Seq[Node]

  final override lazy val myPeers = sequencePeers

  final override lazy val hasDelimiters = hasInitiator || hasTerminator || hasSeparator

  protected def hiddenGroupRefOption: PropertyLookupResult

  /**
   * True if this sequence group has syntactic features itself or
   * within itself.
   */
  final lazy val hasStaticallyRequiredOccurrencesInDataRepresentation = {
    // true if there are syntactic features
    hasInitiator || hasTerminator ||
      // or if any child of the sequence has statically required instances.
      groupMembers.exists { _.hasStaticallyRequiredOccurrencesInDataRepresentation }
  }

  final override def hasKnownRequiredSyntax = LV('hasKnownRequiredSyntax) {
    lazy val memberHasRequiredSyntax = groupMembers.exists(_.hasKnownRequiredSyntax)
    lazy val prefixOrPostfixAndStaticallyRequiredInstance =
      groupMembers.filter { _.isRepresented }.exists { _.hasStaticallyRequiredOccurrencesInDataRepresentation } &&
        (hasPrefixSep || hasPostfixSep)
    lazy val infixAnd2OrMoreStaticallyRequiredInstances =
      groupMembers.filter { m => m.isRepresented && m.hasStaticallyRequiredOccurrencesInDataRepresentation }.length > 1 && hasInfixSep
    lazy val sepAndArryaWith2OrMoreStaticallyRequiredInstances =
      groupMembers.filter { m =>
        m.isRepresented && m.hasStaticallyRequiredOccurrencesInDataRepresentation && (m match {
          case e: ElementBase => e.minOccurs > 1
          case _ => false
        })
      }.length > 0 && hasSeparator
    val res =
      hasInitiator ||
        hasTerminator ||
        memberHasRequiredSyntax ||
        prefixOrPostfixAndStaticallyRequiredInstance ||
        infixAnd2OrMoreStaticallyRequiredInstances ||
        sepAndArryaWith2OrMoreStaticallyRequiredInstances
    res
  }.value

  /**
   * Provides unordered sequence checks.  Will SDE if invalid.
   */
  protected final def checkIfValidUnorderedSequence(): Unit = {
    if (!isOrdered) {
      checkMembersAreAllElementOrElementRef
      checkMembersHaveValidOccursCountKind
      checkMembersHaveUniqueNamesInNamespaces
    }
  }

  private def checkMembersHaveValidOccursCountKind: Unit = {
    val validChildren: Seq[ElementBase] =
      groupMembers.filter { m => m.isInstanceOf[LocalElementDecl] || m.isInstanceOf[ElementRef]
      }.map(_.asInstanceOf[ElementBase])

    val invalidChildren = validChildren.filter(e => {
      if (e.minOccurs == 0 | !e.isScalar) {
        e.occursCountKind match {
          case OccursCountKind.Parsed => false
          case _ => true
        }
      } else false
    })
    val hasInvalidChildren = invalidChildren.length > 0
    if (hasInvalidChildren)
      this.SDE("Members of an unordered sequence (%s) that are optional or array elements must have dfdl:occursCountKind='parsed'." +
        "\nThe offending members: %s.", this.path, invalidChildren.mkString(","))
  }

  private def checkMembersAreAllElementOrElementRef: Unit = {
    val invalidChildren = groupMembers.filterNot(child =>
      child.isInstanceOf[LocalElementDecl] || child.isInstanceOf[ElementRef])
    val hasInvalidChildren = invalidChildren.length > 0
    if (hasInvalidChildren)
      this.SDE("Members of an unordered sequence (%s) must be Element or ElementRef." +
        "\nThe offending members: %s.", this.path, invalidChildren.mkString(","))
  }

  private def checkMembersHaveUniqueNamesInNamespaces: Unit = {
    val childrenGroupedByNamespace =
      groupMembers.filter(m => m.isInstanceOf[ElementBase]).map(_.asInstanceOf[ElementBase]).groupBy(_.targetNamespace)

    childrenGroupedByNamespace.foreach {
      case (ns, children) => {
        // At this point we're looking at the individual namespace buckets
        val childrenGroupedByName = children.groupBy(child => child.name)
        childrenGroupedByName.foreach {
          case (name, children) =>
            // Now we're looking at the individual name buckets within the
            // individual namespace bucket.
            if (children.length > 1)
              this.SDE(
                "Two or more members of the unordered sequence (%s) have the same name and the same namespace." +
                  "\nNamespace: %s\tName: %s.",
                this.path, ns, name)
        }
      }
    }
  }

  def checkHiddenSequenceIsDefaultableOrOVC: Unit = {
    if (this.isHidden) {
      val nonDefaultableOrOVC =
        this.childrenInHiddenGroupNotDefaultableOrOVC
      if (nonDefaultableOrOVC.length > 0) {
        this.SDE("Element(s) of hidden group must define dfdl:outputValueCalc:\n%s", nonDefaultableOrOVC.mkString("\n"))
      }
    }
  }

  final lazy val isOrdered: Boolean = this.sequenceKind match {
    case SequenceKind.Ordered => true
    case SequenceKind.Unordered => false
  }

  final lazy val modelGroupRuntimeData = sequenceRuntimeData

  final lazy val sequenceRuntimeData = {
    new SequenceRuntimeData(
      schemaSet.variableMap,
      encodingInfo,
      // elementChildren.map { _.elementRuntimeData.dpathElementCompileInfo },
      schemaFileLocation,
      dpathCompileInfo,
      diagnosticDebugName,
      path,
      namespaces,
      defaultBitOrder,
      groupMembersRuntimeData,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      optIgnoreCase,
      maybeFillByteEv,
      maybeCheckByteAndBitOrderEv,
      maybeCheckBitOrderAndCharset)
  }

  private val layeredSequenceAllowedProps = Set("ref", "layerTransform", "layerEncoding", "layerLengthKind", "layerLength", "layerLengthUnits", "layerBoundaryMark")

  final lazy val maybeLayerTransformerEv: Maybe[LayerTransformerEv] = {
    if (maybeLayerTransformEv.isEmpty) Maybe.Nope
    else { // need to check that only layering properties are specified
      val localProps = this.formatAnnotation.justThisOneProperties
      val localKeys = localProps.keySet
      val disallowedKeys = localKeys.filterNot(k => layeredSequenceAllowedProps.contains(k))
      if (disallowedKeys.size > 0)
        SDE("Sequence has dfdl:layerTransform specified, so cannot have non-layering properties: %s", disallowedKeys.mkString(", "))

      val lt = new LayerTransformerEv(
        maybeLayerTransformEv.get,
        maybeLayerCharsetEv,
        Maybe.toMaybe(optionLayerLengthKind),
        maybeLayerLengthInBytesEv,
        Maybe.toMaybe(optionLayerLengthUnits),
        maybeLayerBoundaryMarkEv,
        termRuntimeData)
      lt.compile()
      Maybe.One(lt)
    }
  }

  final def isLayered = maybeLayerTransformerEv.isDefined

}

/**
 * Captures concepts associated with definitions of Sequence groups.
 *
 * Used by GlobalSequenceGroupDef and local Sequence, but not by SequenceGroupRef.
 * Used on objects that can carry DFDLSequence annotation objects.
 */
trait SequenceDefMixin
  extends AnnotatedSchemaComponent
  with GroupDefLike {

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => Some(new DFDLSequence(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final def emptyFormatFactory = new DFDLSequence(newDFDLAnnotationXML("sequence"), this)

  final lazy val <sequence>{ apparentXMLChildren @ _* }</sequence> = (xml \\ "sequence")(0)

  final def xmlChildren = apparentXMLChildren

  // The dfdl:hiddenGroupRef property cannot be scoped, nor defaulted. It's really a special
  // attribute, not a format property in the usual sense.
  // So we retrieve it by this lower-level mechanism which only combines short and long form.
  //
  final lazy val hiddenGroupRefOption = LV('hiddenGroupRefOption) {
    findPropertyOptionThisComponentOnly("hiddenGroupRef")
  }.value

}

/**
 * Represents a local sequence definition.
 */
class Sequence(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends SequenceGroupTermBase(xmlArg, parent, position)
  with SequenceDefMixin {

  requiredEvaluations(checkHiddenGroupRefHasNoChildren)

  override lazy val optReferredToComponent = None

  private lazy val checkHiddenGroupRefHasNoChildren = {
    if (hiddenGroupRefOption.isDefined) {
      val axc = apparentXMLChildren
      val len = axc.length
      schemaDefinitionUnless(len == 0, "A sequence with hiddenGroupRef cannot have children.")
    }
  }

  final lazy val hiddenGroupRefXML = LV('hiddenGroupRefXML) {
    val Found(qname, _, _, _) = hiddenGroupRefOption
    // synthesize a group reference here.
    val contextScope = xml.asInstanceOf[Elem].scope
    val hgr = {
      (<xs:group xmlns:xs={ XMLUtils.xsdURI } ref={ qname }/>).copy(scope = contextScope)
    }
    hgr
  }.value
}

/**
 * For the case when a choice branch happens to be a local element decl or element ref
 * with varying or multiple occurrences. In that case we encapsulate them
 * with this so that we have a compiler invariant that all terms have an enclosing sequence.
 *
 * There can't be separators, but the driving of the iteration for the occurrences, is
 * in the sequence parsers/unparsers. So this implements that, leveraging the
 * sequence machinery. In effect this is specifying the properties needed to ensure it is
 * handled as a degenerate sequence having only one element decl within it.
 */
final class ChoiceBranchImpliedSequence(rawGM: Term)
  extends SequenceTermBase(rawGM.xml, rawGM.parent, rawGM.position)
  with GroupDefLike {

  override def separatorSuppressionPolicy: SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingEmptyStrict

  override def sequenceKind: SequenceKind = SequenceKind.Ordered

  override def separatorPosition: SeparatorPosition = SeparatorPosition.Infix

  override def isLayered: Boolean = false

  override lazy val hasSeparator = false
  override lazy val hasTerminator = false
  override lazy val hasInitiator = false

  override def separatorParseEv: SeparatorParseEv = Assert.usageError("Not to be called on choice branches.")

  override def separatorUnparseEv: SeparatorUnparseEv = Assert.usageError("Not to be called on choice branches.")

  override def layerLengthUnits: LayerLengthUnits = Assert.usageError("Not to be called for choice branches.")

  override def isOrdered = true

  override def maybeLayerTransformerEv: Maybe[LayerTransformerEv] = Maybe.Nope

  override def checkHiddenSequenceIsDefaultableOrOVC: Unit = ()

  override lazy val sequenceRuntimeData: SequenceRuntimeData = {
    new SequenceRuntimeData(
      schemaSet.variableMap,
      encodingInfo,
      schemaFileLocation,
      dpathCompileInfo,
      diagnosticDebugName,
      path,
      namespaces,
      defaultBitOrder,
      groupMembersRuntimeData,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      true,
      None,
      Maybe.Nope,
      Maybe.Nope,
      Maybe.Nope)
  }

  /**
   * Implied sequence doesn't exist textually, so can't have properties on it.
   */
  override lazy val nonDefaultPropertySources: Seq[ChainPropProvider] = groupMembers(0).nonDefaultPropertySources

  // Members declared in AnnotatedMixin
  protected def annotationFactory(node: scala.xml.Node): Option[DFDLAnnotation] = None
  protected def emptyFormatFactory: DFDLFormatAnnotation = Assert.usageError("Not to be called on choice branches.")
  protected def isMyFormatAnnotation(a: DFDLAnnotation): Boolean = false

  // Members declared in AnnotatedSchemaComponent
  protected def optReferredToComponent: Option[AnnotatedSchemaComponent] = None

  def modelGroupRuntimeData: ModelGroupRuntimeData = sequenceRuntimeData

  protected def myPeers: Option[Seq[ModelGroup]] = None

  def xmlChildren: Seq[scala.xml.Node] = Seq(xml)

  // Members declared in Term
  def hasStaticallyRequiredOccurrencesInDataRepresentation: Boolean = groupMembers(0).hasStaticallyRequiredOccurrencesInDataRepresentation

}
