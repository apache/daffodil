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
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.processors.LayerTransformerEv
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.processors.SeparatorParseEv
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.processors.SeparatorUnparseEv
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.runtime1.ChoiceBranchImpliedSequenceRuntime1Mixin
import org.apache.daffodil.schema.annotation.props.FindPropertyMixin

/**
 * Base for anything sequence-like.
 *
 * Sequences, group refs to sequences, and the implied sequences
 * that are choice branches, are all instances.
 */
abstract class SequenceTermBase(
  final override val xml: Node,
  final override val optLexicalParent: Option[SchemaComponent],
  final override val position: Int)
  extends ModelGroup(position)
  with SequenceGrammarMixin {

  def separatorSuppressionPolicy: SeparatorSuppressionPolicy

  def sequenceKind: SequenceKind

  def separatorPosition: SeparatorPosition

  def isLayered: Boolean

  def separatorParseEv: SeparatorParseEv

  def separatorUnparseEv: SeparatorUnparseEv

  def layerLengthUnits: LayerLengthUnits

  def isOrdered: Boolean

  def maybeLayerTransformerEv: Maybe[LayerTransformerEv]

}

/**
 * Base for anything sequence-like that actually
 * has the sequence properties. So actual sequences, group refs to them,
 * but NOT implied sequences inside choice branches.
 */
abstract class SequenceGroupTermBase(
  xml: Node,
  lexicalParent: SchemaComponent,
  position: Int)
  extends SequenceTermBase(xml, Option(lexicalParent), position)
  with Sequence_AnnotationMixin
  with SequenceRuntimeValuedPropertiesMixin
  with SeparatorSuppressionPolicyMixin
  with LayeringRuntimeValuedPropertiesMixin {


  requiredEvaluationsIfActivated(checkIfValidUnorderedSequence)

  protected def apparentXMLChildren: Seq[Node]

  final override lazy val hasDelimiters = hasInitiator || hasTerminator || hasSeparator

  protected def hiddenGroupRefOption: PropertyLookupResult

  /**
   * When a sequence has prefix or postfix separator and must have one or more children,
   * then we know a separator MUST appear in the data. Statically this can be
   * determined.
   */
  private lazy val prefixOrPostfixAndStaticallyRequiredInstance =
    (hasPrefixSep || hasPostfixSep) &&
      representedMembers.exists { member =>
        val res = member.hasStaticallyRequiredOccurrencesInDataRepresentation
        res
      }

  /**
   * When a sequence has infix separator, and must have two or more children, then
   * we know a separator MUST appear in the data stream.
   */
  private lazy val infixAndTwoOrMoreStaticallyRequiredInstances =
    hasInfixSep && (representedMembers.filter { m =>
      val res = m.hasStaticallyRequiredOccurrencesInDataRepresentation
      res
    }.length >= 2)

  private lazy val sepAndArrayaWithTwoOrMoreStaticallyRequiredInstances =
    hasSeparator && (representedMembers.filter { m =>
      val res = m.hasStaticallyRequiredOccurrencesInDataRepresentation && (m match {
        case e: ElementBase => e.minOccurs >= 2
        case _ => false
      })
      res
    }.length > 0)

  final override lazy val hasKnownRequiredSyntax = LV('hasKnownRequiredSyntax) {
    if (hasFraming) true
    else {
      lazy val memberHasRequiredSyntax = representedMembers.exists { member =>
        val instancesAreRequired = member.hasStaticallyRequiredOccurrencesInDataRepresentation
        val hasKnownRequiredSyntax = member.hasKnownRequiredSyntax
        val res = instancesAreRequired && hasKnownRequiredSyntax
        res
      }
      val res =
        memberHasRequiredSyntax ||
          prefixOrPostfixAndStaticallyRequiredInstance ||
          infixAndTwoOrMoreStaticallyRequiredInstances ||
          sepAndArrayaWithTwoOrMoreStaticallyRequiredInstances
      res
    }
  }.value

  /**
   * Provides unordered sequence checks.  Will SDE if invalid.
   */
  protected final lazy val checkIfValidUnorderedSequence: Unit = {
    if (!isOrdered) {
      checkMembersAreAllElementOrElementRef
      checkMembersHaveValidOccursCountKind
      checkMembersHaveUniqueNamesInNamespaces
    }
  }

  private lazy val checkMembersHaveValidOccursCountKind: Unit = {
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

  private lazy val checkMembersAreAllElementOrElementRef: Unit = {
    val invalidChildren = groupMembers.filterNot(child =>
      child.isInstanceOf[LocalElementDecl] || child.isInstanceOf[ElementRef])
    val hasInvalidChildren = invalidChildren.length > 0
    if (hasInvalidChildren)
      this.SDE("Members of an unordered sequence (%s) must be Element or ElementRef." +
        "\nThe offending members: %s.", this.path, invalidChildren.mkString(","))
  }

  private lazy val checkMembersHaveUniqueNamesInNamespaces: Unit = {
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

  final lazy val isOrdered: Boolean = this.sequenceKind match {
    case SequenceKind.Ordered => true
    case SequenceKind.Unordered => false
  }

  private val layeredSequenceAllowedProps = Set("ref", "layerTransform", "layerEncoding", "layerLengthKind", "layerLength", "layerLengthUnits", "layerBoundaryMark")

  final lazy val maybeLayerTransformerEv: Maybe[LayerTransformerEv] = {
    if (maybeLayerTransformEv.isEmpty) Maybe.Nope
    else { // need to check that only layering properties are specified
      val localProps = this.formatAnnotation.justThisOneProperties
      val localKeys = localProps.keySet
      val disallowedKeys = localKeys.filterNot(k => layeredSequenceAllowedProps.contains(k))
      if (disallowedKeys.size > 0)
        SDE("Sequence has dfdlx:layerTransform specified, so cannot have non-layering properties: %s", disallowedKeys.mkString(", "))

      val lt = new LayerTransformerEv(
        maybeLayerTransformEv.get,
        maybeLayerCharsetEv,
        Maybe.toMaybe(optionLayerLengthKind),
        maybeLayerLengthInBytesEv,
        Maybe.toMaybe(optionLayerLengthUnits),
        maybeLayerBoundaryMarkEv,
        dpathCompileInfo)
      lt.compile(tunable)
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
  with GroupDefLike
  with FindPropertyMixin {

  def groupMembersNotShared: Seq[Term]

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => Some(new DFDLSequence(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final def emptyFormatFactory = new DFDLSequence(newDFDLAnnotationXML("sequence"), this)

  final lazy val <sequence>{ apparentXMLChildren @ _* }</sequence> = (xml \\ "sequence")(0)

  def xmlChildren = apparentXMLChildren

  // The dfdl:hiddenGroupRef property cannot be scoped, nor defaulted. It's really a special
  // attribute, not a format property in the usual sense.
  // So we retrieve it by this lower-level mechanism which only combines short and long form.
  //
  final lazy val hiddenGroupRefOption = LV('hiddenGroupRefOption) {
    findPropertyOption("hiddenGroupRef")
  }.value

}

/**
 * Represents a local sequence definition.
 */
final class Sequence(xmlArg: Node, lexicalParent: SchemaComponent, position: Int)
  extends SequenceGroupTermBase(xmlArg, lexicalParent, position)
  with SequenceDefMixin {


  requiredEvaluationsIfActivated(checkHiddenGroupRefHasNoChildren)

  override lazy val optReferredToComponent = None

  lazy val checkHiddenGroupRefHasNoChildren = {
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
  extends SequenceTermBase(rawGM.xml, rawGM.optLexicalParent, rawGM.position)
  with SequenceDefMixin
  with ChoiceBranchImpliedSequenceRuntime1Mixin {

  override final protected lazy val groupMembersDef: Seq[Term] = Seq(rawGM)

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

  override def findPropertyOption(pname: String): PropertyLookupResult = rawGM.findPropertyOption(pname)

  /**
   * Implied sequence doesn't exist textually, so can't have properties on it.
   */
  override lazy val nonDefaultPropertySources: Seq[ChainPropProvider] = Seq()

  // Members declared in AnnotatedSchemaComponent
  protected def optReferredToComponent: Option[AnnotatedSchemaComponent] = None

  final override def xmlChildren: Seq[scala.xml.Node] = Seq(xml)

  // Members declared in Term
  def hasKnownRequiredSyntax: Boolean = groupMembers(0).hasKnownRequiredSyntax

}
