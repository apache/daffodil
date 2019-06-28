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

import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Text
import scala.xml._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ModelGroupRuntimeData
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.grammar.ModelGroupGrammarMixin
import org.apache.daffodil.infoset.ChoiceBranchEvent
import org.apache.daffodil.infoset.ChoiceBranchStartEvent
import org.apache.daffodil.infoset.ChoiceBranchEndEvent
import java.lang.{ Integer => JInt, Boolean => JBoolean }
import org.apache.daffodil.schema.annotation.props.AlignmentType
import org.apache.daffodil.schema.annotation.props.gen.AlignmentUnits
import org.apache.daffodil.schema.annotation.props.gen.YesNo

/**
 * A factory for model groups.
 *
 * Takes care of detecting group references, and constructing the
 * proper SequenceGroupRef or ChoiceGroupRef object.
 */
object ModelGroupFactory {

  /**
   * Returns a Model Group.
   *
   * Non Model Group types are/should be handled by the caller.
   *
   */
  def apply(child: Node, lexicalParent: SchemaComponent, position: Int, isHidden: Boolean,
    nodesAlreadyTrying: Set[Node] = Set()): ModelGroup = {
    if (nodesAlreadyTrying.contains(child)) {
      //
      // We are chasing our tail. Circular reference among named model groups/terms.
      //
      lexicalParent.schemaDefinitionError("Model group circular definitions. Group references, or hidden group references form a loop.")
    } else {
      val moreNodesAlreadyTrying = nodesAlreadyTrying + child
      val res =
        makeModelGroup(child, lexicalParent, position, isHidden, nodesAlreadyTrying)
      res
    }
  }

  private def makeModelGroup(child: Node, lexicalParent: SchemaComponent, position: JInt, isHidden: Boolean,
    nodesAlreadyTrying: Set[Node]): ModelGroup = {
    val childModelGroup: ModelGroup = child match {
      case <sequence>{ _* }</sequence> => {
        val seq = new Sequence(child, lexicalParent, position)
        if (seq.hiddenGroupRefOption.isDefined) {
          //
          // construct the group ref XML, then recursively process that,
          // but set flag so it will be hidden.
          //
          val hgrXML = seq.hiddenGroupRefXML
          ModelGroupFactory(hgrXML, lexicalParent, position, true, nodesAlreadyTrying)
        } else {
          seq
        }
      }
      case <choice>{ _* }</choice> => new Choice(child, lexicalParent, position)
      case <group>{ _* }</group> => {
        val pos: Int = lexicalParent match {
          case ct: ComplexTypeBase => 1
          case mg: ModelGroup => position
          case gd: GlobalGroupDef => position
        }
        val isH = isHidden || lexicalParent.isHidden
        val groupRefFactory = new GroupRefFactory(child, lexicalParent, pos, isH)
        val groupRefInstance = groupRefFactory.groupRef
        groupRefInstance.asModelGroup
      }
      case _ => {
        Assert.invariantFailed("Unrecognized construct %s should be handled by caller.".format(child))
      }
    }
    childModelGroup
  }
}

/**
 * Factory for Terms
 */
object TermFactory {

  /**
   * Returns a Term.
   *
   * Non Term/Model Group elements are/should be taken care of by the caller.
   *
   */
  def apply(child: Node, lexicalParent: GroupDefLike, position: Int, nodesAlreadyTrying: Set[Node] = Set()) = {
    val childTerm: Term = child match {
      case <element>{ _* }</element> => {
        val refProp = child.attribute("ref").map { _.text }
        // must get an unprefixed attribute name, i.e. ref='foo:bar', and not
        // be tripped up by dfdl:ref="fmt:fooey" which is a format reference.
        refProp match {
          case None => {
            val eDecl = new LocalElementDecl(child, lexicalParent, position)
            eDecl
          }
          case Some(_) => new ElementRef(child, lexicalParent, position)
        }
      }
      case _ => ModelGroupFactory(child, lexicalParent, position, false, nodesAlreadyTrying)
    }
    childTerm
  }
}

/**
 * Base class for all model groups, which are term containers.
 *
 * There are ultimately 4 concrete classes that implement this:
 * Sequence, Choice, SequenceGroupRef, and ChoiceGroupRef
 */
abstract class ModelGroup(index: Int)
  extends Term
  with ModelGroupGrammarMixin
  with OverlapCheckMixin
  with NestingLexicalMixin {

  requiredEvaluations(groupMembers)
  requiredEvaluations(initiatedContentCheck)

  /**
   * FIXME: DAFFODIL-2132. This tells us if framing is expressed on the schema.
   * It does NOT tell us if that framing occupies bits in the data stream or not.
   */
  final lazy val hasFraming =
    hasInitiator ||
      hasTerminator ||
      !hasNoSkipRegions

  final lazy val hasStaticallyRequiredOccurrencesInDataRepresentation = {
    hasFraming ||
      // or if all arms of the choice have statically required instances.
      groupMembers.forall { _.hasStaticallyRequiredOccurrencesInDataRepresentation }
  }

  def groupMembers: Seq[Term]

  final lazy val representedMembers = groupMembers.filter { _.isRepresented }

  def xmlChildren: Seq[Node]

  final override def isScalar = true
  final override def isOptional = false
  final override def isArray = false

  private def prettyIndex = "[" + index + "]" // 1-based indexing in XML/XSD

  override lazy val diagnosticDebugName = prettyBaseName + prettyIndex

  override lazy val alignmentValueInBits: JInt = {
    this.alignment match {
      case AlignmentType.Implicit => this.alignmentUnits match {
        case AlignmentUnits.Bits => 1
        case AlignmentUnits.Bytes => 8
      }
      case align: JInt => this.alignmentUnits match {
        case AlignmentUnits.Bits => align
        case AlignmentUnits.Bytes => 8 * align
      }
    }
  }

  final lazy val elementChildren: Seq[ElementBase] = LV('elementChildren) {
    val gms = groupMembers
    val echls = gms.flatMap { gm =>
      gm match {
        case eb: ElementBase => Seq(eb)
        case gb: ModelGroup => gb.elementChildren
      }
    }
    echls
  }.value

  final override lazy val runtimeData: RuntimeData = modelGroupRuntimeData

  final override lazy val termRuntimeData: TermRuntimeData = modelGroupRuntimeData

  protected lazy val groupMembersRuntimeData = {
    val res = this match {
      case mg: ModelGroup => mg.groupMembers.map {
        _ match {
          case eb: ElementBase => eb.erd
          case t: Term => t.termRuntimeData
        }
      }
      case _ => Nil
    }
    res
  }

  def modelGroupRuntimeData: ModelGroupRuntimeData

  protected final lazy val prettyBaseName = xml.label

  final lazy val sequenceChildren = groupMembers.collect { case s: SequenceTermBase => s }
  final lazy val choiceChildren = groupMembers.collect { case s: ChoiceTermBase => s }
  final lazy val groupRefChildren = groupMembers.collect { case s: GroupRef => s }

  final override lazy val termChildren = groupMembers

  /**
   * Returns tuple, where the first is children that could be last, and the
   * second is a boolean if all children could be optional, and thus this could
   * be last
   */
  lazy val potentialLastChildren: (Seq[Term], Boolean) = {
    val (potentialLast, allOptional) = this match {
      case ch: ChoiceTermBase => (ch.groupMembers, false)
      case sq: SequenceTermBase if !sq.isOrdered => (sq.groupMembers, true) // TBD: is true correct? Are all children optional in unordered sequence?
      case sq: SequenceTermBase => {
        val maybeLast = sq.groupMembers.lastOption
        if (maybeLast.isDefined) {
          val last = maybeLast.get
          val lastIsOptional = last match {
            case mg: ModelGroup => false // model group is mandatory
            case eb: ElementBase => !eb.isRequiredInInfoset || !eb.isRepresented
          }
          if (lastIsOptional) {
            val (priorSibs, optPriorElementsIncludesThisParent) = last.potentialPriorTerms
            (last +: priorSibs, optPriorElementsIncludesThisParent.isDefined)
          } else {
            (Seq(last), false)
          }
        } else {
          (Seq(), true)
        }
      }
    }
    val potentialLastRepresented = potentialLast.filter { term =>
      term match {
        case eb: ElementBase => eb.isRepresented
        case _ => true
      }
    }
    if (potentialLastRepresented.isEmpty) {
      //If there are no children, by definition, all children are optional.
      (Seq(), true)
    } else {
      (potentialLastRepresented, allOptional)
    }
  }

  final def allSelfContainedTermsTerminatedByRequiredElement: Seq[Term] =
    LV('allSelfContainedTermsTerminatedByRequiredElement) {
      val listOfTerms = groupMembers.map(m => {
        m match {
          case e: ElementBase if !e.isRequiredInInfoset => (Seq(e) ++ e.possibleNextTerms) // A LocalElement or ElementRef
          case e: ElementBase => Seq(e)
          case mg: ModelGroup => Seq(mg)
        }
      }).flatten
      listOfTerms
    }.value

  final def identifyingEventsForChoiceBranch: Seq[ChoiceBranchEvent] = LV('identifyingEventsForChoiceBranch) {
    Assert.usage(enclosingTerm.isDefined && enclosingTerm.get.isInstanceOf[ChoiceTermBase], "identifyingElementsForChoiceBranch must only be called on children of choices")

    val childrenIdentifiers = possibleFirstChildElementsInInfoset
    val parentNextIdentifiers =
      if (!mustHaveRequiredElement) {
        enclosingTerm.get.asInstanceOf[ModelGroup].possibleNextChildElementsInInfoset
      } else {
        Nil
      }
    val startEvents = (childrenIdentifiers ++ parentNextIdentifiers).map { e =>
      ChoiceBranchStartEvent(e.namedQName)
    }

    // Look at the enclosing terms, and find either the first model group that
    // has required next sibling elements, or find an element. If we find an
    // element without finding such a model group, then the end event of that
    // element could potentially be an identifying event for this model group
    // Otherwise, only start events (either children start events next start
    // events of enclosing model groups) could identify this branch, and no end
    // event could identify this branch. Also note that if this model group
    // must have a required element (i.e. it must contribute to the infost)
    // then none of this matters, and it will not have an identifying end
    // event, since one of the child elements must appear in the infoset.
    val endEvent =
      if (mustHaveRequiredElement) {
        Nil
      } else {
        var ec = enclosingTerm.get
        while (!ec.isInstanceOf[ElementBase] &&
          !ec.asInstanceOf[ModelGroup].hasRequiredNextSiblingElement) {
          ec = ec.enclosingTerm.get
        }
        val ee = ec match {
          case e: ElementBase => Seq(ChoiceBranchEndEvent(e.namedQName))
          case mg: ModelGroup => Nil
        }
        ee
      }

    val idEvents = startEvents ++ endEvent
    idEvents
  }.value

  /*
   * Returns list of Terms that could contain the first child element in the infoset
   */
  protected final lazy val possibleFirstChildTerms: Seq[Term] = LV('possibleFirstChildTerms) {
    val firstTerms = this match {
      case c: ChoiceTermBase => groupMembers
      case s: SequenceTermBase if !s.isOrdered => groupMembers
      case s: SequenceTermBase => {
        groupMembers.headOption match {
          case None => Nil
          case Some(e: ElementBase) if e.canBeAbsentFromUnparseInfoset => {
            // this case covers optional elements, arrrays with minOccurs = 0,
            // and elements with outputValueCalc. In each of these cases, the
            // first child could be first, but so could any siblings that
            // follow it
            Seq(e) ++ e.possibleNextSiblingTerms
          }
          case Some(s: SequenceTermBase) if s.isHidden => s.possibleNextSiblingTerms
          case Some(mg: ModelGroup) if !mg.mustHaveRequiredElement => Seq(mg) ++ mg.possibleNextSiblingTerms
          case Some(e: ElementBase) => Seq(e)
          case Some(mg: ModelGroup) => Seq(mg)
        }
      }
    }
    firstTerms
  }.value

  final lazy val nextParentElements: Seq[ElementBase] = {
    Assert.invariant(enclosingTerm.isDefined)
    val et = enclosingTerm.get
    et match {
      case mg: ModelGroup if (!this.hasRequiredNextSiblingElement) =>
        mg.possibleNextChildElementsInInfoset
      case e: ElementBase =>
        // This changes the contract. It doesn't stop at an enclosing element boundary.
        // e.possibleNextChildElementsInInfoset
        Nil
      case mg: ModelGroup =>
        Nil
    }
  }

  /** Always false as model groups can't be elements.*/
  protected final def couldBeLastElementInModelGroup: Boolean = false

  /*
   * Determines if any of the of the terms that could be next have or are
   * required elements. This essentially determines if this could contain
   * the last element in the model group.
   */
  private def hasRequiredNextSiblingElement: Boolean = LV('hasRequiredNextSiblingElement) {
    val hasRequired = enclosingTerm match {
      case None => false
      case Some(s: SequenceTermBase) if s.isOrdered => {
        // possibleNextSiblingTerms is either all optional/does not have a
        // required element, or the last one is required. Thus, this has a
        // required next sibling if the last sibling element is required
        possibleNextSiblingTerms.lastOption match {
          case None => false
          case Some(e: ElementBase) => e.isRequiredInInfoset
          case Some(mg: ModelGroup) => mg.mustHaveRequiredElement
          case Some(_) => Assert.invariantFailed()
        }
      }
      case _ => false
    }
    hasRequired
  }.value

  /*
   * Determines if this model group must have at least one required element, or
   * if everything in the model group is optional and thus, might not cause
   * any unparse events. This is used to determine next children/sibling
   * elements used during unparsing.
   */
  final def mustHaveRequiredElement: Boolean = LV('mustHaveRequiredElement) {
    this match {
      case s: SequenceTermBase if s.isHidden => false
      case s: SequenceTermBase if s.isOrdered =>
        groupMembers.exists {
          case e: ElementBase => !e.canBeAbsentFromUnparseInfoset
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
      case _ =>
        groupMembers.forall {
          case e: ElementBase => !e.canBeAbsentFromUnparseInfoset
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
    }
  }.value

  lazy val initiatedContentCheck: Unit = {
    if (initiatedContent eq YesNo.Yes) {
      groupMembers.foreach { term =>
        term.schemaDefinitionUnless(term.hasInitiator, "Enclosing group has initiatedContent='yes', but initiator is not defined.")
      }
    }
  }
}
