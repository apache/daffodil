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
import scala.xml._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.ModelGroupGrammarMixin
import java.lang.{ Integer => JInt }
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
        val seq = new Sequence(child, lexicalParent, position) // throwaway just so we can grab local properties to check for hidden
        if (seq.hiddenGroupRefOption.isDefined) {
          // explicit check here, because we're about to discard this, and construct
          // a SequenceGroupRef or ChoiceGroupRef, with isHidden = true. So this
          // temporary sequence will be discarded after this.
          seq.checkHiddenGroupRefHasNoChildren
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
        val groupRef = GroupRefFactory(child, lexicalParent, pos, isHidden)
        groupRef.asModelGroup
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

  requiredEvaluationsIfActivated(groupMembers)
  requiredEvaluationsIfActivated(initiatedContentCheck)

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

  /**
   * The group members are shared across all instances of the group, when they
   * are for groups with the same shareKey.
   *
   * This sharing is critical, because without it, we would be creating a new
   * instance of all members recursively for every reference to a group.
   * Recursively that would lead to a combinatorial explosion of copies.
   *
   * The public member to access group members, which insures sharing is preserved
   * is this member.
   *
   * The various subclasses of this class define the group members by
   * overrides of the protected groupMembersDef member.
   *
   * Note that the sharing depends on some scala subtleties here. Like that
   * this is a lazy val, so evaluated exactly once, and that the 2nd argument
   * to getShared is passed by name, so it is not necessarily evaluated at
   * all. E.g., if the share-able item already exists, then it is returned and
   * the 2nd arg isn't evaluated for that call.
   *
   * This also depends on groupMembersDef overrides all being lazy val.
   */
  final lazy val groupMembers: Seq[Term] =
    schemaSet.sharedGroupMembersFactory.getShared(shareKey, groupMembersDef)

  /**
   * Override to define the group members. They are accessed by way of the public
   * member groupMembers which insures these are properly shared across all
   * references to the group.
   *
   * These *must* be overridden as lazy val to insure that we compute them only
   * once and share them appropriately.
   */
  protected def groupMembersDef: Seq[Term]

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

  protected final lazy val prettyBaseName = xml.label

  final lazy val sequenceChildren = groupMembers.collect { case s: SequenceTermBase => s }
  final lazy val choiceChildren = groupMembers.collect { case s: ChoiceTermBase => s }
  final lazy val groupRefChildren = groupMembers.collect { case s: GroupRef => s }

  final override lazy val termChildren = groupMembers

  /**
   * Returns tuple, where the first is children that could be last, and the
   * second is a boolean if all children could be optional, and thus this could
   * be last, and with no children, might itself be not-present.
   *
   * This is an approximation used by the schema compiler however, there are cases where
   * it will provide conservative information.
   */
  lazy val potentialLastChildren: (Seq[Term], Boolean) = {
    val (potentialLast, allOptional) = this match {
        //
        // All the choice children are potentially last, depending on which branch is active. Could be
        // any of them.
        //
        // We say false for whether they can all be absent, because for any choice one of the the branches
        // must be present.
        //
      case ch: ChoiceTermBase => (ch.groupMembers, false) // false - one of the choice branches must be used.
        //
        // An unordered sequence so any group member could be last.
        //
        // Conservatively, we'll return true indicating that all members could in fact be optional.
        // This will be ok, because it will just cause the caller to take one further case into consideration
        // which is that possibly none of the contents of the group could be present.
        //
        // This is fine if we are computing something like the ending alignment approximation of a term.
        // We're just going to take one more case into the approximation.
        //
        // We could be more precise if we searched the children to see if any are scalars. But
        // not bothering for now.
        //
      case sq: SequenceTermBase if !sq.isOrdered => (sq.groupMembers, true)

      case sq: SequenceTermBase => {
        //
        // Ordered sequence.
        //
        // Look at the last group member.
        // If it is defined and is optional, then look backward
        // from it to prior siblings for a required scalar one.
        //
        val maybeLast = sq.groupMembers.lastOption
        if (maybeLast.isDefined) {
          val last = maybeLast.get
          val lastIsOptional = last match {
            case mg: ModelGroup => false // model group is mandatory
            case eb: ElementBase => !eb.isRequiredStreamingUnparserEvent || !eb.isRepresented
          }
          if (lastIsOptional) {
            //
            // The potential prior terms of a term works backward returning a list of everything
            // that could precede it. This stops when it hits a scalar, as nothing prior to that
            // could immediately preceed this.
            //
            val priorSibs = last.potentialPriorTerms
            (last +: priorSibs, !priorSibs.exists { _.isScalar })
          } else {
            //
            // last one is not optional, so the potential last is only that last one,
            // and not everything is optional
            //
            (Seq(last), false)
          }
        } // end maybeLast.isDefined
        else {
          //
          // there are no children.
          // By definition they're all optional.
          //
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

  /*
   * Determines if this model group must have at least one required element, or
   * if everything in the model group is optional and thus, might not cause
   * any unparse events. This is used to determine next children/sibling
   * elements used during unparsing.
   */
  final def mustHaveRequiredElement: Boolean = LV('mustHaveRequiredElement) {
    this match {
      case gr: GroupRef if gr.isHidden => false
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
      groupMembers.foreach {
        term => {
          term.schemaDefinitionUnless(term.hasInitiator,
            "Enclosing group has initiatedContent='yes', but initiator is not defined.")
          term.schemaDefinitionUnless(term.hasNonZeroLengthInitiator,
            "Enclosing group has initiatedContent='yes', but initiator can match zero-length data.")
        }
      }
    }
  }
}
