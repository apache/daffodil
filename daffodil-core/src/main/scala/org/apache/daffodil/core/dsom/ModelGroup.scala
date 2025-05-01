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

package org.apache.daffodil.core.dsom

import java.lang.{ Integer => JInt }
import scala.xml.Comment
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Text

import org.apache.daffodil.core.dsom.walker.ModelGroupView
import org.apache.daffodil.core.grammar.ModelGroupGrammarMixin
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.AlignmentType
import org.apache.daffodil.lib.schema.annotation.props.gen.AlignmentUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo

/**
 * A factory for model groups.
 *
 * Takes care of detecting group references, and constructing the
 * proper SequenceGroupRef or ChoiceGroupRef object.
 *
 * If you want to do source xml to source xml rewriting transformations
 * (like macro expansions) do them here or in TermFactory
 */
object ModelGroupFactory {

  /**
   * Returns a Model Group.
   *
   * Non Model Group types are/should be handled by the caller.
   *
   */
  def apply(
    child: Node,
    lexicalParent: SchemaComponent,
    position: Int,
    isHidden: Boolean,
    nodesAlreadyTrying: Set[Node] = Set()
  ): ModelGroup = {
    if (nodesAlreadyTrying.contains(child)) {
      //
      // We are chasing our tail. Circular reference among named model groups/terms.
      //
      lexicalParent.schemaDefinitionError(
        "Model group circular definitions. Group references, or hidden group references form a loop."
      )
    } else {
      val moreNodesAlreadyTrying = nodesAlreadyTrying + child
      val res =
        makeModelGroup(child, lexicalParent, position, isHidden, nodesAlreadyTrying)
      res
    }
  }

  private def makeModelGroup(
    xmlNode: Node,
    lexicalParent: SchemaComponent,
    position: JInt,
    isHidden: Boolean,
    nodesAlreadyTrying: Set[Node]
  ): ModelGroup = {
    val childModelGroup: ModelGroup = xmlNode match {

      case seqXML: Elem if seqXML.label == "sequence" => {
        // seq is a throwaway just so we can grab local properties to check for things
        // that require rewriting without special-case code
        val seq = LocalSequence(xmlNode, lexicalParent, position)

        //
        // Check sequences for rewriting situations
        //
        if (seq.hiddenGroupRefOption.isDefined)
          rewriteHiddenGroup(seq, lexicalParent, position, isHidden, nodesAlreadyTrying)
        else if (
          seq.isLayered &&
          (seq.termChildren.length > 1 || seq.termChildren.head.isArray)
        )
          rewriteLayeredSequence(seq, lexicalParent, position, isHidden, nodesAlreadyTrying)
        else
          LocalSequence(xmlNode, lexicalParent, position) // No rewriting the sequence.
      }
      case Elem(_, "choice", _, _, _*) => Choice(xmlNode, lexicalParent, position)
      case Elem(_, "group", _, _, _*) => {
        val pos: Int = lexicalParent match {
          case ct: ComplexTypeBase => 1
          case mg: ModelGroup => position
          case gd: GlobalGroupDef => position
        }
        val groupRef = GroupRefFactory(xmlNode, lexicalParent, pos, isHidden)
        groupRef.asModelGroup
      }
      case _ => {
        // $COVERAGE-OFF$
        Assert.invariantFailed(
          "Unrecognized construct %s should be handled by caller.".format(xmlNode)
        )
        // $COVERAGE_ON$
      }
    }
    childModelGroup
  }

  /**
   * Rewrite of XML for layered sequence with multiple children or an array child.
   * This rewrites the schema so that the layered sequence has one non-array child only.
   *
   * @param seq                The LocalSequence to be rewritten.
   * @param lexicalParent      The SchemaComponent representing the lexical parent.
   * @param position           The position of the layered sequence within its enclosing model group
   * @param isHidden           Indicates whether the sequence is hidden or not.
   * @param nodesAlreadyTrying A set of nodes that have already been tried.
   * @return The rewritten ModelGroup.
   */
  private def rewriteLayeredSequence(
    seq: LocalSequence,
    lexicalParent: SchemaComponent,
    position: JInt,
    isHidden: Boolean,
    nodesAlreadyTrying: Set[Node]
  ): ModelGroup = {
    val realChildren: Seq[Node] = seq.apparentXMLChildren.filterNot {
      // strip away all the trash nodes
      case _: Text => true // whitespace nodes
      case _: Comment => true
      case _ => false
    }

    val newXML = realChildren match {
      // If an annotation is first, that stays on the outer sequence
      case Seq(annotation @ Elem(_, "annotation", _, _, _*), terms @ _*) =>
        <sequence dfdlx:layer={seq.xml.attribute("layer")}>
          {annotation}
          <sequence>
            {terms}
          </sequence>
        </sequence>.copy(scope = seq.xml.scope) % seq.xml.attributes
      case terms =>
        // there is no annotation, so all children just go inside the inner sequence.
        <sequence dfdlx:layer={seq.xml.attribute("layer")}>
          <sequence>
            {terms}
          </sequence>
        </sequence>.copy(scope = seq.xml.scope) % seq.xml.attributes
    }
    makeModelGroup(
      newXML,
      lexicalParent,
      position,
      isHidden,
      nodesAlreadyTrying
    )
  }

  /**
   * Rewrites a sequence with dfdl:hiddenGroupRef into a group reference + the isHidden flag.
   *
   * @param seq                The local sequence containing the hidden group.
   * @param lexicalParent      The parent schema component of the hidden group.
   * @param position           The position of the hidden group within the sequence.
   * @param isHidden           Flag indicating whether the hidden group should be hidden.
   * @param nodesAlreadyTrying Set of nodes that have already been processed.
   * @return The processed hidden group.
   */
  private def rewriteHiddenGroup(
    seq: LocalSequence,
    lexicalParent: SchemaComponent,
    position: JInt,
    isHidden: Boolean,
    nodesAlreadyTrying: Set[Node]
  ) = {
    // explicit check here, because we're about to discard this, and construct
    // a SequenceGroupRef or ChoiceGroupRef, with isHidden = true. So this
    // temporary sequence will be discarded after this.
    seq.checkHiddenGroupRefHasNoChildren
    //
    // construct the group ref XML, then recursively process that,
    // but set flag so it will be hidden.
    //
    val hgrXML = seq.hiddenGroupRefXML
    ModelGroupFactory(
      hgrXML,
      lexicalParent,
      position,
      isHidden = true,
      nodesAlreadyTrying
    )
  }
}

/**
 * Factory for Terms
 *
 * If you want to do source XML to source XML rewrite transformations, do them
 * here or in ModelGroupFactory.
 */
object TermFactory {

  /**
   * Returns a Term.
   *
   * Non Term/Model Group elements are/should be taken care of by the caller.
   *
   */
  def apply(
    child: Node,
    lexicalParent: GroupDefLike,
    position: Int,
    nodesAlreadyTrying: Set[Node] = Set()
  ) = {
    val childTerm: Term = child match {
      case Elem(_, "element", _, _, _*) => {
        val refProp = child.attribute("ref").map { _.text }
        // must get an unprefixed attribute name, i.e. ref='foo:bar', and not
        // be tripped up by dfdl:ref="fmt:fooey" which is a format reference.
        refProp match {
          case None => {
            val eDecl = LocalElementDecl(child, lexicalParent, position)
            eDecl
          }
          case Some(_) => ElementRef(child, lexicalParent, position)
        }
      }
      case _ =>
        ModelGroupFactory(child, lexicalParent, position, isHidden = false, nodesAlreadyTrying)
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
abstract class ModelGroup protected (index: Int)
  extends Term
  with ModelGroupGrammarMixin
  with OverlapCheckMixin
  with NestingLexicalMixin
  with ModelGroupView {

  protected[dsom] override def initialize(): Unit = {
    super.initialize()
    xmlChildren
  }

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
  override final lazy val groupMembers: Seq[Term] =
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

  override protected lazy val diagnosticDebugNameImpl = prettyBaseName + prettyIndex

  override lazy val alignmentValueInBits: JInt = {
    this.alignment match {
      case AlignmentType.Implicit =>
        this.alignmentUnits match {
          case AlignmentUnits.Bits => 1
          case AlignmentUnits.Bytes => 8
        }
      case align: JInt =>
        this.alignmentUnits match {
          case AlignmentUnits.Bits => align
          case AlignmentUnits.Bytes => 8 * align
        }
    }
  }

  final lazy val elementChildrenInNonHiddenContext: Seq[ElementBase] =
    LV(Symbol("elementChildrenInNonHiddenContext")) {
      val ebs = groupMembers
        .filter {
          case gr: GroupRef => !gr.isHidden
          case _ => true
        }
        .flatMap { gm =>
          gm match {
            case eb: ElementBase => Seq(eb)
            case gb: ModelGroup => gb.elementChildren
          }
        }
      ebs
    }.value

  final lazy val elementChildren: Seq[ElementBase] = LV(Symbol("elementChildren")) {
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
      case ch: ChoiceTermBase =>
        (ch.groupMembers, false) // false - one of the choice branches must be used.
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
      // If there are no children, by definition, all children are optional.
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
  final lazy val mustHaveRequiredElement: Boolean = LV(Symbol("mustHaveRequiredElement")) {
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
      groupMembers.foreach { memberTerm =>
        val term = memberTerm match {
          case impliedSequence: ChoiceBranchImpliedSequence => impliedSequence.groupMembers(0)
          case regular => regular
        }
        term.schemaDefinitionUnless(
          term.hasInitiator,
          "Enclosing group has initiatedContent='yes', but initiator is not defined."
        )
        term.schemaDefinitionUnless(
          term.hasNonZeroLengthInitiator,
          "Enclosing group has initiatedContent='yes', but initiator can match zero-length data."
        )
      }
    }
  }
}
