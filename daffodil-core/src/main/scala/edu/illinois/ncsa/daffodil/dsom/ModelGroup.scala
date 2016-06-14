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

import scala.Option.option2Iterable
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Text
import scala.xml._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ModelGroupRuntimeData
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.grammar.ModelGroupGrammarMixin
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceBranchEvent
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceBranchStartEvent
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceBranchEndEvent
import edu.illinois.ncsa.daffodil.equality._

/**
 * A factory for model groups.
 */
object GroupFactory {

  /**
   * Because of the contexts where this is used, we return a list. That lets users
   * flatmap it to get a collection of model groups. Nil for non-model groups, non-Nil for the model group
   * object. There should be only one non-Nil.
   */
  def apply(child: Node, parent: SchemaComponent, position: Int) = {
    val childList: List[GroupBase] = child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(child, parent, position))
      case <choice>{ _* }</choice> => List(new Choice(child, parent, position))
      case <group>{ _* }</group> => {
        parent match {
          case ct: ComplexTypeBase => List(new GroupRef(child, ct, 1))
          case mg: ModelGroup => List(new GroupRef(child, mg, position))
        }
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _: Comment => Nil
      case _ => {
        parent.SDE("Unrecognized construct: %s", child)
      }
    }
    childList
  }

}

/**
 * Base class for all model groups, which are term containers.
 */
abstract class ModelGroup(xmlArg: Node, parentArg: SchemaComponent, position: Int)
  extends GroupBase(xmlArg, parentArg, position)
  with DFDLStatementMixin
  with ModelGroupGrammarMixin
  with OverlapCheckMixin
  with RealTermMixin {

  requiredEvaluations(groupMembers)

  final lazy val elementChildren: Seq[ElementBase] =
    groupMembers.flatMap {
      case eb: ElementBase => Seq(eb)
      case gb: GroupBase => gb.group.elementChildren
    }

  final override def isOptional = false
  final override def isRequired = true

  final override lazy val runtimeData: RuntimeData = modelGroupRuntimeData

  final override lazy val termRuntimeData: TermRuntimeData = modelGroupRuntimeData

  protected lazy val groupMembersRuntimeData = this match {
    case mg: ModelGroup => mg.groupMembers.map {
      _ match {
        case eb: ElementBase => eb.erd
        case t: Term => t.termRuntimeData
      }
    }
    case _ => Nil
  }

  def modelGroupRuntimeData: ModelGroupRuntimeData

  final lazy val gRefNonDefault: Option[ChainPropProvider] = groupRef.map { _.nonDefaultFormatChain }
  final lazy val gRefDefault: Option[ChainPropProvider] = groupRef.map { _.defaultFormatChain }

  final def nonDefaultPropertySources = LV('nonDefaultPropertySources) {
    val seq = (gRefNonDefault.toSeq ++ Seq(this.nonDefaultFormatChain)).distinct
    checkNonOverlap(seq)
    seq
  }.value

  final def defaultPropertySources = LV('defaultPropertySources) {
    val seq = (gRefDefault.toSeq ++ Seq(this.defaultFormatChain)).distinct
    seq
  }.value

  protected final lazy val prettyBaseName = xmlArg.label

  protected def xmlChildren: Seq[Node]

  private def goodXmlChildren = LV('goodXMLChildren) { xmlChildren.flatMap { removeNonInteresting(_) } }.value
  private lazy val positions = List.range(1, goodXmlChildren.length + 1) // range is exclusive on 2nd arg. So +1.
  private lazy val pairs = goodXmlChildren zip positions

  final lazy val sequenceChildren = groupMembers.collect { case s: Sequence => s }
  final lazy val choiceChildren = groupMembers.collect { case s: Choice => s }
  final lazy val groupRefChildren = groupMembers.collect { case s: GroupRef => s }

  final def group = this

  final lazy val groupMembers = {
    pairs.flatMap {
      case (n, i) =>
        termFactory(n, this, i)
    }
  }

  final override lazy val termChildren = groupMembers

  final lazy val groupMembersNoRefs = groupMembers.map {
    case eRef: ElementRef => eRef.referencedElement
    case gb: GroupBase => gb.group
    case x => x
  }

  /**
   * Factory for Terms
   *
   * Because of the context where this is used, this returns a list. Nil for non-terms, non-Nil for
   * an actual term. There should be only one non-Nil.
   *
   * This could be static code in an object. It doesn't reference any of the state of the ModelGroup,
   * it's here so that type-specific overrides are possible in Sequence or Choice
   */
  private def termFactory(child: Node, parent: ModelGroup, position: Int) = {
    val childList: List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = child.attribute("ref").map { _.text }
        // must get an unprefixed attribute name, i.e. ref='foo:bar', and not
        // be tripped up by dfdl:ref="fmt:fooey" which is a format reference.
        refProp match {
          case None => List(new LocalElementDecl(child, parent, position))
          case Some(_) => List(new ElementRef(child, parent, position))
        }
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => GroupFactory(child, parent, position)
    }
    childList
  }

  /**
   * XML is full of uninteresting text nodes. We just want the element children, not all children.
   */
  private def removeNonInteresting(child: Node) = {
    val childList: List[Node] = child match {
      case _: Text => Nil
      case _: Comment => Nil
      case <annotation>{ _* }</annotation> => Nil
      case _ => List(child)
    }
    childList
  }

  /**
   * Combine our statements with those of the group ref that is referencing us (if there is one)
   */
  final lazy val statements: Seq[DFDLStatement] = localStatements ++ groupRef.map { _.statements }.getOrElse(Nil)
  final lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    localNewVariableInstanceStatements ++ groupRef.map { _.newVariableInstanceStatements }.getOrElse(Nil)
  final lazy val (discriminatorStatements, assertStatements) = checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)
  private lazy val combinedAsserts: Seq[DFDLAssert] = localAssertStatements ++ groupRef.map { _.assertStatements }.getOrElse(Nil)
  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] = localDiscriminatorStatements ++ groupRef.map { _.discriminatorStatements }.getOrElse(Nil)

  final lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs = localSetVariableStatements ++ groupRef.map { _.setVariableStatements }.getOrElse(Nil)
    checkDistinctVariableNames(combinedSvs)
  }

  final lazy val groupRef = parent match {
    case ggd: GlobalGroupDef => Some(ggd.groupRef)
    case _ => None
  }

  // FIXME:
  // THis is wrong. Somewhere one must compute whether something is known to be of byte length
  //  but this code never does.
  final override lazy val isKnownToBePrecededByAllByteLengthItems: Boolean = {
    val es = nearestEnclosingSequence
    es match {
      case None => true
      case Some(s) => {
        if (s.groupMembers.head _eq_ this) s.isKnownToBePrecededByAllByteLengthItems
        else {
          //pass for now
          val index = s.groupMembers.indexOf(this)
          s.groupMembers.slice(0, index).forall { _.isKnownToBePrecededByAllByteLengthItems }
        }
      }
    }
  }

  final def allSelfContainedTermsTerminatedByRequiredElement: Seq[Term] =
    LV('allSelfContainedTermsTerminatedByRequiredElement) {
      val listOfTerms = groupMembersNoRefs.map(m => {
        m match {
          case e: LocalElementBase if e.isOptional => (Seq(e) ++ e.possibleNextTerms) // A LocalElement or ElementRef
          case e: LocalElementBase => Seq(e)
          case mg: ModelGroup => Seq(mg)
        }
      }).flatten
      listOfTerms
    }.value

  final def identifyingEventsForChoiceBranch: Seq[ChoiceBranchEvent] = LV('identifyingEventsForChoiceBranch) {
    Assert.usage(enclosingTerm.isDefined && enclosingTerm.get.isInstanceOf[Choice], "identifyingElementsForChoiceBranch must only be called on children of choices")

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
  protected final def possibleFirstChildTerms: Seq[Term] = LV('possibleFirstChildTerms) {
    val firstTerms = this match {
      case c: Choice => groupMembersNoRefs
      case s: Sequence if !s.isOrdered => groupMembersNoRefs
      case s: Sequence => {
        groupMembersNoRefs.headOption match {
          case None => Nil
          case Some(e: ElementBase) if e.canBeAbsentFromUnparseInfoset => {
            // this case covers optional elements, arrrays with minOccurs = 0,
            // and elements with outputValueCalc. In each of these cases, the
            // first child could be first, but so could any siblings that
            // follow it
            Seq(e) ++ e.possibleNextSiblingTerms
          }
          case Some(s: Sequence) if s.hiddenGroupRefOption.isDefined => s.possibleNextSiblingTerms
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

  // model groups can't be elements.
  protected final def couldBeLastElementInModelGroup: Boolean = false

  /*
   * Determines if any of the of the terms that could be next have or are
   * required elements. This essentially determines if this could contain
   * the last element in the model group.
   */
  private def hasRequiredNextSiblingElement: Boolean = LV('hasRequiredNextSiblingElement) {
    val hasRequired = enclosingTerm match {
      case None => false
      case Some(s: Sequence) if s.isOrdered => {
        // possibleNextSiblingTerms is either all optional/does not have a
        // required element, or the last one is required. Thus, this has a
        // required next sibling if the last sibling element is required
        possibleNextSiblingTerms.lastOption match {
          case None => false
          case Some(e: ElementBase) => !e.isOptional || e.isRequiredArrayElement
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
   * if everything in the model group is is optional and thus, might not cause
   * any unparse events. This is used to determine next children/sibling
   * elements used during unparsing.
   */
  final def mustHaveRequiredElement: Boolean = LV('mustHaveRequiredElement) {
    this match {
      case s: Sequence if s.hiddenGroupRefOption.isDefined => false
      case s: Sequence if s.isOrdered =>
        groupMembersNoRefs.exists {
          case e: ElementBase => !e.canBeAbsentFromUnparseInfoset
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
      case _ =>
        groupMembersNoRefs.forall {
          case e: ElementBase => !e.canBeAbsentFromUnparseInfoset
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
    }
  }.value

}
