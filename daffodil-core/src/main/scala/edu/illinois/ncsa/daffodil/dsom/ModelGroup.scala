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

import java.util.UUID
import scala.Option.option2Iterable
import scala.xml.Attribute
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Null
import scala.xml.Text
import scala.xml._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.AlignmentUnits
import edu.illinois.ncsa.daffodil.processors.ModelGroupRuntimeData
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.grammar.ModelGroupGrammarMixin

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

  final lazy val modelGroupRuntimeData = {

    val groupMembers = this match {
      case mg: ModelGroup => mg.groupMembers.map {
        _ match {
          case eb: ElementBase => eb.erd
          case t: Term => t.termRuntimeData
        }
      }
      case _ => Nil
    }
    new ModelGroupRuntimeData(
      schemaSet.variableMap,
      encodingInfo,
      // elementChildren.map { _.elementRuntimeData.dpathElementCompileInfo },
      schemaFileLocation,
      dpathCompileInfo,
      prettyName,
      path,
      namespaces,
      defaultBitOrder,
      groupMembers,
      enclosingElement.map { _.elementRuntimeData }.getOrElse(
        Assert.invariantFailed("model group with no surrounding element.")),
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      fillByteValue)
  }

  private val mgID = UUID.randomUUID()

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

  final override lazy val isKnownToBePrecededByAllByteLengthItems: Boolean = {
    val es = nearestEnclosingSequence
    es match {
      case None => true
      case Some(s) => {
        if (s.groupMembers.head eq this) s.isKnownToBePrecededByAllByteLengthItems
        else {
          //pass for now
          val index = s.groupMembers.indexOf(this)
          s.groupMembers.slice(0, index).forall { _.isKnownToBePrecededByAllByteLengthItems }
        }
      }
    }
  }

  final def isKnownToBeAligned: Boolean = LV('isKnownToBeAligned) {
    if (alignmentValueInBits == 1) {
      alignmentUnits match {
        case AlignmentUnits.Bits => true
        case AlignmentUnits.Bytes => isKnownToBePrecededByAllByteLengthItems
      }
    } else if (alignmentValueInBits > 1) {
      isKnownToBePrecededByAllByteLengthItems
    } else false
  }.value

  final def allSelfContainedTermsTerminatedByRequiredElement: Seq[Term] =
    LV('allSelfContainedTermsTerminatedByRequiredElement) {
      val listOfTerms = groupMembersNoRefs.map(m => {
        m match {
          case e: LocalElementBase if e.isOptional => (Seq(e) ++ e.couldBeNext) // A LocalElement or ElementRef
          case e: LocalElementBase => Seq(e)
          case mg: ModelGroup => Seq(mg)
        }
      }).flatten
      listOfTerms
    }.value

  final def identifyingElementsForChoiceBranch: Seq[ElementBase] = LV('identifyingElementsForChoiceBranch) {
    Assert.usage(enclosingTerm.isDefined && enclosingTerm.get.isInstanceOf[Choice], "identifyingElementsForChoiceBranch must only be called on children of choices")

    val childrenIdentifiers = couldBeFirstChildElementInInfoset
    val parentNextIdentifiers =
      if (!mustHaveRequiredElement) {
        enclosingTerm.get.asInstanceOf[ModelGroup].couldBeNextElementInInfoset
      } else {
        Nil
      }
    childrenIdentifiers ++ parentNextIdentifiers
  }.value

  /*
   * Returns list of Terms that could contain the first child element in the infoset
   */
  protected final def couldBeFirstChildTerm: Seq[Term] = LV('couldBeFirstChildTerm) {
    val firstTerms = this match {
      case c: Choice => groupMembersNoRefs
      case s: Sequence if !s.isOrdered => groupMembersNoRefs
      case s: Sequence => {
        groupMembersNoRefs.headOption match {
          case None => Nil
          case Some(e: ElementBase) if (e.isOptional || !e.isRequiredArrayElement) => Seq(e) ++ e.couldBeNextSiblingTerm
          case Some(mg: ModelGroup) if !mg.mustHaveRequiredElement => Seq(mg) ++ mg.couldBeNextSiblingTerm
          case Some(e: ElementBase) => Seq(e)
          case Some(mg: ModelGroup) => Seq(mg)
        }
      }
    }
    firstTerms
  }.value

  protected final def nextParentElements: Seq[ElementBase] = LV('nextParentElements) {
    if (parent.isInstanceOf[ModelGroup] && !hasRequiredNextSiblingElement) {
      parent.asInstanceOf[ModelGroup].couldBeNextElementInInfoset
    } else {
      Nil
    }
  }.value

  // model groups can't be elements.
  protected final def couldBeLastElementInModelGroup: Boolean = false

  /*
   * Determines if any of the of the terms that could be next have or are
   * required elements. This essentially determines if this could contain
   * the last element in the model group.
   */
  final def hasRequiredNextSiblingElement: Boolean = LV('hasRequiredNextSiblingElement) {
    val hasRequired = enclosingTerm match {
      case None => false
      case Some(s: Sequence) if s.isOrdered => {
        // couldBeNextSiblingTerm is either all optional/does not have a
        // required element, or the last one is required. Thus, this has a
        // required next sibling if the last sibling element is required
        couldBeNextSiblingTerm.lastOption match {
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
   * Determines if this model group must have a required element, or if it is
   * possible for this model group to provide nothing towards the infoset.
   */
  final def mustHaveRequiredElement: Boolean = LV('mustHaveRequiredElement) {
    this match {
      case s: Sequence if s.isOrdered =>
        groupMembersNoRefs.exists {
          case e: ElementBase => !e.isOptional || e.isRequiredArrayElement
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
      case _ =>
        groupMembersNoRefs.forall {
          case e: ElementBase => !e.isOptional || e.isRequiredArrayElement
          case mg: ModelGroup => mg.mustHaveRequiredElement
        }
    }
  }.value

}
