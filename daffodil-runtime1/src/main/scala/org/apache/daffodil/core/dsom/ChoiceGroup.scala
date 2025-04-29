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

import scala.xml.Elem
import scala.xml.Node

import org.apache.daffodil.core.dsom.walker.ChoiceView
import org.apache.daffodil.core.grammar.ChoiceGrammarMixin
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.gen.ChoiceAGMixin
import org.apache.daffodil.lib.schema.annotation.props.gen.Choice_AnnotationMixin
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo

/**
 *
 * Captures concepts common to all choice definitions, which includes both
 * local and global choice definitions, but not choice group references.
 *
 * Choices are a bit complicated.
 *
 * They can have initiators and terminators. This is most easily thought of as wrapping each choice
 * inside a sequence having only the choice inside it, and moving the initiator and terminator specification to that
 * sequence.
 *
 * That sequence is then the term replacing the choice wherever the choice is.
 *
 * Choices can have children which are scalar elements. In this case, that scalar element behaves as if it were
 * a child of the enclosing sequence (which could be the one we just injected above the choice.
 *
 * Choices can have children which are recurring elements. In this case, the behavior is as if the recurring child was
 * placed inside a sequence which has no initiator nor terminator, but repeats the separator specification from
 * the sequence context that encloses the choice.
 *
 * All that, and the complexities of separator suppression too.
 *
 * There's also issues like this:
 *
 * <choice>
 *    <element .../>
 *    <sequence/>
 * </choice>
 *
 * in the above, one alternative is an empty sequence. So this choice may produce an element which takes up
 * a child position, and potentially requires separation, or it may produce nothing at all.
 *
 */

trait ChoiceDefMixin extends AnnotatedSchemaComponent with GroupDefLike {

  protected def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]

  protected override def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case Elem("dfdl", "choice", _, _, _*) =>
        Some(new DFDLChoice(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected def emptyFormatFactory: DFDLFormatAnnotation =
    new DFDLChoice(newDFDLAnnotationXML("choice"), this)

  lazy val xmlChildren = xml match {
    case Elem(_, "choice", _, _, c @ _*) => c
    case Elem(_, "group", _, _, _*) => {
      val ch = this match {
        case cgd: GlobalChoiceGroupDef => cgd.xml \ "choice"
      }
      val Elem(_, "choice", _, _, c @ _*) = ch(0)
      c
    }
  }
}

abstract class ChoiceTermBase(
  final override val xml: Node,
  final override val optLexicalParent: Option[SchemaComponent],
  final override val position: Int
) extends ModelGroup(position)
  with Choice_AnnotationMixin
  with RawDelimitedRuntimeValuedPropertiesMixin // initiator and terminator (not separator)
  with ChoiceGrammarMixin
  with ChoiceAGMixin {

  requiredEvaluationsIfActivated(noBranchesFound)
  requiredEvaluationsIfActivated(branchesAreNonOptional)
  requiredEvaluationsIfActivated(branchesAreNotIVCElements)

  final protected lazy val optionChoiceDispatchKeyRaw =
    findPropertyOption("choiceDispatchKey", expressionAllowed = true)
  final protected lazy val choiceDispatchKeyRaw = requireProperty(
    optionChoiceDispatchKeyRaw
  )

  def getChoiceDispatchKeyRaw: Found = choiceDispatchKeyRaw

  final lazy val isDirectDispatch = {
    val isDD = optionChoiceDispatchKeyRaw.isDefined
    if (isDD && initiatedContent == YesNo.Yes) {
      SDE("dfdl:initiatedContent must not equal 'yes' when dfdl:choiceDispatchKey is defined")
    }
    isDD
  }

  final override lazy val hasKnownRequiredSyntax = LV(Symbol("hasKnownRequiredSyntax")) {
    if (hasFraming) true
    // else if (isKnownToBeAligned) true //TODO: Alignment may not occur; hence, cannot be part of determining whether there is known syntax.
    else {
      val res =
        // For a group, we only have known required syntax if all branches do.
        // If even one branch doesn't, then we don't have "known required" because the data could match
        // that branch, meaning we wouldn't, in that case, have syntax.
        groupMembers.forall { member =>
          val res =
            member.hasKnownRequiredSyntax
          res
        }
      res
    }
  }.value

  final lazy val noBranchesFound = LV(Symbol("noBranchesFound")) {
    if (groupMembers.size == 0) {
      SDE("choice element must contain one or more branches")
    }
  }.value

  /**
   * The DFDL Spec as of this writing 2015-06-02 says that branches cannot be optional, but then
   * specifies that what it means is that if a choice branch root is an element/element-ref, it
   * cannot have minOccurs="0". This is a different notion of optional.
   *
   * The DFDL Working group has Action 280 which is about whether this should be corrected/modified.
   * There are two notions of optionality in DFDL. First is optional meaning 0 or 1 occurrence of an
   * element. The second is "not required", and that is defined in terms of dfdl:occursCountKind.
   *
   * For the time being, the definition of non-optional used here is that is not minOccurs="0" if an
   * element, and that unless it is a calculated element, it must have some footprint syntactically
   * from the data value or framing.
   *
   * Open issues:
   * 1) Is alignment or leading/trailing skip to be considered syntax. Alignment might not be there.
   * 2) What about an empty sequence that only carries statement annotations such as dfdl:assert or
   * dfdl:setVariable
   *
   * This latter need to be allowed, because while they do not have known required syntax they do
   * have to be executed for side-effect.
   */
  final lazy val branchesAreNonOptional = LV(Symbol("branchesAreNonOptional")) {
    val branchesOk = groupMembers.map { branch =>
      val realBranch = branch match {
        case impliedSeq: ChoiceBranchImpliedSequence => impliedSeq.groupMembers(0)
        case regular => regular
      }
      if (realBranch.isOptional) {
        realBranch.schemaDefinitionErrorButContinue("Branch of choice must be non-optional.")
        false
      } else true
    }
    assuming(branchesOk.forall { x => x })
  }.value

  final lazy val branchesAreNotIVCElements = LV(Symbol("branchesAreNotIVCElements")) {
    val branchesOk = groupMembers.map { branch =>
      if (!branch.isRepresented) {
        branch.schemaDefinitionErrorButContinue(
          "Branch of choice cannot have the dfdl:inputValueCalc property."
        )
        false
      } else true
    }
    assuming(branchesOk.forall { x => x })
  }.value
}

object Choice {
  def apply(xmlArg: Node, lexicalParent: SchemaComponent, position: Int) = {
    val c = new Choice(xmlArg, lexicalParent, position)
    c.initialize()
    c
  }
}
final class Choice private (xmlArg: Node, lexicalParent: SchemaComponent, position: Int)
  extends ChoiceTermBase(xmlArg, Option(lexicalParent), position)
  with ChoiceDefMixin
  with ChoiceView {

  override lazy val optReferredToComponent = None

  override protected def computeGroupMembers(): Seq[Term] = {
    val firstCut = super.computeGroupMembers()
    val withImpliedSequences =
      firstCut.map { term =>
        val finalTerm =
          if (term.isScalar) term
          else {

            /**
             * If this choice branch is a non-scalar, then we need to encapsulate
             * it with a ChoiceBranchImpliedSequence, which is a kind of Sequence
             * base. When compiling this this choice branch, Daffodil can then
             * depend on the invariant that every recurring element is contained
             * inside a sequence, and that sequence describes everything about how
             * that elements occurrences are separated.
             */
            ChoiceBranchImpliedSequence(term)
          }
        finalTerm
      }
    withImpliedSequences
  }

}
