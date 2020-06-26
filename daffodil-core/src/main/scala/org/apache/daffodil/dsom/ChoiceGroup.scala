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
import scala.xml._
import org.apache.daffodil.schema.annotation.props.gen.Choice_AnnotationMixin
import org.apache.daffodil.schema.annotation.props.gen.ChoiceAGMixin
import org.apache.daffodil.grammar.ChoiceGrammarMixin
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import org.apache.daffodil.schema.annotation.props.gen.ChoiceKeyKindType

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

trait ChoiceDefMixin
  extends AnnotatedSchemaComponent
  with GroupDefLike {

  protected def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]

  protected override def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => Some(new DFDLChoice(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected def emptyFormatFactory: DFDLFormatAnnotation = new DFDLChoice(newDFDLAnnotationXML("choice"), this)

  lazy val xmlChildren = xml match {
    case <choice>{ c @ _* }</choice> => c
    case <group>{ _* }</group> => {
      val ch = this match {
        case cgd: GlobalChoiceGroupDef => cgd.xml \ "choice"
      }
      val <choice>{ c @ _* }</choice> = ch(0)
      c
    }
  }
}

abstract class ChoiceTermBase(
  final override val xml: Node,
  final override val optLexicalParent: Option[SchemaComponent],
  final override val position: Int)
  extends ModelGroup(position)
  with Choice_AnnotationMixin
  with RawDelimitedRuntimeValuedPropertiesMixin // initiator and terminator (not separator)
  with ChoiceGrammarMixin
  with ChoiceAGMixin
  with HasOptRepTypeMixinImpl {

  requiredEvaluationsIfActivated(branchesAreNonOptional)
  requiredEvaluationsIfActivated(branchesAreNotIVCElements)
  requiredEvaluationsIfActivated(modelGroupRuntimeData.preSerialization)

  final protected lazy val optionChoiceDispatchKeyRaw = findPropertyOption("choiceDispatchKey")
  final protected lazy val choiceDispatchKeyRaw = requireProperty(optionChoiceDispatchKeyRaw)

  lazy val optionChoiceDispatchKeyKindRaw = findPropertyOption("choiceDispatchKeyKind")
  lazy val defaultableChoiceDispatchKeyKind =
    if (tunable.requireChoiceDispatchKeyKindProperty) {
      choiceDispatchKeyKind
    } else {
      val asString = optionChoiceDispatchKeyKindRaw.toOption.getOrElse("implicit")
      ChoiceKeyKindType(asString, this)
    }

  lazy val optionChoiceBranchKeyKindRaw = findPropertyOption("choiceBranchKeyKind")
  lazy val defaultableChoiceBranchKeyKind =
    if (tunable.requireChoiceBranchKeyKindProperty) {
      choiceDispatchKeyKind
    } else {
      val asString = optionChoiceBranchKeyKindRaw.toOption.getOrElse("implicit")
      ChoiceKeyKindType(asString, this)
    }

  final lazy val isDirectDispatch = {
    val isDD: Boolean = defaultableChoiceDispatchKeyKind match {
      case ChoiceKeyKindType.ByType => true
      case ChoiceKeyKindType.Explicit => true
      case ChoiceKeyKindType.Implicit => optionChoiceDispatchKeyRaw.isDefined
      case ChoiceKeyKindType.Speculative => false
    }
    if (isDD && initiatedContent == YesNo.Yes) {
      SDE("dfdl:initiatedContent must not equal 'yes' when dfdl:choiceDispatchKey is defined")
    }
    isDD
  }

  // If choiceDispatchKeyKind is byType, verify that all our children share a repType,
  // and use that. Otherwise, there is no need to associate a repType with this choice
  override final lazy val optRepType: Option[SimpleTypeDefBase] = defaultableChoiceDispatchKeyKind match {
    case ChoiceKeyKindType.ByType => {
      val branchReptypes: Seq[SimpleTypeDefBase] = groupMembers.map(term => {
        term match {
          case e: ElementDeclMixin => e.typeDef match {
            case t: SimpleTypeDefBase => t.optRepTypeDef match {
              case None => SDE("When <xs:choice> has choiceBranchKey=\"byType\", all branches must have a type which defines a repType")
              case Some(x) => x
            }
            case _: SimpleTypeBase => SDE("When <xs:choice> has choiceBranchKey=\"byType\", no branch can have a primitive xsd type")
            case _ => SDE("When <xs:choice> has choiceBranchKey=\"byType\", all branches must be a simple type")
          }
          case _ => SDE("When <xs:choice> has choiceBranchKey=\"byType\", all branches must be a simple element")
        }
      })
      val ans = branchReptypes.reduce((a, b) => {
        /*
         * We tolerate type objects being copies or the same object because we compare the QNames.
         */
        if (a.namedQName != b.namedQName) {
          SDE("All children of an <xs:choice> with choiceDispatchKeyKind=byType must have the same reptype")
        }
        a
      })
      Some(ans)
    }
    case ChoiceKeyKindType.Speculative => None
    case ChoiceKeyKindType.Explicit => None
    case ChoiceKeyKindType.Implicit => None
  }

  /*
   * When choiceBranchKeyKind=byType or choiceDispatchKeyKind=byType, we are associated with one or more repTypes (corresponding to
   * the reptypes of our choices).
   * Since the repValueSet represents the set of repValues that we expect to be able to be associated with an element, it does, in theory,
   * make sense to assign a repValueSet to an entire choiceGroup. For instance, in the common case of  choiceBranchKeyKind=byType and choiceDispatchKeyKind=byType,
   * this would just be the union of the repValueSets of all of the individual choices.
   * However, figuring this out becomes more complicated when we start having to deal with choiceBranchKeyKind=explicit
   * There is no indication yet that this problem is particuarly intractable, however it is non-trivial. Since we never
   * actually need to know the optRepValueSet of an entire choiceGroup, we can simple avoid thinking about it until such a time that we do.
   */

  override final lazy val optRepValueSet = Assert.invariantFailed("We shouldn't need to compute the optRepValueSet of a choiceGroup")

  final override lazy val hasKnownRequiredSyntax = LV('hasKnownRequiredSyntax) {
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
  final def branchesAreNonOptional = LV('branchesAreNonOptional) {
    val branchesOk = groupMembers map { branch =>
      if (branch.isOptional) {
        schemaDefinitionErrorButContinue("Branch of choice %s must be non-optional.".format(branch.path))
        false
      } else true
    }
    assuming(branchesOk.forall { x => x })
  }.value

  final def branchesAreNotIVCElements = LV('branchesAreNotIVCElements) {
    val branchesOk = groupMembers map { branch =>
      if (!branch.isRepresented) {
        schemaDefinitionErrorButContinue("Branch of choice %s cannot have the dfdl:inputValueCalc property.".format(branch.path))
        false
      } else true
    }
    assuming(branchesOk.forall { x => x })
  }.value
}

final class Choice(xmlArg: Node, lexicalParent: SchemaComponent, position: Int)
  extends ChoiceTermBase(xmlArg, Option(lexicalParent), position)
  with ChoiceDefMixin {

  override lazy val optReferredToComponent = None

}
