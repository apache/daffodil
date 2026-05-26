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

package org.apache.daffodil.core.grammar

import org.apache.daffodil.core.dsom.ChoiceTermBase
import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.ModelGroup
import org.apache.daffodil.core.dsom.Root
import org.apache.daffodil.core.dsom.SequenceTermBase
import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.core.grammar.primitives.MandatoryTextAlignment
import org.apache.daffodil.core.runtime1.TermRuntime1Mixin
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.ChoiceLengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.lib.schema.annotation.props.gen.SequenceKind
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo

/////////////////////////////////////////////////////////////////
// Common to all Terms (Elements and ModelGroups)
/////////////////////////////////////////////////////////////////

trait TermGrammarMixin extends AlignedMixin with BitOrderMixin with TermRuntime1Mixin {
  self: Term =>

  override protected final def grammarContext = this

  def termContentBody: Gram

  private lazy val newVars = this.newVariableInstanceStatements

  private lazy val newVarStarts = newVars.map { _.gram(self) }
  private lazy val newVarEnds = newVars.map { _.endGram(self) }

  protected lazy val hasEncoding = optionEncodingRaw.isDefined

  // TODO: replace dfdlScopeBegin and dfdlScopeEnd with a single Combinator.
  protected final lazy val dfdlScopeBegin = prod("dfdlScopeBegin", newVarStarts.length > 0) {
    newVarStarts.fold(mt) { _ ~ _ }
  }

  protected final lazy val dfdlScopeEnd = prod("dfdlScopeEnd", newVarEnds.length > 0) {
    newVarEnds.fold(mt) { _ ~ _ }
  }

  private lazy val setVars = this.setVariableStatements

  private lazy val setVarGrams = setVars.map { _.gram(self) }

  protected lazy val dfdlSetVariableStatements =
    prod("dfdlSetVariableStatments", setVarGrams.length > 0) {
      setVarGrams.fold(mt) { _ ~ _ }
    }

  /**
   * Mandatory text alignment or mta
   *
   * mta can only apply to things with encodings. No encoding, no MTA.
   *
   * In addition, it has to be textual data. Just because there's an encoding
   * in the property environment shouldn't get you an MTA region. It has
   * to be textual.
   */
  protected final lazy val mtaBase = prod("mandatoryTextAlignment", hasEncoding) {
    MandatoryTextAlignment(this, knownEncodingAlignmentInBits, false)
  }

  /**
   * Mandatory text alignment for delimiters
   */
  final lazy val delimMTA = prod(
    "delimMTA", {
      hasDelimiters
    }
  ) {
    // This is different from mtaBase because it passes in 'true' for the
    // last parameter to signify that it is MTA for a delimiter. mtaBase
    // passes in 'false'
    MandatoryTextAlignment(this, knownEncodingAlignmentInBits, true)
  }

  def myEffectiveLengthUnits(optLastNonEOPLU: Option[LengthUnits]): Option[LengthUnits] = {
    val elu = this match {
      case e: ElementBase =>
        e.lengthKind match {
          case LengthKind.EndOfParent => optLastNonEOPLU
          case LengthKind.Explicit | LengthKind.Prefixed => Some(e.lengthUnits)
          case LengthKind.Pattern => Some(LengthUnits.Characters)
          case _ => None
        }
      case c: ChoiceTermBase if c.choiceLengthKind == ChoiceLengthKind.Explicit =>
        Some(LengthUnits.Bytes)
      // the spec doesn't actually account for this case, but logically it makes
      // sense that ELU of a sequence is the ELU of its parent that technically is the
      // last non-EndOfParent ELU.
      case s: SequenceTermBase => optLastNonEOPLU
      case _ => None
    }
    elu
  }

  final lazy val childrenEndOfParent: Seq[Term] = LV(Symbol("childrenEndOfParent")) {
    val gms = termChildren
    val chls = gms.flatMap {
      case eb: ElementBase if eb.lengthKind == LengthKind.EndOfParent => Seq(eb)
      case eb: ElementBase => Nil
      case c: ChoiceTermBase => Nil
      case mg: ModelGroup => mg.childrenEndOfParent
    }
    chls
  }.value

  def checkEndOfParentRestrictions(optLastNonEOPLU: Option[LengthUnits]): Unit = {
    val term = this
    lazy val eopChildren = this.childrenEndOfParent
    // checks
    term match {
      case rootElem: Root if rootElem.lengthKind == LengthKind.EndOfParent => {
        rootElem.checkEndOfParentRestrictionsOnCurrentElement(Some(LengthUnits.Characters))
        eopChildren.foreach {
          case e: ElementBase => {
            rootElem.checkChildrenForSiblingsAfterEOPElement(e)
            Assert.invariant(
              optLastNonEOPLU.isDefined,
              "Effective Length Units of parent should not be None"
            )
            e.checkEndOfParentRestrictionsOnCurrentElement(optLastNonEOPLU)
          }
          case _ => // do nothing
        }
      }
      case parent: ElementBase if eopChildren.nonEmpty => {
        eopChildren.foreach {
          case e: ElementBase => {
            val optParentELU = parent.myEffectiveLengthUnits(optLastNonEOPLU)
            parent.lengthKind match {
              case LengthKind.Implicit | LengthKind.Delimited =>
                schemaDefinitionError(
                  "element is specified as dfdl:lengthKind=\"endOfParent\", but its parent is an element with dfdl:lengthKind 'implicit' or 'delimited'."
                )
              case _ => // do nothing
            }
            parent.checkChildrenForSiblingsAfterEOPElement(e)
            Assert.invariant(
              optParentELU.isDefined,
              "Effective Length Units of parent should not be None"
            )
            e.checkEndOfParentRestrictionsOnCurrentElement(optParentELU)
          }
          case _ => // do nothing
        }
      }
      case s: SequenceTermBase if eopChildren.nonEmpty => {
        schemaDefinitionWhen(
          s.separatorPosition == SeparatorPosition.Postfix,
          "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with dfdl:separatorPosition defined as 'postfix'."
        )
        schemaDefinitionWhen(
          s.sequenceKind != SequenceKind.Ordered,
          "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with dfdl:sequenceKind defined as 'unordered'."
        )
        schemaDefinitionWhen(
          s.hasTerminator,
          "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with a dfdl:terminator."
        )
        schemaDefinitionWhen(
          s.realElementChildren.exists(e => e.floating == YesNo.Yes),
          "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with elements defining dfdl:floating='yes'."
        )
        schemaDefinitionWhen(
          s.trailingSkip != 0,
          "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with a non-zero dfdl:trailingSkip."
        )
        eopChildren.foreach {
          case e: ElementBase => {
            s.checkChildrenForSiblingsAfterEOPElement(e)
            e.checkEndOfParentRestrictionsOnCurrentElement(optLastNonEOPLU)
          }
          case _ => // do nothing
        }
      }
      case c: ChoiceTermBase if eopChildren.nonEmpty => {
        // TODO: The DFDL spec (12.3.6) explicitly mentions that an EndOfParent element
        // can be terminated by a choice with choiceLengthKind='explicit', with no reference
        // to implicit length choices. Later on in 12.3.6, it specified what the parent Effective
        // Length Units would be for an explicit length choice, again with no reference to
        // implicit length choices. The only way to get an EndOfParent element within an
        // implicit length choice would be to have it actually be terminated by the choice's
        // parent, similar to how we treat elements within an element that is also EndOfParent.
        // The only mention of implicit length choices in the DFDL Spec is to mention SDEs when
        // an EndOfParent element is enclosed by a choice with choiceLengthKind='implicit'. We
        // think that may be a typo, so for now we disallow ChoiceLengthKind.Implicit
        // enclosing an EndOfParent element.
        // See Daffodil-3080
        schemaDefinitionWhen(
          c.choiceLengthKind == ChoiceLengthKind.Implicit,
          "element is specified as dfdl:lengthKind=\"endOfParent\", but its parent is a choice with dfdl:choiceLengthKind 'implicit'."
        )
        schemaDefinitionWhen(
          c.hasTerminator,
          "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a choice with a dfdl:terminator."
        )
        schemaDefinitionWhen(
          c.trailingSkip != 0,
          "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a choice with a non-zero dfdl:trailingSkip."
        )
        val optParentELU = c.myEffectiveLengthUnits(optLastNonEOPLU)
        eopChildren.foreach {
          case e: ElementBase => {
            c.checkChildrenForSiblingsAfterEOPElement(e)
            Assert.invariant(
              optParentELU.isDefined,
              "Effective Length Units of parent should not be None"
            )
            e.checkEndOfParentRestrictionsOnCurrentElement(optParentELU)
          }
          case _ => // do nothing
        }
      }
      case _ => // do nothing
    }
    // end checks
    term.termChildren.foreach { e =>
      lazy val optParentELU = term.myEffectiveLengthUnits(optLastNonEOPLU)
      term match {
        case parent @ (_: ElementBase | _: ChoiceTermBase | _: SequenceTermBase) => {
          e.checkEndOfParentRestrictions(optParentELU)
        }
        case _ => // do nothing
      }
    }
  }

  def checkChildrenForSiblingsAfterEOPElement(specificChild: ElementBase) = {
    lazy val foundPosition = flattenedChildren.indexOf(specificChild)
    lazy val lastIndexOfChildren = flattenedChildren.length - 1
    if (flattenedChildren.isEmpty || foundPosition < 0) {
      // not found amongst children
      Assert.impossible("EndOfParent element not found amongst term children of parent")
    } else if (foundPosition != lastIndexOfChildren) {
      // get the following children after the EOP element+ 1
      val followingChildrenAfter =
        flattenedChildren.slice(foundPosition + 1, lastIndexOfChildren + 1)
      followingChildrenAfter.foreach {
        case m: ModelGroup => {
          specificChild.SDE(
            "element is specified as dfdl:lengthKind=\"endOfParent\", but a model group is defined between this element and the end of the enclosing component"
          )
        }
        case r if r.isRepresented => {
          specificChild.SDE(
            "element is specified as dfdl:lengthKind=\"endOfParent\", but a represented element is defined between this element and the end of the enclosing component"
          )
        }
        case _ => // do nothing
      }
    }
  }
}
