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

  def optEffectiveLengthUnits(optLastNonEOPELU: Option[LengthUnits]): Option[LengthUnits] = {
    this match {
      case e: ElementBase =>
        e.lengthKind match {
          case LengthKind.EndOfParent => optLastNonEOPELU
          case LengthKind.Explicit | LengthKind.Prefixed => Some(e.lengthUnits)
          case LengthKind.Pattern => Some(LengthUnits.Characters)
          case LengthKind.Implicit | LengthKind.Delimited =>
            None // invalid parent; SDE fires separately
        }
      case c: ChoiceTermBase if c.choiceLengthKind == ChoiceLengthKind.Explicit =>
        Some(LengthUnits.Bytes)
      // Sequences are transparent — the ELU is inherited from the nearest enclosing box.
      case _: SequenceTermBase => optLastNonEOPELU
      case _: ChoiceTermBase => None // implicit-length choice; SDE fires separately
    }
  }

  final lazy val childrenEndOfParent: Seq[ElementBase] = LV(Symbol("childrenEndOfParent")) {
    val gms = termChildren
    val chls = gms.flatMap {
      case eb: ElementBase if eb.lengthKind == LengthKind.EndOfParent => Seq(eb)
      case eb: ElementBase => Nil
      case c: ChoiceTermBase => Nil
      case mg: ModelGroup => mg.childrenEndOfParent
    }
    chls
  }.value

  def checkEndOfParentRestrictions(lastNonEOPELU: Option[LengthUnits]): Boolean = {
    val term = this
    lazy val eopChildren = this.childrenEndOfParent
    lazy val optParentELU = term.optEffectiveLengthUnits(lastNonEOPELU)
    // checks
    term match {
      case rootElem: Root if rootElem.lengthKind == LengthKind.EndOfParent => {
        rootElem.checkEndOfParentRestrictionsOnCurrentElement(Some(LengthUnits.Characters))
        eopChildren.foreach { child =>
          child.checkEndOfParentRestrictionsOnCurrentElement(lastNonEOPELU)
        }
      }
      case e: ElementBase if eopChildren.nonEmpty => {
        eopChildren.foreach { child =>
          child.schemaDefinitionWhen(
            e.lengthKind == LengthKind.Implicit || e.lengthKind == LengthKind.Delimited,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but its parent is an element with dfdl:lengthKind 'implicit' or 'delimited'."
          )
          child.checkEndOfParentRestrictionsOnCurrentElement(optParentELU)
        }
      }
      case s: SequenceTermBase if eopChildren.nonEmpty => {
        eopChildren.foreach { child =>
          child.schemaDefinitionWhen(
            s.separatorPosition == SeparatorPosition.Postfix,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with dfdl:separatorPosition defined as 'postfix'."
          )
          child.schemaDefinitionWhen(
            s.sequenceKind != SequenceKind.Ordered,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with dfdl:sequenceKind defined as 'unordered'."
          )
          child.schemaDefinitionWhen(
            s.hasTerminator,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with a dfdl:terminator."
          )
          child.schemaDefinitionWhen(
            s.realElementChildren.exists(e => e.floating == YesNo.Yes),
            "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with elements defining dfdl:floating='yes'."
          )
          child.schemaDefinitionWhen(
            s.trailingSkip != 0,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a sequence with a non-zero dfdl:trailingSkip."
          )
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
        eopChildren.foreach { child =>
          child.schemaDefinitionWhen(
            c.choiceLengthKind == ChoiceLengthKind.Implicit,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but its parent is a choice with dfdl:choiceLengthKind 'implicit'."
          )
          child.schemaDefinitionWhen(
            c.hasTerminator,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a choice with a dfdl:terminator."
          )
          child.schemaDefinitionWhen(
            c.trailingSkip != 0,
            "element is specified as dfdl:lengthKind=\"endOfParent\", but is in a choice with a non-zero dfdl:trailingSkip."
          )
          child.checkEndOfParentRestrictionsOnCurrentElement(optParentELU)
        }
      }
      case _ => // do nothing
    }
    // end checks
    val sawEOP = term.termChildren.foldLeft(false) { case (sawEOP, child) =>
      if (sawEOP) {
        // Choice branches are alternatives, not sequential data — the after-EOP SDE must
        // not fire across branches. Only sequences and elements have sequential ordering.
        term match {
          case _: ChoiceTermBase => // has alternatives which can all be EOP; skip
          case _ =>
            child.schemaDefinitionWhen(
              child.isInstanceOf[ModelGroup],
              "element is specified as dfdl:lengthKind=\"endOfParent\", but a model group is defined between this element and the end of the enclosing component"
            )
            child.schemaDefinitionWhen(
              child.isRepresented,
              "element is specified as dfdl:lengthKind=\"endOfParent\", but a represented element is defined between this element and the end of the enclosing component"
            )
        }
      }
      val res = child.checkEndOfParentRestrictions(optParentELU)
      term match {
        // Branches of a choice are alternatives, so should never carry sawEOP from one branch to the next.
        case _: ChoiceTermBase => false
        case _ =>
          child match {
            case e: ElementBase if e.lengthKind == LengthKind.EndOfParent => true
            // A non-EOP element forms a hard length boundary. EOP elements nested inside it are
            // scoped to that element's length, so they must not affect sibling scanning here.
            case _: ElementBase => false
            // An explicit-length choice owns a fixed byte span, so EOP elements inside it are
            // scoped to that span. After the choice ends the parent continues normally.
            case c: ChoiceTermBase if c.choiceLengthKind == ChoiceLengthKind.Explicit => false
            // Sequences are transparent (no own length boundary),
            // so EOP state propagates through them.
            case _ => res
          }
      }
    }
    sawEOP
  }
}
