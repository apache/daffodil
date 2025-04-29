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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.dsom._
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.NilKind
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.xml.QNameBase
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.infoset.DoNotUseThisResolver
import org.apache.daffodil.runtime1.infoset.NoNextElement
import org.apache.daffodil.runtime1.infoset.OnlyOnePossibilityForNextElement
import org.apache.daffodil.runtime1.infoset.PartialNextElementResolver
import org.apache.daffodil.runtime1.infoset.SeveralPossibilitiesForNextElement
import org.apache.daffodil.runtime1.processors.ByteOrderEv
import org.apache.daffodil.runtime1.processors.CheckBitOrderAndCharsetEv
import org.apache.daffodil.runtime1.processors.CheckByteAndBitOrderEv
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData

/**
 * A set of possibilities for elements that can arrive as events
 * on the input event stream of the streaming unparser.
 */
sealed trait PossibleNextElements {
  import PossibleNextElements._
  def pnes: Seq[PossibleNextElement]
  final def isClosed = !isOpen
  def isOpen: Boolean
}

object PossibleNextElements {

  /**
   * PNE stands for Possible Next Element.
   *
   * Allows overriding the isRequiredStreamingUnparserEvent characteristic
   * of a term.
   *
   * Used when a required ERD appears in a child of a choice.
   * In that case, that ERD is optional since we could be on
   * another choice branch.
   */
  case class PNE(e: ElementBase, overrideIsRequiredStreamingUnparserEvent: Boolean)
  type PossibleNextElement = PNE

  /**
   * Indicates one of the possibilities must be found.
   * Simplest case for this is a run of optional elements followed
   * by a required element in a sequence. Can also occur if all
   * branches of a choice are Closed. Then the resulting choice
   * possibilities are also Closed.
   */
  case class Closed(override val pnes: Seq[PNE]) extends PossibleNextElements {
    def isOpen = false
  }

  /**
   * Indicates that these are possible, but all are optional
   * so a surrounding sequence group on the stack could also provide
   * a matching element.
   */
  case class Open(override val pnes: Seq[PNE]) extends PossibleNextElements {
    def isOpen = true
  }

  /**
   * Indicates that there should never be any use of possible next elements for
   * the associated Term (QuasiElements is the use case).
   */
  case object DoNotUse extends PossibleNextElements {
    override def pnes = Assert.usageError("do not use")
    override def isOpen = Assert.usageError("do not use")
  }
}

/**
 * Part of Daffodil's Runtime 1 Streaming Unparser Support.
 *
 * When streaming in some representation of an infoset for unparsing,
 * Daffodil (in Runtime 1) must resolve what element to construct.
 * This is context specific, and given sharing of elements and groups,
 * requires a runtime stack of TermRuntimeData, each stack entry has
 * an associated PartialNextElementResolver.
 *
 * This object computes that PartialNextElementResolver based on the
 * possible elements that can follow the current term.
 */
trait TermRuntime1Mixin { self: Term =>

  def termRuntimeData: TermRuntimeData

  import PossibleNextElements._

  /**
   * Finds possible next streaming unparser elements for this term itself.
   *
   * Disregards any surrounding structure,
   */
  lazy val possibleThisTermNextStreamingUnparserElements: PossibleNextElements =
    LV(Symbol("possibleThisTermNextStreamingUnparserElements")) {
      possibleThisTermNextStreamingUnparserElementsDef
    }.value

  // separate def assists breakpoint debugging
  private lazy val possibleThisTermNextStreamingUnparserElementsDef: PossibleNextElements = {
    val thisItself: PossibleNextElements = this match {
      //
      // An array may be required in the infoset, but the array
      // may also be terminated by finding the next element after the array,
      // so we get closed only if required and not an array.
      //
      case eb: ElementBase if (eb.isRequiredStreamingUnparserEvent) =>
        Closed(Seq(PNE(eb, true)))
      case eb: ElementBase =>
        Open(Seq(PNE(eb, false)))
      case sgr: SequenceGroupRef if sgr.isHidden =>
        Open(Nil)
      case cgr: ChoiceGroupRef if cgr.isHidden =>
        Open(Nil)
      case ctb: ChoiceTermBase => {
        val gms = ctb.groupMembers
        val individualBranchPossibles = gms.map {
          _.possibleThisTermNextStreamingUnparserElements
        }
        //
        // If all branches are closed, then the overall set will be
        // closed for the whole choice. Some element from the choice will
        // have to appear next. But no specific individual one is required.
        //
        val allBranchesClosed =
          individualBranchPossibles.forall {
            case c: Closed => true
            case o: Open => false
            case DoNotUse => Assert.invariantFailed("should never be DoNotUse")
          }
        //
        // Individual possible next elements (aka sibs) come from
        // various branches. Within those branches they may be required to appear
        // in the stream of events, but as alternatives of a choice, none of
        // them are specifically required, because you could always be
        // unparsing some other branch.
        // So in the PNE we override them to have false for
        // the overrideIsRequiredStreamingUnparserEvent.
        //
        val allSibsAsOptional = individualBranchPossibles.flatMap { poss =>
          poss.pnes.map {
            case PNE(eb, true) =>
              PNE(eb, false)
            case ok @ PNE(eb, false) =>
              ok
          }
        }
        val res: PossibleNextElements =
          if (allBranchesClosed) {
            Closed(allSibsAsOptional)
          } else {
            Open(allSibsAsOptional)
          }
        res
      }
      case stb: SequenceTermBase => {
        val subTerms = stb.groupMembers
        val res =
          subTerms.headOption.map {
            //
            // We headOption above, because here we only have to look at the first group member
            // so long as we ask for self plus lexical following information
            // as that will cover the other members.
            // properly cutting off any that are after a Closed one.
            //
            _.possibleSelfPlusNextLexicalSiblingStreamingUnparserElements
          }
        res.getOrElse(Open(Nil))
      }

    }
    thisItself
  }

  /*
   * Returns a Closed or Open list of posslble elements that could follow this Term, within its
   * lexically enclosing model group.
   *
   * Does not follow backpointers from group defs to group refs.
   *
   * It is ok, and has no negative impact on sharing for a Term to reference
   * its lexically enclosing model group.
   *
   * Within the lexically enclosing model group, if that group is a sequence, then
   * this computation involves subsequent siblings, children of those siblings.
   */
  lazy val (
    hasNamesDifferingOnlyByNS: Boolean,
    possibleNextLexicalSiblingStreamingUnparserElements: PossibleNextElements
  ) = {
    var hasNamesDifferingOnlyByNS = false
    val possibles = this match {
      // Quasi elements are used for type-value calc, and for prefixed lengths
      // we never consider them for streaming unparsing. They are never present as events.
      // Nor should they ever be used as the source of next-event information.
      case qe: QuasiElementDeclBase =>
        DoNotUse // never resolve siblings. These shouldn't pull unparser events ever
      //
      // For a sequence, the possible next siblings elements excludes children of
      // the sequence itself. This is because we only use this list to find things that
      // are AFTER the sequence itself. The contents of the sequence itself are
      // going to be handled by the "self" cases for elements and choices.
      // So we use just the following siblings.
      //
      // In the actual runtime, this information is never on top of the dynamic context
      // stack. It is always down at least one on the stack. It is only used when the
      // term itself has not provided a match to the incoming event, and we need to
      // see if things following the term provide a match.
      //
      case stb: SequenceTermBase => {
        followingLexicalSiblingStreamingUnparserElements
      }
      case _: ElementBase | _: ChoiceTermBase => {
        // If this element is closed, we only want it and not it's siblings
        if (possibleThisTermNextStreamingUnparserElements.isClosed)
          possibleThisTermNextStreamingUnparserElements
        else
          possibleSelfPlusNextLexicalSiblingStreamingUnparserElements
      }
    }
    //
    // Check for ambiguity except for namespaces
    // Since some Infoset representations (e.g., JSON) can't support that
    //
    val sibs = possibles match {
      case PossibleNextElements.Closed(sibs) => sibs
      case PossibleNextElements.Open(sibs) => sibs
      case PossibleNextElements.DoNotUse => Nil
    }
    if (sibs.size > 1) {
      val groupedByName = possibles.pnes.groupBy(_.e.namedQName.local)
      groupedByName.foreach { case (_, sameNamesEB) =>
        if (sameNamesEB.length > 1) {
          val groupedByNamespace = sameNamesEB.groupBy(_.e.namedQName.namespace)
          if (groupedByNamespace.size > 1) {
            SDW(
              WarnID.NamespaceDifferencesOnly,
              "Neighboring QNames differ only by namespaces. " +
                "Infoset representations that do not support namespaces " +
                "cannot differentiate between these elements and " +
                "may fail to unparse. QNames are: %s",
              sameNamesEB.map(_.e.namedQName.toExtendedSyntax).mkString(", ")
            )
            hasNamesDifferingOnlyByNS = true
          }
        }
      }
    }
    (hasNamesDifferingOnlyByNS, possibles)
  }

  final protected lazy val possibleSelfPlusNextLexicalSiblingStreamingUnparserElements
    : PossibleNextElements =
    LV(Symbol("possibleSelfPlusNextLexicalSiblingStreamingUnparserElements")) {
      val thisItself: PossibleNextElements = this match {
        //
        // An array may be required in the infoset, but the array
        // may also be terminated by finding the next element after the array,
        // so we get closed only if required and not an array.
        //
        case eb: ElementBase if (eb.isRequiredStreamingUnparserEvent) =>
          Closed(Seq(PNE(eb, true)))
        case eb: ElementBase =>
          Open(Seq(PNE(eb, false)))
        case gr: GroupRef if gr.isHidden => Open(Nil)
        case ctb: ChoiceTermBase => {
          val individualBranchPossibles = ctb.groupMembers.map {
            _.possibleSelfPlusNextLexicalSiblingStreamingUnparserElements
          }
          //
          // If all branches are closed, then the overall set will be
          // closed for the whole choice. Some element from the choice will
          // have to appear next. But no specific individual one is required.
          //
          val allBranchesClosed =
            individualBranchPossibles.forall {
              case c: Closed => true
              case o: Open => false
              case DoNotUse => Assert.invariantFailed("should never be DoNotUse")
            }
          //
          // Individual possible next elements (aka sibs) come from
          // various branches. Within those branches they may be required to appear
          // in the stream of events, but as alternatives of a choice, none of
          // them are specifically required, because you could always be
          // unparsing some other branch.
          // So in the PNE we override them to have false for
          // the overrideIsRequiredStreamingUnparserEvent.
          //
          val allSibsAsOptional = individualBranchPossibles.flatMap { poss =>
            poss.pnes.map {
              case PNE(eb, true) =>
                PNE(eb, false)
              case ok @ PNE(eb, false) =>
                ok
            }
          }
          val res: PossibleNextElements =
            if (allBranchesClosed) {
              Closed(allSibsAsOptional)
            } else {
              Open(allSibsAsOptional)
            }
          res
        }
        case stb: SequenceTermBase => {
          //
          // This case only applies to when we are analyzing a sequence, but it is
          // being considered as contributing possible elements that are after the
          // end of a Term.
          //
          // In this case, we DO recursively walk into the sequence's own children
          //
          val subTerms = stb.groupMembers
          val res =
            subTerms.headOption.map {
              _.possibleSelfPlusNextLexicalSiblingStreamingUnparserElements
            }
          res.getOrElse(Open(Nil))
        }
      }
      Logger.log.debug(s"""
        NextElementResolver -> this: $this\n
        NextElementResolver -> thisItself: $thisItself\n
        NextElementResolver -> following: $followingLexicalSiblingStreamingUnparserElements""")
      val res: PossibleNextElements =
        (thisItself, followingLexicalSiblingStreamingUnparserElements) match {
          case (_, Closed(Nil)) => {
            thisItself // case of Open(...) followed by end of complex element.
          }
          case (Closed(pnes1), Open(List())) => {
            thisItself // case of Closed(...) followed by empty list, which is default for nothing
          }
          case (Closed(pnes1), Closed(pnes2)) => {
            thisItself // When everything is closed, we only want thisItself and not the entire list of Closed possibilities
          }
          case (Open(pnes1), Closed(pnes2)) if !this.isInstanceOf[ChoiceGroupRef] => {
            // If there are optional elements before a required element, the whole sequence is considered closed
            // unless this is a ChoiceGroupRef as the group could be shared and may not have the appropriate
            // context to find the expected following element. By not closing when it is a ChoiceGroupRef we
            // allow other contexts to be tried if necessary.
            Closed((pnes1 ++ pnes2).distinct)
          }
          case (poss1, poss2) => {
            Open((poss1.pnes ++ poss2.pnes).distinct)
          }
        }
      res
    }.value

  /**
   * Computes the possible next elements after a term, not including any that
   * the term itself may have as possibilities.
   */
  private lazy val followingLexicalSiblingStreamingUnparserElements: PossibleNextElements =
    LV(Symbol("followingLexicalSiblingStreamingUnparserElements")) {
      Assert.invariant(optLexicalParent.isDefined)
      val lexicalParent = optLexicalParent.get
      lexicalParent match {
        case parentSeqDef: SequenceDefMixin => {
          //
          // start after this term in the lexically enclosing sequence siblings
          //
          val sibTerms = parentSeqDef.groupMembersNotShared.drop(position)
          //
          // compute what is possible for each of them.
          //
          val sibPossibles = sibTerms.map {
            _.possibleSelfPlusNextLexicalSiblingStreamingUnparserElements
          }
          //
          // split the possibles into the opens and closed lists
          // Then assemble the opens plus the first closed if it exists.
          //
          val (opens, closedAndAfter) = sibPossibles.span { _.isInstanceOf[Open] }
          val optFirstClosed = closedAndAfter.headOption
          val opensPlusFirstClosed = opens ++ optFirstClosed
          val hasClosed = optFirstClosed.isDefined

          val combinedSibs = opensPlusFirstClosed.flatMap { _.pnes }.distinct
          val res: PossibleNextElements =
            if (hasClosed) {
              Closed(combinedSibs)
            } else {
              Open(combinedSibs)
            }
          res
        }
        case c: ChoiceDefMixin => {
          // This is a branch of a choice
          // So what follows this branch is whatever follows the choice
          // but we don't compute that statically, because if this
          // is a term inside a global choice group def, then we'd have
          // to know about the dynamic context.
          // Instead we just leave this open
          // and at runtime a stack will have the enclosing dynamic term's info.
          Open(Nil)
        }
        case ct: ComplexTypeBase => {
          // This is the model group of a complex type element
          Assert.invariant(this.isInstanceOf[ModelGroup])
          Closed(Nil)
        }
        case sd: SchemaDocument => {
          // Can only happen for Root
          Assert.invariant(this.isInstanceOf[Root])
          Closed(Nil)
        }
        case _ =>
          Assert.invariantFailed("unexpected lexical parent type for term: " + lexicalParent)
      }
    }.value

  /**
   * The PartialNextElementResolver is used to determine what infoset event comes next, and "resolves" which is to say
   * determines the ElementRuntimeData for that infoset event. This can be used to construct the initial
   * infoset from a stream of XML events.
   */
  final lazy val partialNextElementResolver: PartialNextElementResolver = {
    val context = self
    val possibles = possibleNextLexicalSiblingStreamingUnparserElements
    self match {
      case _: QuasiElementDeclBase => {
        Assert.invariant(possibles eq PossibleNextElements.DoNotUse)
        new DoNotUseThisResolver(termRuntimeData) // Does an assert fail if used.
      }
      case _ => {
        val trd = context.termRuntimeData
        val (sibs, isRequiredStreamingUnparserEvent) = possibles match {
          case PossibleNextElements.Closed(sibs) => (sibs, true)
          case PossibleNextElements.Open(sibs) => (sibs, false)
          case PossibleNextElements.DoNotUse =>
            Assert.invariantFailed("should never be DoNotUse")
        }
        //
        // Annoying, but scala's immutable Map is not covariant in its first argument
        // the way one would normally expect a collection to be.
        // So Map[NamedQName, ElementRuntimeData] is not a subtype of Map[QNameBase, ElementRuntimeData]
        // So we need a cast upward to Map[QNameBase,ElementRuntimeData]
        // We need the fold because .toMAp overwrites earlier element with duplicate later elements,
        // but we don't want that, so instead we deduplicate here with earlier elements taking precedence
        val eltMap = sibs
          .map { sib =>
            (sib.e.namedQName, sib.e.erd)
          }
          .foldLeft(Map.empty[QNameBase, ElementRuntimeData]) { case (map, (key, value)) =>
            if (map.contains(key)) map else map + (key -> value)
          }
          .asInstanceOf[Map[QNameBase, ElementRuntimeData]]
        val resolver = eltMap.size match {
          case 0 => new NoNextElement(trd, isRequiredStreamingUnparserEvent)
          case 1 =>
            new OnlyOnePossibilityForNextElement(
              trd,
              sibs.head.e.erd,
              isRequiredStreamingUnparserEvent
            )
          case _ => {
            new SeveralPossibilitiesForNextElement(
              trd,
              eltMap,
              hasNamesDifferingOnlyByNS,
              isRequiredStreamingUnparserEvent
            )
          }
        }
        resolver
      }
    }
  }

  /**
   * For streaming unparser, determines if this Term could
   * have suspensions associated with it.
   */
  final protected lazy val couldHaveSuspensions: Boolean = {
    val commonCouldHaveSuspensions =
      !isKnownToBeAligned || // AlignmentFillUnparser
        (if (hasDelimiters) !isDelimiterKnownToBeTextAligned else false)

    this match {
      case eb: ElementBase => {
        val elementCouldHaveSuspensions =
          commonCouldHaveSuspensions ||
            !isKnownToBeTextAligned || // MandatoryTextAlignmentUnparser
            (if (eb.isSimpleType) eb.isOutputValueCalc else false) || // SimpleTypeRetryUnparser
            eb.shouldAddFill || // ElementUnusedUnparser, RightFillUnparser
            eb.shouldCheckExcessLength || // ElementUnusedUnparser, RightFillUnparser
            eb.shouldAddPadding || // OnlyPaddingUnparser, RightCenteredPaddingUnparser, LeftCenteredPaddingUnparser
            (eb.maybeUnparseTargetLengthInBitsEv.isDefined && eb.isNillable && eb.nilKind == NilKind.LiteralCharacter) || // NilLiteralCharacterUnparser
            (if (eb.isComplexType) eb.complexType.group.couldHaveSuspensions else false)

        elementCouldHaveSuspensions
      }
      case mg: ModelGroup => {
        val modelGroupCouldHaveSuspensions =
          commonCouldHaveSuspensions ||
            mg.groupMembers.exists { _.couldHaveSuspensions }

        modelGroupCouldHaveSuspensions
      }
    }
  }

  /**
   * For a choice branch, provides a collection (unordered) of events that indicate the
   * selection of that branch.
   *
   * This can be an empty list, because a branch can contain no elements yet be legal (such
   * as a branch containing only say, dfdl assertions, or just a hidden group).
   * In that case the occurrence of an
   * event that doesn't indicate any other branch implicitly indicates the first
   * branch that has an empty list of identifying events.
   *
   * Note that this is NOT the same as the first branch with an Open list of events.
   *
   * Specifically, we don't search past the end of the choice to find out what is next.
   * This analysis occurs within each choice branch only.
   */
  final def identifyingEventsForChoiceBranch: PossibleNextElements =
    possibleThisTermNextStreamingUnparserElements

  /**
   * Set of elements referenced from an expression in the scope of this term.
   *
   * Specific to certain function call contexts e.g., only elements referenced
   * by dfdl:valueLength or dfdl:contentLength.
   *
   * Separated by parser/unparser since parsers have to derive from
   * dfdl:inputValueCalc, and must include discriminators and assert test
   * expressions. Unparsers must derive from dfdl:outputValueCalc and exclude
   * discriminators and asserts. Both must include setVariable/newVariableInstance,
   * and property expressions are nearly the same. There are some unparser-specfic
   * properties that take runtime-valued expressions - dfdl:outputNewLine is
   * one example.
   */
  final lazy val contentLengthParserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyContentReferencedElementInfos
    val stmtRefs = statementContentParserReferencedElementInfos
    val calcRefs = calcContentParserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) =>
      s.union(i.contentLengthParserReferencedElementInfos)
    }
    res
  }

  /**
   * Any element referenced from an expression in the scope of this term
   * is in this set.
   */
  final lazy val contentLengthUnparserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyContentReferencedElementInfos
    val stmtRefs = statementContentUnparserReferencedElementInfos
    val calcRefs = calcContentUnparserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) =>
      s.union(i.contentLengthUnparserReferencedElementInfos)
    }
    res
  }

  /**
   * Any element referenced from an expression in the scope of this term
   * is in this set.
   */
  final lazy val valueLengthParserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyValueReferencedElementInfos
    val stmtRefs = statementValueParserReferencedElementInfos
    val calcRefs = calcValueParserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) =>
      s.union(i.valueLengthParserReferencedElementInfos)
    }
    res
  }

  /**
   * Any element referenced from an expression in the scope of this term
   * is in this set.
   */
  final lazy val valueLengthUnparserReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val propRefs = propertyValueReferencedElementInfos
    val stmtRefs = statementValueUnparserReferencedElementInfos
    val calcRefs = calcValueUnparserReferencedElementInfos
    val locRefs = propRefs ++ stmtRefs ++ calcRefs
    val res = realChildren.foldLeft(locRefs) { (s, i) =>
      s.union(i.valueLengthUnparserReferencedElementInfos)
    }
    res
  }

  private lazy val mboEv: Maybe[ByteOrderEv] = self match {
    case eb: ElementBase => eb.maybeByteOrderEv
    case _ => Maybe.Nope
  }

  lazy val maybeCheckByteAndBitOrderEv: Maybe[CheckByteAndBitOrderEv] = {
    //
    // TODO: Performance: could be improved, as there are situations where byteOrder
    // is defined, but still we know it will not be used and this could
    // be Nope in those cases also. An example would be a 100% text-only item.
    //
    if (!isRepresented || !optionByteOrderRaw.isDefined)
      Maybe.Nope
    else {
      val checkByteAndBitOrder = {
        val ev = new CheckByteAndBitOrderEv(ci, defaultBitOrder, mboEv)
        ev.compile(tunable)
        ev
      }
      Maybe(checkByteAndBitOrder)
    }
  }

  lazy val maybeCheckBitOrderAndCharsetEv: Maybe[CheckBitOrderAndCharsetEv] = {
    lazy val se = summaryEncoding
    if (!isRepresented || se == NoText || se == Binary)
      Maybe.Nope
    else {
      val checkBitOrderAndCharset = {
        val ev = new CheckBitOrderAndCharsetEv(ci, defaultBitOrder, charsetEv)
        ev.compile(tunable)
        ev
      }
      Maybe(checkBitOrderAndCharset)
    }
  }

}
