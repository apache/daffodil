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

import java.util.UUID
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.grammar.TermGrammarMixin
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import java.lang.{ Integer => JInt }
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.schema.annotation.props.gen.NilKind

/**
 * Term, and what is and isn't a Term, is a key concept in DSOM.
 *
 * From elements, ElementRef and LocalElementDecl are Term. A GlobalElementDecl is *not* a Term.
 * From sequences, Sequence and SequenceGroupRef are Term. GlobalSequenceGroupDef is *not* a Term.
 * From choices, Choice and ChoiceGroupRef are Term. GlobalChoiceGroupDef is *not* a Term.
 *
 * Terms are the things we actually generate parsers/unparsers for. Non-Terms just
 * contribute information used by Terms.
 */
trait Term
  extends AnnotatedSchemaComponent
  with ResolvesProperties
  with ResolvesDFDLStatementMixin
  with TermRuntimeValuedPropertiesMixin
  with TermGrammarMixin
  with DelimitedRuntimeValuedPropertiesMixin
  with InitiatedTerminatedMixin
  with TermEncodingMixin {

  def position: Int

  def optIgnoreCase: Option[YesNo] = {
    val ic = cachePropertyOption("ignoreCase")
    ic match {
      case Found(value, location, _, _) => Some(YesNo(value, location))
      case _ => None
    }
  }

  /**
   * A scalar means has no dimension. Exactly one occurrence.
   *
   * Since terms include both model groups and elements, in DFDL v1.0,
   * model groups are always scalar, as DFDL v1.0 doesn't allow min/max
   * occurs on model groups.
   */
  def isScalar: Boolean

  /**
   * Determines if the element is optional, as in has zero or one instance only.
   *
   * There are two senses of optional
   *
   * 1) Optional as in "might not be present" but for any reason.
   * Consistent with this is Required meaning must occur (but for any
   * reason. So all the occurrences of an array that has fixed number of
   * occurrences are required, and some of the occurrances of an array
   * that has a variable number of occurrences are optional.
   *
   * 2) Optional is in minOccurs="0" maxOccurs="1".
   *
   * Consistent with (2) is defining array as maxOccurs >= 2, and Required
   * as minOccurs=maxOccurs=1, but there are also special cases for occursCountKind parsed and stopValue
   * since they don't examine min/max occurs - they are only used for validation
   * in those occursCountKinds.
   *
   * The DFDL spec is not entirely consistent here either I don't believe.
   */
  def isOptional: Boolean

  /**
   * Something is required if it is not optional
   * and not an array, unless that array has required elements.
   */
  def isRequired: Boolean

  /**
   * An array can have more than 1 occurrence.
   *
   * An optional element (minOccurs=0, maxOccurs=1) is an array only
   * if occursCountKind is parsed, because then the max/min are ignored.
   */
  def isArray: Boolean

  def termRuntimeData: TermRuntimeData

  def elementChildren: Seq[ElementBase]

  /**
   * An integer which is the alignment of this term. This takes into account the
   * representation, type, charset encoding and alignment-related properties.
   */
  def alignmentValueInBits: JInt

  /**
   * True if this term is known to have some text aspect. This can be the value, or it can be
   * delimiters.
   * <p>
   * False only if this term cannot ever have text in it. Example: a sequence with no delimiters.
   * Example: a binary int with no delimiters.
   * <p>
   * Note: this is not recursive - it does not roll-up from children terms.
   * TODO: it does have to deal with the prefix length situation. The type of the prefix
   * may be textual.
   * <p>
   * Override in element base to take simple type or prefix length situations into account
   */
  lazy val couldHaveText = hasDelimiters

  //TODO: if we add recursive types capability to DFDL this will have to change
  // but so will many of these compiler passes up and down through the DSOM objects.

  /**
   * The termChildren are the children that are Terms, i.e., derived from the Term
   * base class. This is to make it clear
   * we're not talking about the XML structures inside the XML parent (which might
   * include annotations, etc.
   *
   * For elements this is Nil for simple types, a single model group for
   * complex types. For model groups there can be more children.
   */
  def termChildren: Seq[Term]

  final val tID = UUID.randomUUID()

  // Scala coding style note: This style of passing a constructor arg that is named fooArg,
  // and then having an explicit val/lazy val which has the 'real' name is
  // highly recommended. Lots of time wasted because a val constructor parameter can be
  // accidently hidden if a derived class uses the same name as one of its own parameters.
  // These errors just seem easier to deal with if you use the fooArg style.

  lazy val someEnclosingComponent = enclosingComponent.getOrElse(Assert.invariantFailed("All terms except a root element have an enclosing component."))

  /** Overridden as false for elements with dfdl:inputValueCalc property. */
  lazy val isRepresented = true

  /**
   * nearestEnclosingSequence
   *
   * An attribute that looks upward to the surrounding
   * context of the schema, and not just lexically surrounding context. It needs to see
   * what declarations will physically surround the place. This is the dynamic scope,
   * not just the lexical scope. So, a named global type still has to be able to
   * ask what sequence is surrounding the element that references the global type.
   *
   * This is why we have to have the GlobalXYZDefFactory stuff. Because this kind of back
   * pointer (contextual sensitivity) prevents sharing.
   */
  final lazy val nearestEnclosingSequence: Option[SequenceTermBase] = enclosingTerm match {
    case None => None
    case Some(s: SequenceTermBase) => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingSequence
  }

  final lazy val nearestEnclosingChoiceBeforeSequence: Option[ChoiceTermBase] = enclosingTerm match {
    case None => None
    case Some(s: SequenceTermBase) => None
    case Some(c: ChoiceTermBase) => Some(c)
    case Some(_) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
  }

  final lazy val nearestEnclosingUnorderedSequence: Option[SequenceTermBase] = enclosingTerm match {
    case None => None
    case Some(s: SequenceTermBase) if !s.isOrdered => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  final lazy val nearestEnclosingUnorderedSequenceBeforeSequence: Option[SequenceTermBase] = enclosingTerm match {
    case None => None
    case Some(s: SequenceTermBase) if !s.isOrdered => Some(s)
    case Some(s: SequenceTermBase) => None
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  final lazy val inChoiceBeforeNearestEnclosingSequence: Boolean = enclosingTerm match {
    case None => false
    case Some(s: SequenceTermBase) => false
    case Some(c: ChoiceTermBase) => true
    case Some(_) => enclosingTerm.get.inChoiceBeforeNearestEnclosingSequence
  }

  final lazy val nearestEnclosingElement: Option[ElementBase] = enclosingTerm match {
    case None => None
    case Some(eb: ElementBase) => Some(eb)
    case Some(_) => enclosingTerm.get.nearestEnclosingElement
  }

  /**
   * We want to determine if we're in an unordered sequence
   * at any point along our parents.
   */
  final lazy val inUnorderedSequence: Boolean =
    nearestEnclosingSequence match {
      case None => {
        false
      }
      case Some(s) => {
        if (s.isOrdered) {
          val result = s.inUnorderedSequence
          result
        } else true
      }
    }

  final lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] = {
    val res = parent match {
      case c: ChoiceTermBase => Some(c)
      case s: SequenceTermBase => Some(s)
      case d: SchemaDocument => {
        // we must be the Root elementRef
        Assert.invariant(this.isInstanceOf[Root])
        None
      }
      case gr: GroupRef => gr.asModelGroup.immediatelyEnclosingModelGroup
      case gdd: GlobalGroupDef => Some(gdd.groupRef.asModelGroup)
      case ged: GlobalElementDecl => ged.elementRef.immediatelyEnclosingModelGroup
      case ct: ComplexTypeBase => {
        None
        // The above formerly was ct.element.immediatelyEnclosingModelGroup,
        // but if we have a CT as our parent, the group around the element whose type
        // that is, isn't "immediately enclosing".
      }
      case _ => Assert.invariantFailed("immediatelyEnclosingModelGroup called on " + this + "with parent " + parent)
    }
    res
  }

  final lazy val positionInNearestEnclosingSequence: Int = {
    val res =
      if (enclosingComponent == nearestEnclosingSequence) position
      else if (this.isInstanceOf[Root]) 1
      else {
        enclosingComponent match {
          case Some(term: Term) => term.positionInNearestEnclosingSequence
          case Some(ct: ComplexTypeBase) => {
            val ctPos = ct.elementDecl match {
              case eb: ElementBase => eb.positionInNearestEnclosingSequence
              case ged: GlobalElementDecl => ged.elementRef.positionInNearestEnclosingSequence
            }
            ctPos
          }
          case Some(ggd: GlobalGroupDef) => ggd.groupRef.asModelGroup.positionInNearestEnclosingSequence
          case x => Assert.invariantFailed("For " + this + " unable to compute position in nearest enclosing sequence. The enclosingComponent was " + x)
        }
      }
    res
  }

  final lazy val isDirectChildOfSequence = parent.isInstanceOf[Sequence]

  import org.apache.daffodil.util.ListUtils

  final lazy val allSiblings: Seq[Term] = {
    val res = nearestEnclosingSequence.map { enc =>
      val allSiblings = enc.groupMembers
      allSiblings
    }
    res.getOrElse(Nil)
  }

  final lazy val priorSiblings = ListUtils.preceding(allSiblings, this)
  final lazy val laterSiblings = ListUtils.tailAfter(allSiblings, this)
  final lazy val laterElementSiblings = laterSiblings.collect { case elt: ElementBase => elt }

  final lazy val laterSiblingsWithinEnclosingElement: Seq[Term] = {
    enclosingElement.flatMap { ee =>
      enclosingTerm.map { et =>
        val eeGroup = ee.complexType.group
        val res =
          laterSiblings ++
            (if (et eq eeGroup) Nil
            else et.laterSiblingsWithinEnclosingElement)
        res
      }
    }.getOrElse(Nil)
  }

  final lazy val priorSibling = priorSiblings.lastOption
  final lazy val nextSibling = laterSiblings.headOption

  final lazy val priorPhysicalSiblings = priorSiblings.filter { _.isRepresented }
  final lazy val priorPhysicalSibling = priorPhysicalSiblings.lastOption

  //
  // FIXME: incomplete analysis. This needs to walk outward to parent, then down into
  // last of prior sibling sequence group looking downward at last child until it finds
  // a physical term that satisfies the test.
  // E.g., the prior sibling in this sequence might satisfy, or the enclosing parent if we're
  // first, or the prior sibling of the enclosing parent, or the last child of the prior
  // sibling of the enclosing parent, and so on.
  //
  // Really we often need the "things before this" enumerated and filtered, so there
  // should be a stream of things looking at prior prior of that, prior of that, etc.
  //
  // Choice groups require special consideration. A prior that's a choice only has a
  // defined property if (1) it's relevant to the choice group - so dfdl:terminator yes, dfdl:byteOrder no.
  // (2) it is present for that choice group, or (3) it is recursively present on the last of
  // ALL children of the choice group, so that it is present with a specific value no matter
  // which branch of the choice is realized.
  //
  // It is ok for this to stop early and be less comprehensive about walking backward
  // IFF it is used in conservative analysis, i.e., where not finding the term, even if
  // it does in fact exist back there someplace, causes no incorrectness, just suboptimality.
  //
  // Note also that the predicate test interacts with sequence groups in a complex way.
  // If the sequence group has separators, the separator will be present (because the current
  // term is not first, or the sep is in prefix position, or ...) then if the predicate
  // is true of a sequence separator (e.g., such as has same encoding property value) then
  // we have a preceding physical term, the enclosing sequence, which has a physical
  // syntax, the separator, which satisfies the predicate.
  //
  // That's the job of the predicate. The point is that this predicate may or may not
  // stop on some enclosing parent, depending on separators, etc. You can't just have the
  // predicate be "has same encoding" test, because whether that encoding will have been
  // put into effect depends on whether some group syntax - such as a separator, or initiator
  // will have been present and so required establishing that property to be in effect.
  //
  // If a sequence has no separator and no initiator, then it doesn't impose an encoding prior to or
  // between the sibling children. Hence, even if it has an encoding property in scope, and even
  // uses it for a terminator, it doesn't re-establish that encoding prior to children, so
  // the analysis can't stop on the sequence.
  final def nearestPriorPhysicalTermSatisfying(pred: Term => Boolean): Option[Term] = {
    priorPhysicalSiblings.filter { pred(_) }.lastOption match {
      case x @ Some(sib) => x
      case None => {
        // must try enclosing terms outward
        enclosingTerm match {
          case None => None
          case x @ Some(t) if pred(t) => x
          case Some(t) => t.nearestPriorPhysicalTermSatisfying(pred)
        }
      }
    }
  }

  final lazy val hasLaterRequiredSiblings = laterSiblings.exists(_.hasStaticallyRequiredInstances)
  final lazy val hasPriorRequiredSiblings = priorSiblings.exists(_.hasStaticallyRequiredInstances)

  def hasStaticallyRequiredInstances: Boolean
  def isKnownRequiredElement = false
  def hasKnownRequiredSyntax = false

  def hasPotentiallyTrailingInstances: Boolean
  final def isPotentiallyTrailing = LV('isPotentiallyTrailing) {
    if (!isRequired) {
      val es = nearestEnclosingSequence
      val res = es match {
        case None => true
        case Some(s) => {
          val allRequired = s.groupMembers.filter(_.isRequired)
          val lastDeclaredRequired = allRequired.last
          if (s.groupMembers.indexOf(lastDeclaredRequired) < s.groupMembers.indexOf(this)) true
          else false
        }
      }
      res
      // Since we can't determine at compile time, return false so that we can continue processing.
      // Runtime checks will make final determination.
    } else false
  }.value

  /**
   * Returns a tuple, where the first item in the tuple is the list of sibling
   * terms that could appear before this. The second item in the tuple is a
   * One(parent) if all siblings are optional or this element has no prior siblings
   */
  lazy val potentialPriorTerms: (Seq[Term], Option[Term]) = {
    val (potentialPrior, parent) = enclosingTerm match {
      case None => (Seq(), None)
      case Some(eb: ElementBase) => (Seq(), Some(eb))
      case Some(ch: ChoiceTermBase) => (Seq(), Some(ch))
      case Some(sq: SequenceTermBase) if !sq.isOrdered => (sq.groupMembers, Some(sq))
      case Some(sq: SequenceTermBase) if sq.isOrdered => {
        val previousTerms = sq.groupMembers.takeWhile { _ != this }
        if (previousTerms.isEmpty) {
          // first child of seq, the seq is the only previous term
          (Seq(), Some(sq))
        } else {
          val firstNonOptional = previousTerms.reverse.find {
            _ match {
              case eb: ElementBase if !eb.isRequired || !eb.isRepresented => false
              case _ => true
            }
          }
          if (firstNonOptional.isEmpty) {
            // all previous siblings are optional, all or the seq could be previous
            (previousTerms, Some(sq))
          } else {
            // drop all siblings up until the first non optional
            (previousTerms.dropWhile { _ != firstNonOptional.get }, None)
          }
        }
      }
    }
    val potentialPriorRepresented = potentialPrior.filter { term =>
      term match {
        case eb: ElementBase => eb.isRepresented
        case _ => true
      }
    }
    (potentialPriorRepresented, parent)
  }

  /*
   * This function returns at list of simple elements that are descendents of
   * this term that are not defaultable or OVC. This is a requirement for terms
   * inside a hidden group. Note that there is an exception for choices, in
   * which only a single branch be all defaultable or OVC. If any elements in a
   * hidden group are not defaultable or OVC, then it is an SDE. This function
   * assumes it is only called on elements inside of a hidden group.
   *
   * Note that this currently only requires OVC since default's aren't
   * implemented. This function may need to change when we support defaults.
   */
  lazy val childrenInHiddenGroupNotDefaultableOrOVC: Seq[ElementBase] = {
    // this should only be called on hidden elements
    val isH = isHidden
    Assert.invariant(isH)

    val res = this match {
      case s: SequenceTermBase => {
        s.groupMembers.flatMap { member =>
          val res = member.childrenInHiddenGroupNotDefaultableOrOVC
          res
        }
      }
      case c: ChoiceTermBase => {
        val branches = c.groupMembers.map { _.childrenInHiddenGroupNotDefaultableOrOVC }
        val countFullyDefaultableOrOVCBranches = branches.count { _.length == 0 }
        if (countFullyDefaultableOrOVCBranches == 0) {
          c.SDE("xs:choice inside a hidden group must contain a branch with all children having the dfdl:outputValueCalc property set.")
          // TODO: Diagnostics to display which branches contained non-defaultable elements, and what those elements were
        }
        Nil
      }
      case e: ElementBase if e.isComplexType => {
        e.complexType.group.childrenInHiddenGroupNotDefaultableOrOVC
      }
      case e: ElementBase => {
        if (!e.canBeAbsentFromUnparseInfoset) {
          Seq(e)
        } else {
          Nil
        }
      }
    }
    res
  }

  lazy val couldHaveSuspensions: Boolean = {
    val commonCouldHaveSuspensions =
      !isKnownToBeAligned || // AlignmentFillUnparser
        (if (hasDelimiters) !isDelimiterKnownToBeTextAligned else false) || // MandatoryTextAlignmentUnparser
        needsBitOrderChange // BitOrderChangeUnparser

    this match {
      case eb: ElementBase => {
        val elementCouldHaveSuspensions =
          commonCouldHaveSuspensions ||
            !isKnownToBeTextAligned || // MandatoryTextAlignmentUnparser
            (if (eb.isSimpleType) eb.isOutputValueCalc else false) || // OVCRetryUnparser
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

  final lazy val possibleNextTerms: Seq[Term] = LV('possibleNextTerms) {
    val es = this.nearestEnclosingSequence
    val eus = this.nearestEnclosingUnorderedSequenceBeforeSequence
    val ec = this.nearestEnclosingChoiceBeforeSequence

    val enclosingUnorderedGroup = {
      (ec, eus) match {
        case (None, None) => None
        case (Some(choice), _) => Some(choice)
        case (None, Some(uoSeq)) => Some(uoSeq)
      }
    }
    val listOfNextTerm = (enclosingUnorderedGroup, es) match {
      case (None, None) => Seq.empty
      case (Some(unorderedGroup), _) => {
        // We're in a choice or unordered sequence
        //
        // List must be all of our peers since (as well as our self)
        // we could be followed by any of them plus
        // whatever follows the unordered group.
        val peersCouldBeNext = unorderedGroup.groupMembers

        val termsUntilFirstRequiredTerm = peersCouldBeNext ++ unorderedGroup.possibleNextTerms
        termsUntilFirstRequiredTerm
      }
      case (None, Some(oSeq)) => {
        // We're in an ordered sequence

        val termsUntilFirstRequiredTerm =
          isDeclaredLastInSequence match {
            case true => oSeq.possibleNextTerms
            case false => {

              val members = oSeq.groupMembers

              val selfAndAfter = members.dropWhile(m => m ne this)
              val after = selfAndAfter.drop(1)
              val nextMember = after.headOption

              val nextMembers =
                nextMember match {
                  case Some(e: ElementBase) if e.isOptional => Seq(e) ++ e.possibleNextTerms
                  case Some(e: ElementBase) => Seq(e)
                  case Some(mg: ModelGroup) => Seq(mg)
                  case None => Nil // Assert.impossibleCase
                }
              nextMembers
            }
          }
        termsUntilFirstRequiredTerm
      }
    }
    listOfNextTerm
  }.value

  final def isDeclaredLastInSequence = LV('isDeclaredLastInSequence) {
    val es = nearestEnclosingSequence
    // how do we determine what child node we are? We search.
    // TODO: better structure for O(1) answer to this.
    es match {
      case None => Assert.invariantFailed("We are not in a sequence therefore isDeclaredLastInSequence is an invalid question.")
      case Some(s) => {
        val members = s.groupMembers
        if (members.last eq this) true // we want object identity comparison here, not equality.
        else false
      }
    }
  }.value

  protected def possibleFirstChildTerms: Seq[Term]

  /*
   * Returns list of Elements that could be the first child in the infoset of this model group or element.
   */
  final def possibleFirstChildElementsInInfoset: Seq[ElementBase] = LV('possibleFirstChildElementsInInfoset) {
    val pfct = possibleFirstChildTerms
    val firstChildren = pfct.flatMap {
      case e: ElementBase if e.isHidden => Nil
      case e: ElementBase => Seq(e)
      case s: SequenceTermBase if s.isHidden => Nil
      case mg: ModelGroup => mg.possibleFirstChildElementsInInfoset
    }
    firstChildren.distinct
  }.value

  /*
   * Returns a list of Elements that could follow this Term, including
   * siblings, children of siblings, and siblings of the parent and their children.
   *
   * What stops this is when the end of an enclosing element has to be next.
   */
  final def possibleNextChildElementsInInfoset: Seq[ElementBase] = LV('possibleNextChildElementsInInfoset) {
    val arrayNext = if (isArray) Seq(this.asInstanceOf[ElementBase]) else Nil

    val nextSiblingElements = {
      val poss = possibleNextSiblingTerms
      val res = poss.flatMap {
        possible =>
          possible match {
            case e: ElementBase => Seq(e)
            case mg: ModelGroup => mg.possibleFirstChildElementsInInfoset
          }
      }
      res
    }

    val nextParentElts = nextParentElements
    val res = arrayNext ++ nextSiblingElements ++ nextParentElts
    res
  }.value

  def nextParentElements: Seq[ElementBase]

  protected def couldBeLastElementInModelGroup: Boolean

  /*
   * Returns a list of sibling Terms that could follow this term. This will not
   * return any children of sibling Terms, or any siblings of the parent.
   */
  final def possibleNextSiblingTerms: Seq[Term] = LV('possibleNextSiblingTerms) {
    val et = enclosingTerm
    val listOfNextTerm = et match {
      case None => Nil // root element, has no siblings
      case Some(e: ElementBase) => Nil // complex element, cannot have another model group other than this one
      case Some(c: ChoiceTermBase) => Nil // in choice, no other siblings could come after this one
      case Some(s: SequenceTermBase) if !s.isOrdered => s.groupMembers // unorderd sequence, all siblings (and myself) could be next
      case Some(s: SequenceTermBase) => {
        // in a sequence, the next term could be any later sibling that is not
        // or does not have a required element, up to and including the first
        // term that is/has a required element
        //        def isOutputValueCalc(term: Term) =
        //          term match { case eb: ElementBase if eb.isOutputValueCalc => true; case _ => false }
        val selfAndAllNextSiblings = s.groupMembers.dropWhile(_ != this)
        val allNextSiblings = if (selfAndAllNextSiblings.length > 0) selfAndAllNextSiblings.tail else Nil
        val nextSiblings = allNextSiblings // .dropWhile(isOutputValueCalc(_))
        val (optional, firstRequiredAndLater) = nextSiblings.span {
          case e: ElementBase => e.canBeAbsentFromUnparseInfoset
          case mg: ModelGroup => !mg.mustHaveRequiredElement
        }
        optional ++ firstRequiredAndLater.take(1)
      }
    }
    listOfNextTerm
  }.value

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
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.contentLengthParserReferencedElementInfos) }
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
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.contentLengthUnparserReferencedElementInfos) }
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
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.valueLengthParserReferencedElementInfos) }
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
    val res = realChildren.foldLeft(locRefs) { (s, i) => s.union(i.valueLengthUnparserReferencedElementInfos) }
    res
  }

  private lazy val realChildren: Seq[Term] = {
    this match {
      case mg: ModelGroup => mg.groupMembers.asInstanceOf[Seq[Term]]
      case eb: ElementBase if (eb.isComplexType) => Seq(eb.complexType.group)
      case eb: ElementBase => Seq()
    }
  }

}
