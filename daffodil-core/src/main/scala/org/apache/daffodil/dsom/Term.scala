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
import org.apache.daffodil.util.ListUtils
import org.apache.daffodil.processors.unparsers.NeverZeroLengthDetector
import org.apache.daffodil.processors.unparsers.PossiblyZeroArrayOccurrencesDetector
import org.apache.daffodil.processors.unparsers.PossiblyZeroLengthModelGroupDetector
import org.apache.daffodil.processors.unparsers.HexBinaryZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NillableStringZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NillableZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NillableHexBinaryZeroLengthDetector
import org.apache.daffodil.processors.unparsers.StringZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NotRepresentedZeroLengthDetector
import org.apache.daffodil.processors.unparsers.ZeroLengthDetector
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.infoset.PartialNextElementResolver

/**
 * Mixin for objects that are shared, but have consistency checks to be run
 * that are based on the concrete Term objects they are associated with.
 *
 * E.g., DFDL statements may have checks that need to know the encoding
 * (if it is known at compile time). We call this on each statement to enable
 * the checking code to be expressed on that statement where it is relevant,
 * but have it be callable from the concrete Term once it is created.
 *
 * This is a way to avoid use of backpointers from shared objects to every
 * thing referencing them.
 */
trait HasTermCheck {

  /**
   * Perform checking of an object against the supplied Term arg.
   */
  final def checkTerm(term: Term): Unit = {
    //
    // This public method calling a protected method lets us play tricks
    // in the future to avoid repeated check calls by memoizing the
    // results.
    //
    check(term)
  }

  /**
   * Override to perform necessary checks that require information about the
   * concrete Term.
   *
   * This avoids the need for the checking code to have a backpointer to the
   * Term.
   */
  protected def check(term: Term): Unit = {
    // by default this does nothing.
  }
}

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
  with ResolvesScopedProperties
  with ResolvesDFDLStatementMixin
  with TermRuntimeValuedPropertiesMixin
  with TermGrammarMixin
  with DelimitedRuntimeValuedPropertiesMixin
  with InitiatedTerminatedMixin
  with TermEncodingMixin
  with EscapeSchemeRefMixin {

  requiredEvaluations(annotationObjs)
  requiredEvaluations(nonDefaultPropertySources)
  requiredEvaluations(defaultPropertySources)
  requiredEvaluations(termChecks)

  private lazy val termChecks = {
    statements.foreach { _.checkTerm(this) }
  }

  /**
   * Abbreviation analogous to trd, tci is the compile-time counterpart.
   */
  final def tci = dpathCompileInfo

  /**
   * Used to recursively go through Terms and look for DFDL properties that
   * have not been accessed and record it as a warning. This function uses the
   * property cache state to determine which properties have been access, so
   * this function must only be called after all property accesses are complete
   * (e.g. schema compilation has finished) to ensure there are no false
   * positives.
   */
  final lazy val checkUnusedProperties: Unit = {
    // Get the properties defined on this term and what it refers to
    val localProps = formatAnnotation.justThisOneProperties
    val refProps = optReferredToComponent.map { _.formatAnnotation.justThisOneProperties }.getOrElse(Map.empty)

    // If a term references a global simple type, we need to inspect the
    // propCache of the simple type in addition to this terms propCache. This
    // is because some property lookup results are cached on the global simple
    // type, like in the case of type calc properties
    val optSimpleTypeCached = optReferredToComponent.collect { case gstd: GlobalSimpleTypeDef => gstd.propCache }
    val usedProperties = propCache ++ optSimpleTypeCached.getOrElse(Map.empty)

    localProps.foreach {
      case (prop, (value, _)) =>
        if (!usedProperties.contains(prop)) {
          SDW(WarnID.IgnoreDFDLProperty, "DFDL property was ignored: %s=\"%s\"", prop, value)
        }
    }

    refProps.foreach {
      case (prop, (value, _)) =>
        if (!usedProperties.contains(prop)) {
          optReferredToComponent.get.SDW(WarnID.IgnoreDFDLProperty, "DFDL property was ignored: %s=\"%s\"", prop, value)
        }
    }

    termChildren.foreach { _.checkUnusedProperties }
  }

  def position: Int

  def optIgnoreCase: Option[YesNo] = {
    val ic = findPropertyOption("ignoreCase")
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
   * Consistent with this is Required meaning must occur but for any
   * reason. So all the occurrences of an array that has fixed number of
   * occurrences are required, and some of the occurrences of an array
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
   * An array can have more than 1 occurrence.
   *
   * An optional element (minOccurs=0, maxOccurs=1) is an array only
   * if occursCountKind is parsed, because then the max/min are ignored.
   */
  def isArray: Boolean

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
    //
    // We want to recurse outward, and don't care about these implied sequence terms
    // that choices wrap around branch elements.
    //
    case Some(cs: ChoiceBranchImpliedSequence) => enclosingTerm.get.nearestEnclosingSequence
    case Some(s: SequenceTermBase) => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingSequence
  }

  final lazy val nearestEnclosingChoiceBeforeSequence: Option[ChoiceTermBase] = enclosingTerm match {
    case None => None
    //
    // We want to recurse outward, and don't care about these implied sequence terms
    // that choices wrap around branch elements.
    //
    case Some(cs: ChoiceBranchImpliedSequence) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
    case Some(s: SequenceTermBase) => None
    case Some(c: ChoiceTermBase) => Some(c)
    case Some(_) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
  }

  final lazy val nearestEnclosingUnorderedSequence: Option[SequenceTermBase] = enclosingTerm match {
    case None => None
    case Some(s: SequenceTermBase) if !s.isOrdered => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  final lazy val isInUnorderedSequence: Boolean = !nearestEnclosingUnorderedSequence.isEmpty

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

  final lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] = {
    optLexicalParent.flatMap { lexicalParent =>
      val res = lexicalParent match {
        case c: ChoiceTermBase => Some(c)
        //
        // skip past the implied sequence that is wrapped around choice branches
        // to the actual choice
        //
        case c: ChoiceBranchImpliedSequence => c.immediatelyEnclosingModelGroup
        case s: SequenceTermBase => Some(s)
        case d: SchemaDocument => {
          // we must be the Root elementRef or a quasi node
          Assert.invariant(this.isInstanceOf[Root] || this.isInstanceOf[QuasiElementDeclBase])
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
        case qe: QuasiElementDeclBase => {
          //
          // If your lexical parent is a Quasi-element, then your model group is
          // the one surrounding the quasi-element.
          //
          qe.immediatelyEnclosingModelGroup
        }
        case _ => Assert.invariantFailed("immediatelyEnclosingModelGroup called on " + this + " with lexical parent " + lexicalParent)
      }
      res
    }
  }

  /**
   * One-based position in the nearest enclosing sequence.
   * Follows backpointers from group defs to group refs until it
   * finds a sequence.
   */
  final lazy val positionInNearestEnclosingSequence: Int = {
    // FIXME:Classic example of a method that creates a pointless
    // need for the backpointers to parent that make structure
    // sharing of the DSOM objects impossible. This should be a value
    // passed down to a SequenceChild constructor, and the algorithms
    // that use this should be on SequenceTermBase or child classes thereof.
    val optET = enclosingTerm
    val optNES = nearestEnclosingSequence
    val res =
      (optET, optNES) match {
        case (Some(et), Some(nes)) if (et == nes) =>
          position
        case _ => {
          if (this.isInstanceOf[Root]) 1
          else {
            optET match {
              case Some(term: Term) => term.positionInNearestEnclosingSequence
              case x => Assert.invariantFailed("For " + this + " unable to compute position in nearest enclosing sequence. The enclosingComponent was " + x)
            }
          }
        }
      }
    res
  }

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

  final lazy val hasLaterRequiredSiblings = laterSiblings.exists(_.hasStaticallyRequiredOccurrencesInDataRepresentation)
  final lazy val hasPriorRequiredSiblings = priorSiblings.exists(_.hasStaticallyRequiredOccurrencesInDataRepresentation)

  /**
   * Does this term have always have statically required instances in the data stream.
   *
   * This excludes elements that have no representation e.g., elements with dfdl:inputValueCalc.
   *
   * Terms that are optional either via element having zero occurrences, or via a choice branch
   * fail this test.
   */
  def hasStaticallyRequiredOccurrencesInDataRepresentation: Boolean

  /**
   * True if the term has some syntax itself or recursively within itself that
   * must appear in the data stream.
   *
   * False only if the term has possibly no representation whatsoever in the
   * data stream.
   */
  def hasKnownRequiredSyntax: Boolean

  /**
   * Can have a varying number of occurrences.
   *
   * Overridden for elements. See [[ParticleMixin.isVariableOccurrences]]
   */
  def isVariableOccurrences: Boolean = false

  /**
   * True when a term's immediately enclosing model group is a Sequence.
   */
  final lazy val isSequenceChild: Boolean =
    immediatelyEnclosingModelGroup.map { _.isInstanceOf[SequenceTermBase] }.getOrElse(false)

  /**
   * The concept of potentially trailing is defined in the DFDL specification.
   *
   * This concept applies to terms that are direct children of a sequence only.
   *
   * It is true for terms that may be absent from the representation, but furthermore, may be last
   * in a sequence, so that the notion of whether they are trailing, and so their separator may not be
   * present, is a relevant issue.
   *
   * If an element is an array, and has some required instances, then it is not potentially trailing, as some
   * instances will have to appear, with separators.
   *
   * This concept applies only to elements and model groups that have representation in the data stream.
   *
   * Previously there was a misguided notion that since only DFDL elements can have minOccurs/maxOccurs
   * that this notion of potentially trailing didn't apply to model groups. (Sequences and Choices, the other
   * kind of Term). But this is not the case.
   *
   * A sequence/choice which has no framing, and whose content doesn't exist - no child elements, any contained
   * model groups recursively with no framing and no content - such a model group effectively "dissapears" from
   * the data stream, and in some cases need not have a separator.
   *
   * This is computed by way of couldBePotentiallyTrailing. This value means that the term, in isolation, looking only
   * at its own characteristics, disregarding its following siblings in any given sequence, has the characteristics
   * of being potentially trailing.
   *
   * Then that is combined with information about following siblings in a sequence to determine if a given term, that
   * is a child of a sequence, is in fact potentially trailing within that sequence.
   *
   * These two concepts are mutually recursive, since a sequence that is entirely composed of potentially trailing children
   * satisfies couldBePotentialyTrailing in whatever sequence encloses it.
   */
  final lazy val isPotentiallyTrailing: Boolean = {
    val thisCouldBe = couldBePotentiallyTrailing
    lazy val laterSibilingsAre = laterSiblings.forall { _.isPotentiallyTrailing }
    val res = thisCouldBe && laterSibilingsAre
    res
  }

  final lazy val couldBePotentiallyTrailing: Boolean = {
    import SeparatorSuppressionPolicy._
    this match {
      case e: ElementBase => {
        lazy val allowsZeroOccurs = e.minOccurs == 0
        lazy val minOccursNotZeroButDeclaredLast = !e.isScalar && e.minOccurs > 0 && e.isLastDeclaredRepresentedInSequence
        lazy val hasAllowedOCK = (e.occursCountKind eq OccursCountKind.Implicit) ||
          (e.occursCountKind eq OccursCountKind.Parsed)
        lazy val hasAllowedLengthKind = e.lengthKind eq LengthKind.Delimited
        lazy val hasNoDiscriminators = !statements.exists { s => s.isInstanceOf[DFDLDiscriminator] }
        val res =
          isRepresented &&
            (allowsZeroOccurs ||
              minOccursNotZeroButDeclaredLast) &&
              hasAllowedOCK &&
              hasAllowedLengthKind &&
              hasNoDiscriminators
        res
      }
      case m: ModelGroup => {
        lazy val seqIsNotSSPNever = m match {
          case s: SequenceTermBase =>
            s.hasSeparator &&
              (s.separatorSuppressionPolicy match {
                case TrailingEmpty => true
                case TrailingEmptyStrict => true
                case AnyEmpty => true
                case Never => false
              })
          case _ => true
        }
        lazy val hasNoStatements = statements.length == 0
        lazy val recursivelyOk =
          m.representedMembers.forall { m =>
            m.couldBePotentiallyTrailing
          }
        val res =
          isRepresented &&
            !m.hasFraming &&
            seqIsNotSSPNever &&
            hasNoStatements &&
            recursivelyOk
        res
      }
    }
  }
  /**
   * Returns a tuple, where the first item in the tuple is the list of sibling
   * terms that could appear before this. The second item in the tuple is a
   * One(enclosingParent) if all prior siblings are optional or this element has no prior siblings
   */
  lazy val potentialPriorTerms: (Seq[Term], Option[Term]) = LV('potentialPriorTerms) {
    val et = enclosingTerm
    val (potentialPrior, optEnclosingParent) = et match {
      case None => (Seq(), None)
      case Some(eb: ElementBase) => (Seq(), Some(eb))
      case Some(ch: ChoiceTermBase) => (Seq(), Some(ch))
      case Some(sq: SequenceTermBase) if !sq.isOrdered => {
        (sq.groupMembers, Some(sq))
      }
      case Some(sq: SequenceTermBase) if sq.isOrdered => {
        val previousTerms = sq.groupMembers.takeWhile { _ != this }
        if (previousTerms.isEmpty) {
          // first child of seq, the seq is the only previous term
          (Seq(), Some(sq))
        } else {
          val firstNonOptional = previousTerms.reverse.find {
            _ match {
              case eb: ElementBase if !eb.isRequiredStreamingUnparserEvent || !eb.isRepresented => false
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
    (potentialPriorRepresented, optEnclosingParent)
  }.value

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
          isLastDeclaredRepresentedInSequence match {
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

  /**
   * True if this term is the last one in the enclosing sequence that is represented
   * in the data stream. That is, it is not an element with dfdl:inputValueCalc.
   *
   * This means whether the enclosing sequence's separator (if one is defined) is
   * relevant.
   */
  final lazy val isLastDeclaredRepresentedInSequence = {
    val res = laterSiblings.forall(!_.isRepresented)
    res
  }

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
   * siblings, children of siblings, and siblings of the enclosingParent and their children.
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
   * return any children of sibling Terms, or any siblings of the enclosingParent.
   */
  final def possibleNextSiblingTerms: Seq[Term] = LV('possibleNextSiblingTerms) {
    enclosingTerms.flatMap { et =>
      val listOfNextTerm = et match {
        case e: ElementBase => Nil // complex element, cannot have another model group other than this one
        case c: ChoiceTermBase => Nil // in choice, no other siblings could come after this one
        case s: SequenceTermBase if !s.isOrdered => s.groupMembers // unorderd sequence, all siblings (and myself) could be next
        case s: SequenceTermBase => {
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
    }
  }.value

  final protected lazy val realChildren: Seq[Term] = {
    this match {
      case mg: ModelGroup => mg.groupMembers.asInstanceOf[Seq[Term]]
      case eb: ElementBase if (eb.isComplexType) => Seq(eb.complexType.group)
      case eb: ElementBase => Seq()
    }
  }

}
