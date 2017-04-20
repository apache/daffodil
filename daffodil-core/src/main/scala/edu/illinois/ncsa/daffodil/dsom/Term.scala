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
import scala.xml.Node
import scala.xml._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.grammar.TermGrammarMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import java.lang.{ Integer => JInt }
import edu.illinois.ncsa.daffodil.schema.annotation.props.Found
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.NilKind

/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

// A term is content of a group
abstract class Term(xmlArg: Node, parentArg: SchemaComponent, val position: Int)
  extends AnnotatedSchemaComponent(xmlArg, parentArg)
  with TermRuntimeValuedPropertiesMixin
  with TermGrammarMixin
  with DelimitedRuntimeValuedPropertiesMixin
  with InitiatedTerminatedMixin
  with TermEncodingMixin {

  def optIgnoreCase: Option[YesNo] = {
    val ic = cachePropertyOption("ignoreCase")
    ic match {
      case Found(value, location, _, _) => Some(YesNo(value, location))
      case _ => None
    }
  }

  override final def term = this

  def isOptional: Boolean
  def isRequired: Boolean

  def termRuntimeData: TermRuntimeData

  def elementChildren: Seq[ElementBase]

  override lazy val dpathCompileInfo =
    new DPathCompileInfo(
      enclosingComponent.map { _.dpathCompileInfo },
      variableMap,
      namespaces,
      path,
      schemaFileLocation)

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
   */
  def termChildren: Seq[Term]

  final val tID = UUID.randomUUID()

  // Scala coding style note: This style of passing a constructor arg that is named fooArg,
  // and then having an explicit val/lazy val which has the 'real' name is
  // highly recommended. Lots of time wasted because a val constructor parameter can be
  // accidently hidden if a derived class uses the same name as one of its own parameters.
  // These errors just seem easier to deal with if you use the fooArg style.

  lazy val someEnclosingComponent = enclosingComponent.getOrElse(Assert.invariantFailed("All terms except a root element have an enclosing component."))

  lazy val referredToComponent = this // override in ElementRef and GroupRef

  lazy val isRepresented = true // overridden by elements, which might have inputValueCalc turning this off

  def isScalar = true // override in local elements

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
  final lazy val nearestEnclosingSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingSequence
  }

  final lazy val nearestEnclosingChoiceBeforeSequence: Option[Choice] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) => None
    case Some(c: Choice) => Some(c)
    case Some(_) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
  }

  final lazy val nearestEnclosingUnorderedSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) if !s.isOrdered => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  final lazy val nearestEnclosingUnorderedSequenceBeforeSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) if !s.isOrdered => Some(s)
    case Some(s: Sequence) => None
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  final lazy val inChoiceBeforeNearestEnclosingSequence: Boolean = enclosingTerm match {
    case None => false
    case Some(s: Sequence) => false
    case Some(c: Choice) => true
    case Some(_) => enclosingTerm.get.inChoiceBeforeNearestEnclosingSequence
  }

  final lazy val nearestEnclosingElement: Option[ElementBase] = enclosingTerm match {
    case None => None
    case Some(eb: ElementBase) => Some(eb)
    case Some(_) => enclosingTerm.get.nearestEnclosingElement
  }

  final lazy val nearestEnclosingElementNotRef: Option[ElementBase] = nearestEnclosingElement match {
    case None => None
    case Some(er: ElementRef) => er.nearestEnclosingElement // can't be an element ref again
    case x => x
  }

  protected final def thisTermNoRefs: Term = LV('thisTermNoRefs) {
    val es = nearestEnclosingSequence

    val thisTerm = this match {
      case eRef: ElementRef => eRef.referencedElement
      // case gd: GlobalGroupDef => gd.thisTermNoRefs // TODO: scala 2.10 compiler says this line is impossible.
      case gb: GroupBase if gb.enclosingTerm.isDefined => {
        // We're a group.  We need to determine what we're enclosed by.
        gb.enclosingTerm.get match {
          case encGRef: GroupRef => {
            // We're enclosed by a GroupRef.  We need to retrieve
            // what encloses that GroupRef

            val res = encGRef.enclosingTerm match {
              case None => encGRef.group
              case Some(encTerm) => encTerm.thisTermNoRefs
            }
            //encGRef.thisTerm
            res
          }
          case encGB: GroupBase if es.isDefined && encGB == es.get => {
            // We're an immediate child of the nearestEnclosingSequence
            // therefore we just return our self as the Term
            this
          }
          case e: LocalElementBase => e // Immediate enclosed by LocalElementBase, return it.
          case _ => gb.group
        }
      }
      case gb: GroupBase => gb.group
      case x => x
    }
    thisTerm
  }.value

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
      case c: Choice => Some(c)
      case s: Sequence => Some(s)
      case d: SchemaDocument => {
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no enclosing model group at all
              None
            }
            case Some(er) => er.immediatelyEnclosingModelGroup
          }
        }
      }
      case gdd: GlobalGroupDef => gdd.groupRef.immediatelyEnclosingModelGroup
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
      else {
        enclosingComponent match {
          case Some(term: Term) => term.positionInNearestEnclosingSequence
          case Some(ct: ComplexTypeBase) => {
            val ctElem = ct.element
            val ctPos = ctElem.positionInNearestEnclosingSequence
            ctPos
          }
          case Some(ggd: GlobalGroupDef) => ggd.groupRef.positionInNearestEnclosingSequence
          case _ => Assert.invariantFailed("unable to compute position in nearest enclosing sequence")
        }
      }
    res
  }

  final lazy val isDirectChildOfSequence = parent.isInstanceOf[Sequence]

  import edu.illinois.ncsa.daffodil.util.ListUtils

  final lazy val allSiblings: Seq[Term] = {
    val res = nearestEnclosingSequence.map { enc =>
      val allSiblings = enc.groupMembers.map { _.referredToComponent }
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


  // Returns a tuple, where the first item in the tuple is the list of sibling
  // terms that could appear before this. The second item in the tuple is a
  // One(parent) if all siblings are optional or this element has no prior siblings
  lazy val potentialPriorTerms: (Seq[Term], Option[Term]) = {
    val (potentialPrior, parent) = enclosingTerm match {
      case None => (Seq(), None)
      case Some(eb: ElementBase) => (Seq(), Some(eb))
      case Some(ch: Choice) => (Seq(), Some(ch))
      case Some(sq: Sequence) if !sq.isOrdered => (sq.groupMembersNoRefs, Some(sq))
      case Some(sq: Sequence) if sq.isOrdered => {
        val previousTerms = sq.groupMembersNoRefs.takeWhile { _ != this }
        if (previousTerms.isEmpty) {
          // first child of seq, the seq is the only previous term
          (Seq(), Some(sq))
        } else {
          val firstNonOptional = previousTerms.reverse.find { _ match {
            case eb: ElementBase if !eb.isRequired || !eb.isRepresented => false
            case _ => true
          }}
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
    Assert.invariant(this.isHidden)

    val res = this match {
      case s: Sequence => {
        s.groupMembersNoRefs.flatMap { _.childrenInHiddenGroupNotDefaultableOrOVC }
      }
      case c: Choice => {
        val branches = c.groupMembersNoRefs.map { _.childrenInHiddenGroupNotDefaultableOrOVC }
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
      (if (hasDelimiters) !isDelimiterKnownToBeTextAligned else false) ||  // MandatoryTextAlignmentUnparser
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
          mg.groupMembersNoRefs.exists { _.couldHaveSuspensions }

        modelGroupCouldHaveSuspensions
      }
    }
  }

}
