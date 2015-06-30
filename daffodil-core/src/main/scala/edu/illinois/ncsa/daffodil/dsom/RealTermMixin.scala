package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * A RealTerm is an element or a sequence or a choice. Group references
 * are excluded as they go away and really have no realization.
 *
 * Things that apply to real terms, but not group references, go in this trait.
 */
trait RealTermMixin { self: Term =>

  final lazy val couldBeNext: Seq[Term] = LV('couldBeNext) {
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
        val peersCouldBeNext = unorderedGroup.groupMembersNoRefs

        // Note: There was a tiny difference here between the couldBeNext of LocalElementBase
        // and the couldBeNext of model group. This difference appears to be that this
        // version is more correct (from LocalElementBase).
        val termsUntilFirstRequiredTerm = peersCouldBeNext ++ unorderedGroup.couldBeNext
        termsUntilFirstRequiredTerm
      }
      case (None, Some(oSeq)) => {
        // We're in an ordered sequence

        val termsUntilFirstRequiredTerm =
          isDeclaredLastInSequence match {
            case true => oSeq.couldBeNext
            case false => {

              val members = oSeq.groupMembersNoRefs

              val nextMember =
                members.dropWhile(m => m != thisTermNoRefs).filterNot(m => m == thisTermNoRefs).headOption

              val nextMembers =
                nextMember match {
                  case Some(e: LocalElementBase) if e.isOptional => Seq(e) ++ e.couldBeNext
                  case Some(e: LocalElementBase) => Seq(e)
                  case Some(gb: GroupBase) => Seq(gb.group)
                  case None => Assert.impossibleCase // Difference: model group has Nil here.
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
        val members = s.groupMembersNoRefs
        if (members.last eq thisTermNoRefs) true // we want object identity comparison here, not equality. 
        else false
      }
    }
  }.value

  protected def couldBeFirstChildTerm: Seq[Term]

  /*
   * Returns list of Elements that could be the first child in the infoset of this model group
   */
  final def couldBeFirstChildElementInInfoset: Seq[ElementBase] = LV('couldBeFirstChildElementInInfoset) {
    val firstChildren = couldBeFirstChildTerm.flatMap {
      case e: ElementBase => Seq(e)
      case mg: ModelGroup => mg.couldBeFirstChildElementInInfoset
    }
    firstChildren
  }.value

  /*
   * Returns a list of Elements that could follow this Term, including
   * siblings, children of siblings, and siblings of the parent and their children.
   */
  final def couldBeNextElementInInfoset: Seq[ElementBase] = LV('couldBeNextElementInInfoset) {
    val arrayNext = if (isArray) Seq(this.asInstanceOf[ElementBase]) else Nil

    val nextSiblingElements = couldBeNextSiblingTerm.flatMap {
      case e: ElementBase => Seq(e)
      case mg: ModelGroup => mg.couldBeFirstChildElementInInfoset
    }

    arrayNext ++ nextSiblingElements ++ nextParentElements
  }.value

  protected def nextParentElements: Seq[ElementBase]

  protected def couldBeLastElementInModelGroup: Boolean

  /*
   * Returns a list of sibling Terms that could follow this term. This will not
   * return any children of sibling Terms, or any siblings of the parent.
   */
  final def couldBeNextSiblingTerm: Seq[Term] = LV('couldBeNextSiblingTerm) {
    val listOfNextTerm = enclosingTerm match {
      case None => Nil // root element, has no siblings
      case Some(e: ElementBase) => Nil // complex element, cannot have another model group other than this one
      case Some(c: Choice) => Nil // in choice, no other siblings could come after this one
      case Some(s: Sequence) if !s.isOrdered => s.groupMembersNoRefs // unorderd sequence, all siblings (and myself) could be next
      case Some(s: Sequence) => {
        // in a sequence, the next term could be any later sibling that is not
        // or does not have a required element, up to and including the first
        // term that is/has a required element
        val nextSiblings = s.groupMembersNoRefs.dropWhile(_ != thisTermNoRefs).tail
        val (optional, firstRequiredAndLater) = nextSiblings.span {
          case e: ElementBase => e.isOptional || (e.isArray && !e.isRequiredArrayElement)
          case mg: ModelGroup => !mg.mustHaveRequiredElement
        }
        optional ++ firstRequiredAndLater.take(1)
      }
    }
    listOfNextTerm
  }.value

}
