package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.processors.unparsers.InfosetAugmenter
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.OutputValueCalcAugmenter
import edu.illinois.ncsa.daffodil.processors.unparsers.GeneralAugmenter
import edu.illinois.ncsa.daffodil.processors.unparsers.DefaultableScalarAugmenter

/*
   * There are 3 different entities involved in augmented infosets for unparsing.
   *
   * A - the anchor element
   * B - the insertable element - an element with dfdl:outputValueCalc, or a default value and all the other characteristics
   * that make it something that can be defaulted.
   * C - the indicator element or indicator event - an element start event Start(e: DIElement) - which can be complex or simple type,
   * that is required to be in the infoset event stream, or if there is
   * no such element, the End(c: DIComplex) event for the complex type element that contains B. For any given insertible element B, there must
   * be at least one indicator event (the End of the containing complex type). But there can be any number of such indicator elements as well.
   *
   * The insertable element B can be a child of A (if A has complex type), or it can be a following sibling of A.
   *
   * The indicator is an event that if found, indicates that the insertable element should in fact be inserted 
   * into the augmented infoset. It can be a Start(e: DIElement) for any element known to be 
   * after the insertable element, or it can be an End(c: DIComplex) for the enclosing parent.
   *
   * Note that for a defaultable element if the incoming indicator is the same NQN as the defaultable element, that's not
   * a case for augmenting. It's a case where the element already exists. No augmentation occurs.
   *
   * Every element has the potential to have later siblings that are insertable.
   *
   * For an element of complex type there is also the case of an insertible child element. When an insertible child
   * element is potentially first in the children, or potentially last in the children, then the complex element's
   * child infoset augmenter is used to determine whether to insert the insertible child.
   *
   * If there are no insertible elements as children or later siblings, then the infoset augmenters for that anchor element are Nope.
   */

/**
 * This mixin is only used in ElementBase. It's purpose is just separating out
 * the various attributes having to do with augmenting the infoset during unparsing.
 *
 * While unparsing, we must insert infoset elements for defaultable elements that are not
 * present, and we must insert infoset elements that are computed by dfdl:outputValueCalc expressions.
 */
trait UnparserAugmentedInfosetElementMixin
  extends AnchorAugmentedInfosetElementMixin // from perspective of this element as an anchor
  with InsertableAugmentedInfosetElementMixin // from perspective of this element as an insertable element
  {
  self: ElementBase =>
}

/**
 * An anchor must analyze to determine what incoming events after it are indicators that
 * trigger defaulting or creation of an insertable.
 */
trait AnchorAugmentedInfosetElementMixin { self: ElementBase =>

  /**
   * Computes all the insertables that could possibly be the first event after
   * this anchor.
   */
  private def allFirstInsertables(s: ChoOrd): Seq[ElementBase] = s match {
    case ChoOrd.MT => Nil
    case Leaf(e) =>
      if (isInsertable(e)) Seq(e)
      else Nil
    case Cho(c) => c.flatMap { allFirstInsertables(_) }
    case Ord(s) => allFirstInsertables(s)
  }

  private def allFirstInsertables(s: Seq[ChoOrd]): Seq[ElementBase] = s match {
    case Seq() => Nil
    case Seq(ChoOrd.MT) => Nil
    case Leaf(hd) :: _ if isInsertable(hd) => Seq(hd)
    case Leaf(hd) :: tl if hd.isOptional || (hd.isArray && !hd.isRequiredArrayElement) => allFirstInsertables(tl)
    case Leaf(hd) :: _ => Nil // leaf is a required thing. Stop looking for insertables if we hit this.
    //
    // Q: What if a Cho has a Ord in it that contains no insertable elements at all, nor required elements
    // A: If that's the case, we don't care about that branch w.r.t. augmentation of infoset, so the flatten here
    // will remove it from consideration.
    //
    case Cho(c) :: rest => {
      val firstInsertiblesForChoice = c.map { allFirstInsertables(_) }
      val allHaveInsertible = firstInsertiblesForChoice.forall { fi => !fi.isEmpty }
      val more = if (allHaveInsertible) Nil else allFirstInsertables(rest)
      val res = firstInsertiblesForChoice.flatten ++ more
      res
    }
  }

  /**
   * Computes all the insertables that could possibly be the last event after
   * this anchor.
   *
   * This provides the insertables that should be inserted upon encountering
   * the End(DIElement) for the containing element.
   *
   * TODO: Does this work right? What if the processing of siblings has already
   * inserted the insertable element? That is. c(a, b, d) where d is OVC, and b is required.
   * B will have d as an insertable.
   */
  private def allLastInsertables(s: ChoOrd): Seq[ElementBase] = s match {
    case ChoOrd.MT => Nil
    case Leaf(e) =>
      if (isInsertable(e)) Seq(e)
      else Nil
    case Cho(c) => c.flatMap { allLastInsertables(_) }
    case Ord(s) => allFirstInsertables(s.reverse)
  }

  private[dsom] lazy val possibleFirstSiblingInsertables = allFirstInsertables(followingElementTreeForLaterSiblings)

  private[dsom] lazy val childrenFollowingElementTree = this.elementComplexType.group.followingElementTree

  private[dsom] lazy val possibleFirstChildInsertables = allFirstInsertables(childrenFollowingElementTree)

  private[dsom] lazy val possibleLastChildInsertables = {
    val possiblyLast = allLastInsertables(childrenFollowingElementTree)
    val lastThatAreNotInFirstInsertables = possiblyLast.filterNot {
      possibleFirstChildInsertables.contains(_)
    }
    lastThatAreNotInFirstInsertables
  }

  /**
   * list of all the children that must be inserted when End(DIComplex) is received.
   */
  private[dsom] lazy val lastChildInsertibles = allLastInsertables(childrenFollowingElementTree)

  lazy val maybeChildInfosetAugmenter: Maybe[InfosetAugmenter] = {
    if (isSimpleType)
      Nope
    else if (possibleFirstChildInsertables.isEmpty && possibleLastChildInsertables.isEmpty)
      Nope
    else {
      val childPairs = possibleFirstChildInsertables.flatMap { insertable =>
        val augmenter = insertable.siblingAugmenter
        insertable.siblingIndicators.map { ind => // Note: siblingIndicators: yes. We're looking for its laterSiblings here
          (ind.namedQName, augmenter)
        }
      }
      val anchorName = namedQName
      val endOfParentAugmenterPairs = possibleLastChildInsertables.map { insertable =>
        (anchorName, insertable.childAugmenter)
      }
      //
      // The reverse here is critical. Because in the case of a choice, where there are 
      // two branches with OVC in each. It's really ambiguous, so we want to resolve to always use the
      // first. When converting pairs to a Map, the toMap method seems to let later entries supercede earlier.
      // So we need things where the 'winning' entries are later in the pairs list.
      //
      val augMap = (childPairs ++ endOfParentAugmenterPairs).reverse.toMap
      // 
      // This map now tells us what augment elements are created for given indicator QNames as incoming events
      // and gives us the augmenter to call.
      //
      val augmenter = new GeneralAugmenter(augMap)
      One(augmenter)
    }
  }

  lazy val maybeLaterSiblingInfosetAugmenter: Maybe[InfosetAugmenter] = {
    if (laterSiblings.isEmpty) Nope
    else if (possibleFirstSiblingInsertables.isEmpty) Nope
    else {
      val siblingAugmenterPairs = possibleFirstSiblingInsertables.flatMap { insertable =>
        val augmenter = insertable.siblingAugmenter
        insertable.siblingIndicators.map { ind =>
          (ind.namedQName, augmenter)
        }
      }
      val anchorName = this.namedQName
      val endOfParentAugmenterPairs = possibleFirstSiblingInsertables.map { insertable =>
        (anchorName, insertable.childAugmenter)
      }
      //
      // reverse below is important. We're populating a map, and whatever is added to the map
      // last "wins", and we want siblingAugmenterPairs to "win" over the endOfParentAugmenter pairs.
      //
      val augMap = (siblingAugmenterPairs ++ endOfParentAugmenterPairs).reverse.toMap
      val augmenter = new GeneralAugmenter(augMap)
      One(augmenter)
    }
  }

}

trait InsertableAugmentedInfosetElementMixin { self: ElementBase =>

  private def allPossibleFirstElementsAfterThis(s: ChoOrd): Seq[ElementBase] = s match {
    case ChoOrd.MT => Nil
    case Leaf(e) =>
      if (e.isOutputValueCalc) Nil
      else Seq(e)
    case Cho(c) => c.flatMap { allPossibleFirstElementsAfterThis(_) }
    case Ord(s) => allPossibleFirstElementsAfterThis(s)
  }

  private def allPossibleFirstElementsAfterThis(s: Seq[ChoOrd]): Seq[ElementBase] = s match {
    case Seq() => Nil
    case Seq(ChoOrd.MT) => Nil
    case Leaf(hd) :: rest if hd.isOutputValueCalc => allPossibleFirstElementsAfterThis(rest)
    case Leaf(hd) :: rest if isInsertable(hd) => s.flatMap { allPossibleFirstElementsAfterThis(_) }
    case Leaf(hd) :: _ => Seq(hd)
    case Cho(c) :: rest => {
      val allPossiblesForChoChildren = c.map { allPossibleFirstElementsAfterThis(_) }
      val allChoChildrenHaveARequiredElement = allPossiblesForChoChildren.forall { s => s.find { e => !isInsertable(e) }.isDefined }
      val more =
        if (allChoChildrenHaveARequiredElement) Nil
        else allPossibleFirstElementsAfterThis(rest)
      val res = allPossiblesForChoChildren.flatten ++ more
      res
    }
  }

  private[dsom] lazy val siblingIndicators: Seq[ElementBase] = {
    Assert.invariant(isInsertable(this))
    allPossibleFirstElementsAfterThis(followingElementTreeForLaterSiblings)
  }

  private[dsom] lazy val childAugmenter: InfosetAugmenter = this match {
    case e if e.isOutputValueCalc => new OutputValueCalcAugmenter(e.erd, false)
    //    case e if e.isArray => new DefaultableArrayAugmenter(e.erd, false)
    case e if e.isScalar => new DefaultableScalarAugmenter(e.erd, false)
    case _ => Assert.invariantFailed("only defined for insertable elements")
  }

  private[dsom] lazy val siblingAugmenter: InfosetAugmenter = this match {
    case e if e.isOutputValueCalc => new OutputValueCalcAugmenter(e.erd, true)
    //    case e if e.isArray => new DefaultableArrayAugmenter(e.erd, true) 
    case e if e.isScalar => new DefaultableScalarAugmenter(e.erd, true)
    case _ => Assert.invariantFailed("only defined for insertable elements")
  }

}
