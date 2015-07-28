package edu.illinois.ncsa.daffodil.dsom

/**
 * This mixin is only used in Term. It's purpose is just separating out
 * the various attributes having to do with augmenting the infoset during unparsing.
 */
trait UnparserAugmentedInfosetTermMixin { self: Term =>

  protected final def isInsertable(e: ElementBase) = {
    e.isOutputValueCalc ||
      (e.isScalar && e.isDefaultable) // ||
    //      e.isRequiredArrayElement  // TODO: implement defaultable arrays
  }

  /**
   * the elementPossibilityTree is a tree of nodes representing
   * possible elements that can be in the content of this term.
   *
   * If it is a Cho object, then the children
   * are alternatives. If it is a Ord object, then the children
   * are ordered.
   *
   * There is no unnecessary nesting here. Cho never contains Cho,
   * Ord never contains Ord, and there are never any degenerate
   * Cho of just one thing, or Ord of just one thing.
   */
  private[dsom] lazy val elementPossibilityTree: ChoOrd = {
    import ChoOrd._
    this match {
      case e: ElementBase => Leaf(e)
      case s: Sequence if s.isOrdered =>
        mkOrd(s.groupMembersNoRefs.map { _.elementPossibilityTree })
      case m: ModelGroup =>
        mkCho(m.groupMembersNoRefs.map { _.elementPossibilityTree })
    }
  }

  /**
   * Don't combine it if it's an outputValueCalc, or it's defaultable
   * or it's flat out required and non-defaultable (cannot be optional by any means)
   */
  private def dontCombineIt(l: Leaf) = l match {
    case Leaf(e) =>
      e.isOutputValueCalc ||
        (e.isScalar && !e.isDefaultable) || // required scalar
        (e.isScalar && e.isDefaultable) || // defaultable scalar
        (e.isArray && e.isDefaultable && e.isRequiredArrayElement) // defaultable array element
  }

  /**
   * It's a lot easier to express the don't-combine logic, but when we
   * use this, it's easier to think in terms of permission to combine it.
   * So we invert the boolean sense here.
   */
  private def combineIt(l: Leaf) = !dontCombineIt(l)

  protected final lazy val followingElementTreeForLaterSiblings = {
    import ChoOrd._
    val ls = laterSiblingsWithinEnclosingElement
    val laterSiblingFETs = ls.map { _.followingElementTree }
    val ord = mkOrd(laterSiblingFETs)
    val res = groupAdjacentSatisfying(ord, combineIt _)
    res
  }

  /**
   * Tree that indicates what this term contains when
   * viewed as contributing following elements.
   */
  lazy val followingElementTree: ChoOrd = {
    import ChoOrd._
    this match {
      case e: ElementBase => e.elementPossibilityTree
      case s: Sequence if s.isOrdered =>
        groupAdjacentSatisfying(s.elementPossibilityTree, combineIt _)
      case m: ModelGroup =>
        // unordered sequences or choices
        mkCho(m.groupMembersNoRefs.map { _.followingElementTree })
    }
  }

}