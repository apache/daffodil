package edu.illinois.ncsa.daffodil.dsom

/**
 * Information needed specifically for unparsing.
 */
object UnparserInfo {

  sealed trait InfosetEventBehavior // i.e., relative to the infoset event stream, what behavior does this element have?

  sealed trait Defaultable extends InfosetEventBehavior // may or may not appear in infoset events

  case object ScalarDefaultable extends Defaultable // scalar that may or may not appear in infoset events

  case object ArrayDefaultable extends Defaultable // array element that may or may not appear in infoset events (need array index to determine whether to default it or not)

  case object Computed extends InfosetEventBehavior // Cannot appear in the infoset events - always created

  case object MustExist extends InfosetEventBehavior // corresponding element must appear in the infoset event stream.

  case object Optional extends InfosetEventBehavior // optional element or array with all-optional occurrences

}