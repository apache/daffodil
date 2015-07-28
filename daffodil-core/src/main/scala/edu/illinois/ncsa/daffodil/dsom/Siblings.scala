package edu.illinois.ncsa.daffodil.dsom

/**
 * The compiler does lots of analysis of terms and the siblings of terms.
 *
 * To get some stronger type-correctness guarantees than just using a nested list, we
 * specifically have bags of things that are ordered, and that represent alternatives.
 */

//case class Alternatives[T](alts: Seq[T]) {
//  override def size = alts.size
//}
//
//object Sibs {
//  def apply[T <: Term](thing: T): Siblings[T] = Siblings[T](Seq(Alternatives[T](Seq(thing))))
//  def apply[T <: Term](things: Seq[T]): Siblings[T] = Siblings[T](things.map { thing => Alternatives[T](Seq(thing)) })
//}
//
//case class Siblings[T <: Term](sibs: Seq[Alternatives[T]]) {
//  def size = sibs.size
//  def isEmpty = sibs.isEmpty
//}

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