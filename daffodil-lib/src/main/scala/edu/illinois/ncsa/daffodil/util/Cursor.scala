package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Cursor pattern is a performance hack to avoid allocation of objects
 * purely due to an API constraint.
 *
 * Cursor[A] is an alternative to Iterator[T]. In Iterator[T] the next() method
 * (and peek() also) must construct an object of type T in order to return it.
 *
 * Cursor avoids this, by instead modifying an "accessor object" to contain the
 * next content (or peeked content).
 *
 * So you advance the cursor, access data using the accessor, and no allocation
 * is required by the API.
 *
 * The danger is that accessors are stateful, and must be per-thread.
 *
 * Contrast this with a callback-style API: the distinct fields of the accessor
 * would be passed as arguments to the callback method, so as to avoid
 * having to allocate an object of type T, but as there is no state object
 * being modified, a callback mechanism is always thread-safe.
 *
 * But the pattern of control it imposes is very troublesome. A cursor, like
 * an Iterator, is a "pull" style interface. It just doesn't pull new
 * objects, rather a pull operation causes a pre-existing state object to
 * be updated.
 *
 * So the accessor state objects must be thread-local, or part of some per-thread
 * state block.
 */

trait HasCpy[+ItemType] {
  def cpy(): ItemType
}

trait Accessor[ItemType] extends AnyRef with HasCpy[ItemType] {

  // TODO: Performance - Get rid of cpy and assignFrom. They are legacy of old copying
  // and allocating way of doing things.
  def cpy(): ItemType
  def assignFrom(other: ItemType): Unit
}

trait Cursor[AccessorType <: Accessor[AccessorType]] {
  def advanceAccessor: AccessorType
  def inspectAccessor: AccessorType

  /**
   * Analogous to Iterator.next, except the accessor provides
   * access to the data, the returned boolean indicates whether
   * that happened successfully or there was no more data.
   */
  def advance: Boolean

  /**
   * Peek-like operation. Not called peek to avoid
   * confusion with iterators that have peek methods.
   *
   * The inspectAccessor provides access to the data, the returned
   * boolean indicates whether that happened successfully or there
   * was no more data.
   */
  def inspect: Boolean

}

trait CursorImplMixin[AccessorType <: Accessor[AccessorType]] { self: Cursor[AccessorType] =>

  private trait OpKind
  private case object Advance extends OpKind
  private case object Inspect extends OpKind
  private case object Unsuccessful extends OpKind
  private var priorOpKind: OpKind = Advance

  /**
   * Implement to fill in the accessor defined by the private var
   * `accessor`
   */
  protected def fill: Boolean

  protected final var accessor: AccessorType = null.asInstanceOf[AccessorType]

  private var isFilled = false

  final override def advance: Boolean = {
    accessor = advanceAccessor
    val res = priorOpKind match {
      case Advance => doAdvance(false)
      case Inspect => {
        // prior operation was inspect!
        priorOpKind = Advance
        advanceAccessor.assignFrom(inspectAccessor)
        isFilled = false
        true
      }
      case Unsuccessful => return false
    }
    // successful
    Assert.invariant(isFilled == false)
    res
  }

  final override def inspect: Boolean = {
    accessor = inspectAccessor
    val res = priorOpKind match {
      case Advance => {
        priorOpKind = Inspect
        doAdvance(true)
      }
      case Inspect => true // inspect again does nothing.
      case Unsuccessful => return false
    }
    // successful
    Assert.invariant(isFilled == true || !res)
    res
  }

  private def doAdvance(isFilledValue: Boolean) = {
    Assert.invariant(isFilled == false)
    val res = fill
    if (!res) priorOpKind = Unsuccessful
    isFilled = if (res) isFilledValue else false
    res
  }
}

/**
 * Many tests are written using an Iterator pattern.
 *
 * Create an iterator (with peek), or a stream, given a Cursor.
 *
 * This copies the accessors - so that the result is truly safe, and
 * as would normally be expected it is a separate object.
 *
 * This defeats the purpose of cursors though, which is to populate
 * accessors, not copy them.
 */

class IteratorFromCursor[ItemType <: HasCpy[ItemType], AccessorType <: Accessor[AccessorType]](
  cursor: Cursor[AccessorType],
  converter: AccessorType => ItemType)
  extends IteratorWithPeek[ItemType] {

  override def hasNext = cursor.inspect
  override def next() = { Assert.invariant(cursor.advance); converter(cursor.advanceAccessor.cpy()) }
  override def peek = { Assert.invariant(cursor.inspect); converter(cursor.inspectAccessor.cpy()) }
}
