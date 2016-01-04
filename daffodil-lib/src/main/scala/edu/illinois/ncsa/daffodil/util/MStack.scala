package edu.illinois.ncsa.daffodil.util

import scala.collection.AbstractIterator
import edu.illinois.ncsa.daffodil.exceptions.Assert
// import edu.illinois.ncsa.daffodil.equality._ // TODO: Scala compiler bug - can't use =#= in this file (scalac 2.11.7) because we get a spurious compile error (unable to find ViewEquality in package equality.)
import scala.collection.AbstractIterable
import Maybe._

object MStack {
  final case class Mark(val v: Int) extends AnyVal
  val nullMark = Mark(0)

  /**
   * Avoid boxing and unboxing of primitive types by using these
   */
  final class OfBoolean extends MStack[Boolean]((n: Int) => new Array[Boolean](n), false)
  final class OfInt extends MStack[Int]((n: Int) => new Array[Int](n), 0)
  final class OfLong extends MStack[Long]((n: Int) => new Array[Long](n), 0L)

  /**
   * Workaround for when we want MStack.Of[Maybe[T]]. Use MStack.OfMaybe[T], and to push
   * and pop call pushMaybe and popMaybe.
   *
   * We need this workaround since Scala won't let us define Iterator[Maybe[T]] due to
   * problems compiling the def next(): Maybe[T] method. It cannot seem to deal
   * with an Iterator with a value class as the result type.
   *
   * So we use an Array[AnyRef] as the representation here, and we
   * convert null to Nope, and an actual object reference to One(x)
   */
  final class OfMaybe[T <: AnyRef] {

    override def toString = delegate.toString

    private val delegate = new Of[T]
    private val nullT = null.asInstanceOf[T]

    @inline final def length = delegate.length

    @inline final def push(m: Maybe[T]) = {
      if (m.isDefined) delegate.push(m.get)
      else delegate.push(nullT)
    }

    @inline final def pop: Maybe[T] = {
      val m = delegate.pop
      if (m eq null) Nope
      else One(m)
    }

    @inline final def top: Maybe[T] = {
      val m = delegate.top
      if (m eq null) Nope
      else One(m)
    }

    @inline final def isEmpty = delegate.isEmpty

    def clear() = delegate.clear()
    def toListMaybe = delegate.toList.map {
      x: AnyRef => Maybe(x) // Scala compiler bug without this cast
    }
  }

  /**
   * Use for objects
   *
   * AnyRef is used here because we really don't need more than one specialized version of this.
   * One generic object version will be sufficient.
   *
   * TODO: Note: Maybe or other Value classes (derived from AnyVal). Currently
   * scala will not let you define an Iterator[Maybe[T]] because of a problem with
   * the def next(): Maybe[T]. It seems to not want to allow this to be a value class.
   * The workaround, which still avoids boxing the Maybe objects, is to use
   * an object reference or null, and call Maybe(thing) explicitly outside the
   * iteration. Maybe(null) is Nope, and Maybe(thing) is One(thing) if thing is not null.
   */
  final class Of[T <: AnyRef] {

    override def toString = delegate.toString

    @inline final def length = delegate.length

    private val delegate = new MStack[AnyRef](
      (n: Int) => new Array[AnyRef](n),
      null.asInstanceOf[T])

    @inline final def mark = delegate.mark
    @inline final def reset(m: MStack.Mark) = delegate.reset(m)

    @inline final def push(t: T) = delegate.push(t)
    @inline final def pop: T = delegate.pop.asInstanceOf[T]
    @inline final def top: T = delegate.top.asInstanceOf[T]
    @inline final def isEmpty = delegate.isEmpty
    def clear() = delegate.clear()
    def toList = delegate.toList

    def iterator = delegate.iterator.asInstanceOf[Iterator[T]]

  }

}

/**
 * This is a specialized mutable stack.
 *
 * We have stacks of objects, but we also have stacks of Int, Long, and Boolean
 * and we don't want boxing (which allocates) and unboxing (which leaves things
 * for the garbage collector to reclaim), when we push and pop Int/Long/Boolean
 * things.
 */
protected class MStack[@specialized T] private[util] (arrayAllocator: (Int) => Array[T], nullValue: T) {

  override def toString: String = {
    val stackContents = table.take(index).reverse.mkString(", ")
    "MStack(top=" + stackContents + ")"
  }

  private var index = 0
  private var table: Array[T] = arrayAllocator(32)
  // private var currentIteratorIndex = -1

  private def growArray(x: Array[T]) = {
    val y = arrayAllocator(math.max(x.length * 2, 1))
    Array.copy(x, 0, y, 0, x.length)
    y
  }

  /**
   * Saves current stack index. Use with reset to restore stack index.
   *
   * This preserves the contents of the stack, to the extent that after calling
   * mark, so long as you don't pop before push, and don't pop more times than push,
   * it will restore the stack to the contents it had.
   */
  @inline final def mark = MStack.Mark(index)

  /**
   *  resets stack top to where it was when mark was called.
   */
  @inline final def reset(m: MStack.Mark) {
    index = m.v
  }

  /** The number of elements in the stack */
  @inline final def length = index

  /**
   * Push an element onto the stack.
   *
   *  @param x The element to push
   */
  @inline final def push(x: T) {
    if (index == table.length) table = growArray(table)
    table(index) = x
    index += 1
  }

  /**
   * Pop the top element off the stack.
   *
   *  @return the element on top of the stack
   */
  @inline final def pop(): T = {
    if (index == 0) Assert.usageError("Stack empty")
    index -= 1
    val x = table(index)
    table(index) = nullValue
    x
  }

  /**
   * View the top element of the stack.
   *
   *  Does not remove the element on the top. If the stack is empty,
   *  an exception is thrown.
   *
   *  @return the element on top of the stack.
   */
  @inline final def top: T = table(index - 1).asInstanceOf[T]

  @inline final def isEmpty: Boolean = index == 0

  def clear() = {
    index = 0
  }

  def toList = iterator.toList

  /**
   * Creates and iterator over the stack in LIFO order.
   *  @return an iterator over the elements of the stack.
   */
  def iterator: Iterator[T] = new Iterator[T] {
    private var currentIndex = index
    private val initialIndex = index

    def hasNext = currentIndex > 0
    def next() = {
      Assert.usage(hasNext)
      Assert.usage(initialIndex <= index) // can't make it smaller than when initialized.
      currentIndex -= 1
      table(currentIndex).asInstanceOf[T]
    }
  }

}
