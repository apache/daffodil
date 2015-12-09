package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.util.Maybe._

trait IteratorWithPeek[+T] extends Iterator[T] {
  def peek: T
}

/**
 * Stateful, not thread safe.
 */
class IteratorWithPeekImpl[T <: AnyRef](originalIterator: Iterator[T]) extends IteratorWithPeek[T] {

  private var peekedItem: Maybe[T] = Nope

  def peek = {
    if (peekedItem.isDefined) {
      val result = peekedItem.get
      result
    } else {
      val result = originalIterator.next()
      peekedItem = One(result)
      result
    }
  }

  def next = {
    if (peekedItem.isDefined) {
      val result = peekedItem.get
      peekedItem = Nope
      result
    } else {
      val result = originalIterator.next()
      result
    }
  }

  /**
   * Postcondition is that if hasNext is true, addedItems contains 1 or more items.
   */
  def hasNext = {
    if (peekedItem.isDefined) true
    else {
      val res = originalIterator.hasNext
      res
    }
  }
}
