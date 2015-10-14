package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.collection.mutable

trait IteratorWithPeek[+T] extends Iterator[T] {
  def peek: T
}

/**
 * Stateful, not thread safe.
 */
class IteratorWithPeekImpl[T](originalIterator: Iterator[T], addedItems: mutable.ArrayBuffer[T]) extends IteratorWithPeek[T] {

  def peek = {
    Assert.usage(hasNext)
    addedItems(0)
  }

  def next = {
    Assert.usage(hasNext)
    val result = addedItems.head
    addedItems.remove(0)
    result
  }

  /**
   * Postcondition is that if hasNext is true, addedItems contains 1 or more items.
   */
  def hasNext = {
    if (!addedItems.isEmpty) true
    else if (originalIterator.hasNext) {
      addedItems += originalIterator.next()
      true
    } else false
  }
}