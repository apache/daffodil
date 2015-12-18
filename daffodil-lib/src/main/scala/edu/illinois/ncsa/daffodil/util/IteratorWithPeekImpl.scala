package edu.illinois.ncsa.daffodil.util

trait IteratorWithPeek[+T] extends Iterator[T] {
  def peek: T
}
