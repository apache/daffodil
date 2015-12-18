package edu.illinois.ncsa.daffodil.util

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
  override def next() = converter(cursor.advanceMaybe.get).cpy()
  override def peek = converter(cursor.inspectMaybe.get).cpy()
}
