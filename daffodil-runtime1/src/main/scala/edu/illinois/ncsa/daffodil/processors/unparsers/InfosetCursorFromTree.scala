package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.processors.InfosetItem
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.util.CursorImplMixin

/**
 * Iterates an infoset tree, handing out elements one by one in response to pull calls.
 */
private[unparsers] class InfosetCursorFromTree(item: InfosetItem)
  extends InfosetCursor
  with CursorImplMixin[InfosetAccessor] {

  private val nodeStack = new MStack.Of[DINode]
  private val indexStack0b = new MStack.OfInt

  private var visitKind: InfosetEventKind = StartKind

  val node = {
    val res = item match {
      case doc: InfosetDocument => doc.getRootElement()
      case el: InfosetElement => el
    }
    res.asInstanceOf[DINode]
  }

  nodeStack.push(node)
  indexStack0b.push(0)

  /**
   * In-order traversal of the nodes of an Infoset tree.
   *
   * State is kept in a nodeStack, and a corresponding indexStack0b. These
   * are always pushed and popped together - (two non-allocating stacks instead of one stack of 2-tuple)
   *
   * Note how the visitKind flag is used to keep track of order of arrival.
   */
  override def fill: Boolean = {
    if (nodeStack.isEmpty) return false
    val c = nodeStack.top
    visitKind match {
      case StartKind => {
        accessor.kind = visitKind
        accessor.node = c
        visitKind = EndKind
      }
      case EndKind => {
        val nextChildIndex0b = indexStack0b.pop
        indexStack0b.push(nextChildIndex0b + 1)
        if (nextChildIndex0b < c.numChildren) {
          // visit child
          val child = c.filledSlots(nextChildIndex0b)
          accessor.kind = StartKind
          accessor.node = child
          visitKind = EndKind
          nodeStack.push(child)
          indexStack0b.push(0)
        } else {
          // done with this complex node
          visitKind = EndKind
          accessor.kind = visitKind
          accessor.node = c
          nodeStack.pop
          indexStack0b.pop
        }
      }
    }
    true
  }
}
