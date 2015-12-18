package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.util.CursorImplMixin

/**
 * Iterates an infoset tree, handing out elements one by one in response to pull calls.
 *
 * Assumes that arrays have already been recognized and turned into DIArray nodes.
 */

class InfosetSourceFromTree(doc: InfosetDocument)
  extends InfosetSource
  with CursorImplMixin[InfosetEvent] {

  private val nodeStack = new MStack.Of[DINode]
  private val indexStack0b = new MStack.OfInt

  private var visitKind: InfosetEventKind = StartKind

  nodeStack.push(doc.getRootElement().asInstanceOf[DINode])
  indexStack0b.push(0)

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
