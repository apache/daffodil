/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.processors.unparsers

import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData

/**
 * How unparsing a tree that build made ahead of it finds the infoset nodes, in place of InfosetFromEvents: build already made, attached
 * and finished each node, so this takes each node from the tree, and
 * finishing one only leaves what build has counted for it.
 */
trait InfosetFromTree { self: InfosetTreeState =>

  protected def treeEvents: TreeEventState

  // Hidden elements have no events, but build already created this one.
  override def getHiddenElement(erd: ElementRuntimeData): DIElement =
    treeEvents.takeExistingHiddenChild(currentInfosetNode, erd)

  // Build already made this element and removed what created its event.
  override def getOvcElement(
    startEvent: InfosetAccessor,
    erd: ElementRuntimeData
  ): DIElement =
    startEvent.info.element

  // Build already attached it.
  override def attachElement(newElem: DIElement): Unit = ()

  override def finishElement(cur: DINode, erd: ElementRuntimeData): Unit =
    finishExistingNode(cur)

  override def finishOvcElement(cur: DINode): Unit = finishExistingNode(cur)

  /**
   * Build attached the node and marked it final, apart from a simple node
   * still waiting for its value, and the event state frees it once its end
   * event is out. Unparsing the node is done, so it leaves build's lead.
   */
  private def finishExistingNode(cur: DINode): Unit = {
    if (cur.isSimple && !cur.isFinal && cur.asSimple.hasValue) {
      cur.setFinal()
    }
    treeEvents.decrementLead()
    if (withinHiddenNest) {
      treeEvents.freeExistingHiddenChild()
    }
  }
}
