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
 * Provides the infoset nodes of a state that unparses a tree build made ahead of it:
 * build already made, attached and finished each node, so this takes each node
 * from the tree, and finishing one only leaves what build has counted for it.
 */
trait PrebuiltNodeProvider { self: InfosetTreeState =>

  protected def treeEvents: TreeEventState

  // Build already made and attached every node. A hidden element has no events,
  // so it is the existing one; any other has an event, which leads to its node.
  override def getElement(
    erd: ElementRuntimeData,
    startEvent: Maybe[InfosetAccessor]
  ): DIElement = {
    if (withinHiddenNest) {
      treeEvents.takeExistingHiddenChild(currentInfosetNode, erd)
    } else {
      Assert.invariant(startEvent.isDefined)
      startEvent.get.info.element
    }
  }

  override def finishElement(cur: DINode): Unit = finishExistingNode(cur)

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
