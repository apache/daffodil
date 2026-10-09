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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData

/**
 * How unparsing the events of an inputter makes the infoset: it creates the
 * node of an element that has no event, attaches each new node to its parent,
 * and finishes a node when its end is reached.
 */
trait InfosetFromEvents { self: InfosetTreeState =>

  override def getHiddenElement(erd: ElementRuntimeData): DIElement = {
    // Since we never get events for elements in hidden contexts, their infoset elements
    // will have never been created. This means we need to manually create them
    val hiddenElem = if (erd.isComplexType) {
      new DIComplex(erd)
    } else {
      new DISimple(erd)
    }
    hiddenElem.setHidden()
    hiddenElem
  }

  override def getOvcElement(
    startEvent: InfosetAccessor,
    erd: ElementRuntimeData
  ): DIElement = {
    val e = new DISimple(erd)
    // Remove any state that was set by what created this event. Later
    // code asserts that OVC elements do not have a value
    e.resetValue()
    e
  }

  override def attachElement(newElem: DIElement): Unit = {
    val parentNodeMaybe = currentInfosetNodeMaybe
    if (parentNodeMaybe.isDefined) {
      val parentComplex = parentNodeMaybe.get.asComplex
      Assert.invariant(!parentComplex.isFinal)
      if (parentComplex.isNilled) {
        // cannot add content to a nilled complex element
        UnparseError(
          One(newElem.erd.schemaFileLocation),
          Nope,
          "Nilled complex element %s has content from %s",
          parentComplex.erd.namedQName.toExtendedSyntax,
          newElem.erd.namedQName.toExtendedSyntax
        )
      }

      // We are about to add a child to this complex element. Before we do
      // that, if the last child added to this complex is a DIArray, and this
      // new child isn't part of that array, that implies that the DIArray
      // will have no more children added and should be marked as final, and
      // we can attempt to free that array.
      val lastChildMaybe = parentComplex.maybeLastChild
      if (lastChildMaybe.isDefined) {
        val lastChild = lastChildMaybe.get
        if (lastChild.isArray && (lastChild.erd ne newElem.erd)) {
          lastChild.setFinal()
          freeChildIfNoLongerNeeded(parentComplex, parentComplex.numChildren - 1)
        }
      }

      parentComplex.addChild(newElem, tunable)
    } else {
      // We do not yet have an infoset element (this new element is the
      // root), so add the infoset node to the DIDocument
      documentElement.addChild(newElem, tunable)
    }
  }

  override def finishElement(cur: DINode, erd: ElementRuntimeData): Unit = {
    if (cur.isComplex) {
      // We are ending a complex element. If the last child of this complex
      // is a DIArray, that implies that the array will have no more children
      // and should be marked as isFinal. Normally this happens when we add a
      // new sibling after an array in attachElement, but in this case there
      // is no sibling following the array, so it must be set here.
      val lastChild = cur.maybeLastChild
      if (lastChild.isDefined && lastChild.get.isArray) {
        lastChild.get.setFinal()
        freeChildIfNoLongerNeeded(cur, cur.numChildren - 1)
      }
    }

    // cur is finished, mark it as final and free if possible. Note that we
    // need the container and not the parent of the current element to free
    // it. This way if this element is in an array, we free this element
    // from the array. We also do not set hidden IVC elements as
    // final--although we allow hidden IVC elements when unparsing, they
    // never get a value so we can't set them as final without breaking
    // assertions. Nothing can access hidden IVC elements, so this should
    // not break anything
    if (!withinHiddenNest || erd.isRepresented) {
      cur.setFinal()
    }
    val curContainer = if (cur.erd.isArray) {
      cur.diParent.maybeLastChild.get
    } else {
      cur.diParent
    }
    freeChildIfNoLongerNeeded(curContainer, curContainer.numChildren - 1)
    markDocumentFinalIfRootEnded()
  }

  override def finishOvcElement(cur: DINode): Unit = {
    // ovcElem is finished, free it if possible. OVC elements are not allowed in
    // arrays, so we can directly get the diParent to get the container DINode
    val ovcContainer = cur.diParent
    freeChildIfNoLongerNeeded(ovcContainer, ovcContainer.numChildren - 1)
    markDocumentFinalIfRootEnded()
  }

  protected def markDocumentFinalIfRootEnded(): Unit = {
    if (currentInfosetNodeStack.isEmpty) {
      // If there is nothing else on the infoset stack after popping off the
      // current infoset node, that means we have finished the root element,
      // so mark the DIDocument as final
      val doc = documentElement
      Assert.invariant(!doc.isFinal)
      doc.setFinal()
    }
  }
}
