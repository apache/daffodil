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

package org.apache.daffodil.infoset

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.MStackOfInt
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.Maybe.Nope

object InfosetWalker {

  /**
   * Create an infoset walker starting with a specified DINode. If the caller
   * starts the InfosetWalker at a specified DINode that is not a DIDocument
   * (which is often the case with the debugger) the InfosetWalker will walk
   * the node and its children, but will not walk any siblings of the node.
   * Regardless whether the DINode is a DIDocument or something else, the
   * InfosetWalker will still always call the start/endDocument functions of
   * the InfosetOutputter.
   *
   * @param root
   *
   *   The DINode to start walking the infoset
   *
   * @param outputter
   *
   *   The InfosetOutputter to output events while walking the infoset
   *
   * @param walkHidden
   *
   *   Whether or not to walk infoset elements that are considered hidden. This
   *   should usually only be set to true while debugging
   *
   * @param ignoreBlocks
   *
   *   Whether or not to ignore blocks when walking the infoset, which are used
   *   to prevent creation of infoset events that might be backtracked. This
   *   should usually only be set to true while debugging
   *
   * @param removeUnneeded
   *
   *   Whether or not to remove infoset nodes once it is determined that they
   *   will no longer be used by Daffodil. This should usually be set to true
   *   except while debugging
   */
  def apply(
    root: DINode,
    outputter: InfosetOutputter,
    walkHidden: Boolean,
    ignoreBlocks: Boolean,
    removeUnneeded: Boolean): InfosetWalker = {

    // Determine the container of the root node and the index in which it
    // appears in that node
    val (startingContainerNode, startingContainerIndex) = root match {
      case d: DIDocument => {
        // We want to start at the zero'th child index of the document
        (d, 0)
      }
      case _ => {
        // This case should only be hit when using the debugger to start
        // walking at a node that isn't the root DIDocument. This gets the
        // container of the root node to start at and finds the index in that
        // container
        (root.containerNode, root.containerNode.contents.indexOf(root))
      }
    }
    new InfosetWalker(
      startingContainerNode,
      startingContainerIndex,
      outputter,
      walkHidden,
      ignoreBlocks,
      removeUnneeded)
  }

}

/**
 * The purpose of this class is to walk Daffodil's internal infoset
 * representation (i.e. DINodes), starting at a specified DINode, and call the
 * appropriate functions on the InfosetOutputter. The InfosetOutputter provided
 * to this class determines how the internal infoset is projectted to the user
 * (e.g. XML, json, SAX events).
 *
 * Calling the walk() method causes the InfosetWalker to begin this process. At
 * any point as determined by the walker, it may pause the walk. Thus, the
 * entire infoset is not guaranteed to have been walked when the walk() method
 * returns. In fact, it is possible that no progress could be made. It is up to
 * the caller to call walk() at appropriate times where it is likely that a
 * walk could make progress.
 *
 * The isFinished() method can be used to determine if the walker walked the
 * entire infoset or not.
 *
 * @param startingContainerNode
 *
 *   The container DINode of the element to start at. This should be either a
 *   DIComplex, DIDocument, or DIArray. DISimple is technically allowed, but
 *   will not create any useful events as it contains no children.
 *
 * @param startingContainerIndex
 *
 *   The child index of the element in the startingContainerNode
 *
 * @param outputter
 *
 *   The InfosetOutputter to output events while walking the infoset
 *
 * @param walkHidden
 *
 *   Whether or not to walk infoset elements that are considered hidden. This
 *   should usually only be set to true while debugging
 *
 * @param ignoreBlocks
 *
 *   Whether or not to ignore blocks when walking the infoset, which are used
 *   to prevent creation of infoset events that might be backtracked. This
 *   should usually only be set to true while debugging
 *
 * @param removeUnneeded
 *
 *   Whether or not to remove infoset nodes once it is determined that they
 *   will no longer be used by Daffodil. This should usually be set to true
 *   except while debugging
 */
class InfosetWalker private (
  startingContainerNode: DINode,
  startingContainerIndex: Int,
  val outputter: InfosetOutputter,
  walkHidden: Boolean,
  ignoreBlocks: Boolean,
  removeUnneeded: Boolean) {

  /**
   * These two pieces of mutable state are all that is needed to keep track of
   * where we are in the infoset. The element that the walker will output an
   * event for when step() is called is referenced by its container DINode
   * (either a DIComplex/DIDocument or DIArray) which is stored in
   * containerNode, and its index within that containing node (which is the
   * value on the top of the containerIndexStack). Once step() creates the
   * appropriate event for the element, it will mutate this state so the next
   * call to step creates events for the next element in the infoset.
   *
   * To step to the next sibling of an element, we only need to increment the
   * index on the top of the stack, since siblings have the same container.
   *
   * To step into a DIComplex/DIArray, we mutate containerNode to be the
   * DIComplex/DIArray we want to step into and push a zero onto the container
   * index stack so that the next element is the zero'th child of that
   * container.
   *
   * To step out of a DIComplex/DIArray, we mutate containerNode to be the
   * container of the containerNode and pop off the top of the stack. We then
   * can perform the logic to step to the next sibling.
   *
   * Special helper functions are created to make the above logic more clear.
   *
   * Note that we initialize the top of the container index stack with one less
   * than the starting container index. This lets the getNextStep function know
   * that we have not yet started the document. Once the document is started,
   * we increment this value on the top of the stack so that the starting index
   * is correct.
   */
  private var containerNode: DINode = startingContainerNode
  private var containerIndexStack: MStackOfInt = {
    val stack = MStackOfInt()
    stack.push(startingContainerIndex - 1)
    stack
  }

  private var finished = false

  /**
   * Determine if the walker has finished walking.
   */
  def isFinished = finished

  /**
   * Take zero or more steps in the infoset. This may or may not walk the
   * entire infoset. If isFinished returns false, walk can be called again to
   * continue attempting to walk the infoset where it left off. Because this is
   * not guaranteed to make any progress, the caller should attempt to only
   * call walk() when infoset state has changed such that progress may be
   * possible.
   *
   * It is an error to call walk() if isFinished returns true
   */
  def walk(): Unit = {
    Assert.usage(!finished)

    var maybeNextStep: Maybe[InfosetWalkerStep] = Nope
    while ({
      maybeNextStep = getNextStep()
      maybeNextStep.isDefined
    }) {
      maybeNextStep.get.step()
    }
  }

  /**
   * Determine the next step to take, if any
   */
  private def getNextStep(): Maybe[InfosetWalkerStep] = {
    if (finished) {
      Nope
    } else if ((containerNode ne startingContainerNode) || containerIndexStack.top == startingContainerIndex) {
      // The containerNode is either some child of the starting node (container
      // node != starting container code) or we are exactly on the starting
      // node (container node == starting container node && top of index stack
      // == starting index). So we can potentially take a normal step.
      //
      // This doesn't necessarily mean we have a step though, since there may
      // be PoU's that could backtrack and so we can't create events yet, or
      // other kinds of blocks could exist. So we need to inspect the infoset
      // state to determine if we can actually create events for the current
      // infoset node and take a step.
      if (ignoreBlocks || canTakeStep()) {
        One(InfosetWalkerStepMove)
      } else {
        Nope
      }
    } else {
      // We only get here if the container node is the starting container node,
      // but the top of the index is not the index of the root node. This means
      // the next step is either a start or end step.
      //
      // If the top of the index stack is less than the starting index, then we
      // have not started the document yet (because we initialize the index
      // stack with one less than the starting index). The next step must be a
      // start step. The InfosetWalkerStepStart is responsible for creating the
      // right event and updating the containerIndexStack to reference the
      // correct starting index.
      //
      // Otherwise the top of the index stack must be greater than the starting
      // index because we have moved passed the starting element index, and
      // thus the next step is an end step. The InfosetWalkerStepEnd is
      // responsible for creating the endDocument event and cleaning up state.
      if (containerIndexStack.top < startingContainerIndex) {
        One(InfosetWalkerStepStart)
      } else {
        One(InfosetWalkerStepEnd)
      }
    }
  }

  private def canTakeStep(): Boolean = {

    if (containerNode.infosetWalkerBlockCount > 0) {
      // This happens in two cases:
      //
      // 1) If we have already walked into a complex type that includes an
      //    unordered sequence. When we start adding the unordered sequence we
      //    increment the infosetWalkerBlockCount of the container because we
      //    can't increment the infosetWalkerBlockCount of the yet to be
      //    created unordered sequence elements. So we take no further steps
      //    until the unordered sequence is finished and the block is removed
      // 2) When we are speculatively parsing elements. When an element may or
      //    may not exist, we set a point of uncertainty before it is created.
      //    Setting this mark increments the block count of the container
      //    element because this optional element does not exist yet.
      //
      // In both cases we cannot take a step until the block is removed
      false

    } else {
      // no blocks on the container, figure out if we can take a step for the
      // element and the child index of this container 

      val children = containerNode.contents
      val childIndex = containerIndexStack.top

      if (childIndex < children.size) {
        // There is a child element at this index. Taking a step would create
        // the events for, and moved passed, this element.
        val elem = children(childIndex)
        if (elem.infosetWalkerBlockCount > 0) {
          // This element has a block associated with it, likely meaning we are
          // speculatively parsing this element and so it may or may not exist.
          // We cannot walk into it until we figure it out and the block is
          // removed.
          false
        } else if (elem.isInstanceOf[DISimple] && !elem.isFinal) {
          // This is a simple element that is not final, i.e. we are still
          // doing some parsing for this simple element. Wait until we finish
          // that parse before stepping into it.
          false
        } else {
          true
        }
      } else {
        // There is not currently a child at this index.
        if (containerNode.isFinal) {
          // This container is final, no more children will be added. So we
          // can make a step that will end this container
          true
        } else {
          // This container is not final, meaning we are potentially waiting
          // for more another child to be added at this index. In this case, we
          // cannot make a step until a child is added or the container is
          // marked as final
          false
        }
      }
    }
  }

  private trait InfosetWalkerStep {
    /**
     * Output events associated with this step kind, and mutate the
     * InfosetWalker state to walk to the next node in the infoset
     */
    def step(): Unit

    final def moveToFirstChild(newContainer: DINode): Unit = {
      containerNode = newContainer
      containerIndexStack.push(0)
    }

    final def moveToContainer(): Unit = {
      containerNode = containerNode.containerNode
      containerIndexStack.pop
    }

    final def moveToNextSibling(): Unit = {
      containerIndexStack.push(containerIndexStack.pop + 1)
    }
  }

  private object InfosetWalkerStepStart extends InfosetWalkerStep {
    /**
     * Start the document. Note that because the top of container index is
     * initialized to one less that the starting index, we also call
     * moveToNextSibling to increment the starting index to the correct
     * position.
     */
    override def step(): Unit = {
      outputter.startDocument()
      moveToNextSibling()
    }
  }

  private object InfosetWalkerStepEnd extends InfosetWalkerStep {
    /**
     * End document and clean up state. Setting finished to true causes
     * the next step to be None, walk() will return, and the caller
     * should not call walk() again because it is finished.
     */
    override def step(): Unit = {
      outputter.endDocument()
      containerNode = null
      containerIndexStack = null
      finished = true
    }
  }

  private object InfosetWalkerStepMove extends InfosetWalkerStep {
    /**
     * Output start/end events for DIComplex/DIArray/DISimple, and mutate state
     * so we are looking at the next node in the infoset.
     */
    def step(): Unit = {
      val children = containerNode.contents
      val childIndex = containerIndexStack.top

      if (childIndex < children.size) {
        // This block means we need to create a start event for the element in
        // the children array at childIndex. We then need to mutate the walker
        // state so the next call to step() is for either the first child of
        // this element or the next sibling.

        children(childIndex) match {
          case s: DISimple => {
            if (!s.isHidden || walkHidden) {
              outputter.startSimple(s)
              outputter.endSimple(s)
            }
            if (removeUnneeded) {
              // now we can remove this simple element to free up memory
              containerNode.freeChildIfNoLongerNeeded(containerIndexStack.top)
            }
            moveToNextSibling()
          }
          case c: DIComplex => {
            if (!c.isHidden || walkHidden) {
              outputter.startComplex(c)
              moveToFirstChild(c)
            } else {
              moveToNextSibling()
            }
          }
          case a: DIArray => {
            if (!a.isHidden || walkHidden) {
              outputter.startArray(a)
              moveToFirstChild(a)
            } else {
              moveToNextSibling()
            }
          }
        }

      } else {
        // This else block means that we incremented the index stack past the
        // number of children in this container (must be a DIComplex/DIDocument
        // or DIArray), which means we have created events for all if its
        // children. So we must now create the appropriate end event for the
        // container and then mutate the state so that we are looking at its
        // next sibling. Note that if there is no next sibling, that will be
        // found the next time step() is called (because we incremented past
        // this element) and we will fall into this block again, call the end
        // function again, and repeat the process.

        // create appropriate end event
        containerNode match {
          case a: DIArray => {
            outputter.endArray(a)
          }
          case c: DIComplex => {
            outputter.endComplex(c)
          }
          case _ => Assert.impossible()
        }

        // we've ended this array/complex associated with the container, so we
        // now want to move to the parent container, potentially free up the
        // memory associated with this container, and then move to the next
        // sibling of this container
        moveToContainer()
        if (removeUnneeded) {
          containerNode.freeChildIfNoLongerNeeded(containerIndexStack.top)
        }
        moveToNextSibling()
      }
    }
  }

}
