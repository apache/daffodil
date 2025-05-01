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

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.MStackOfInt

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
   * @param releaseUnneededInfoset
   *
   *   Whether or not to release infoset nodes once it is determined that they
   *   will no longer be used by Daffodil. This should usually be set to true
   *   except while debugging
   *
   * @param walkSkipMin
   *
   *   The minimum number of walk() calls to skip before actually trying to
   *   walk the infoset. A value of zero disables skipping.
   *
   * @param walkSkipMax
   *
   *   When a walk() call fails to make any progress, it assumes we are blocked
   *   (e.g. due to an unresolved point of uncertainty) and increases the
   *   number of walk() calls to skip before trying again. This defines the
   *   maximum number of skipped calls, even as that number increases.
   */
  def apply(
    root: DIElement,
    outputter: InfosetOutputter,
    walkHidden: Boolean,
    ignoreBlocks: Boolean,
    releaseUnneededInfoset: Boolean,
    walkSkipMin: Int = 32,
    walkSkipMax: Int = 2048
  ): InfosetWalker = {

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
        val container: DINode =
          if (root.maybeArray.isDefined) root.maybeArray.get.asInstanceOf[DINode]
          else root.parent.asInstanceOf[DINode]
        (container, container.indexOf(root))
      }
    }
    new InfosetWalker(
      startingContainerNode,
      startingContainerIndex,
      outputter,
      walkHidden,
      ignoreBlocks,
      releaseUnneededInfoset,
      walkSkipMin,
      walkSkipMax
    )
  }

}

/**
 * The purpose of this class is to walk Daffodil's internal infoset
 * representation (i.e. DINodes), starting at a specified DINode, and call the
 * appropriate functions on the InfosetOutputter. The InfosetOutputter provided
 * to this class determines how the internal infoset is projected to the user
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
 * @param releaseUnneededInfoset
 *
 *   Whether or not to remove infoset nodes once it is determined that they
 *   will no longer be used by Daffodil. This should usually be set to true
 *   except while debugging
 *
 * @param walkSkipMin
 *
 *   The minimum number of walk() calls to skip before actually trying to
 *   remove unneeded infoset elements.
 *
 * @param walkSkipMax
 *
 *   When a walk() call fails to remove any infoset elements, it assumes we
 *   being blocked for removal (e.g. due to an unresolved point of uncertainty)
 *   and increases the number of walk() calls to skip before trying again. This
 *   defines the maximum number of skiped calls, even as this number increases.
 */
class InfosetWalker private (
  startingContainerNode: DINode,
  startingContainerIndex: Int,
  val outputter: InfosetOutputter,
  walkHidden: Boolean,
  ignoreBlocks: Boolean,
  releaseUnneededInfoset: Boolean,
  walkSkipMin: Int,
  walkSkipMax: Int
) {

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
  private var containerNodeStack: MStackOf[DINode] = {
    val stack = new MStackOf[DINode]()
    stack.push(startingContainerNode)
    stack
  }
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
   * The following variables are used to determine when to skip the walk()
   * logic. The idea here is that calls to walk() result in a decrease in
   * performance in some situations, especially if we can't take steps due to
   * points of uncertainty or infoset blocks. In such cases, we perform a lot
   * of logic without actually making any progress. And the callers don't know
   * if they can take a step or not since that logic is controlled by this
   * walker.
   *
   * So what we do is we first skip walkSkipMin number of calls to walk(). Once
   * we have skipped that many walk()'s, only then do we actually try to take a
   * step. If that step fails, that means there is still some block, so we
   * double the number skips and return. We continue this doubling up until
   * walkSkipMax. However, if we are able to successfully make a step, it means
   * any points of uncertainty have been resolved, and so we drop the number of
   * steps back down to the min value so that we try taking steps and stream
   * events more frequently.
   */
  private var walkSkipSize = walkSkipMin
  private var walkSkipRemaining = walkSkipSize

  /**
   * Take zero or more steps in the infoset. This may or may not walk the
   * entire infoset. If isFinished returns false, walk can be called again to
   * continue attempting to walk the infoset where it left off. Because this is
   * not guaranteed to make any progress, the caller should attempt to only
   * call walk() when infoset state has changed such that progress may be
   * possible.
   *
   * It is an error to call walk() if isFinished returns true
   *
   * For performance reasons, sometimes a call to walk will intentionally not
   * try to take any steps even if it is possible. This lets us essentially
   * batch infoset events and improve performance. If this is the last time
   * walk() will be called, the lastWalk parameter should be set to true, which
   * will cause walk() to not skip any steps.
   */
  def walk(lastWalk: Boolean = false): Unit = {
    Assert.usage(!finished)

    if (walkSkipRemaining > 0 && !lastWalk) {
      // do not attempt to take a step
      walkSkipRemaining -= 1
    } else {
      // no more skips or this is the last walk, so try to take a step
      var canTakeAnotherStep = maybeDoStep()

      walkSkipSize = if (canTakeAnotherStep) {
        // we've skipped walkSkipSize calls to walk() and we were able to
        // make progress taking a step. Let's set the skip size to skip min
        // to ensure we try to take steps in future calls to walk() more
        // frequently
        walkSkipMin
      } else {
        // we've skipped skip size calls to walk() and still failed to take
        // a single step. Most likely we have an early unresolved point of
        // uncertainty, so double the walkSkipSize to give more time for that
        // PoU to get resolved before actually trying to take more steps
        Math.min(walkSkipSize << 2, walkSkipMax)
      }

      // set the remaining number of skips to the adjusted size so the next
      // time walk is called we start skipping again
      walkSkipRemaining = walkSkipSize

      // continue taking steps as far as we can
      while (canTakeAnotherStep) {
        canTakeAnotherStep = maybeDoStep()
      }

    }
  }

  /**
   * Take a step if possible. Returns true if it is possible another step could
   * be take, false if there was a block or something that would prevent
   * another step from being taken.
   */
  private def maybeDoStep(): Boolean = {
    val containerNode = containerNodeStack.top
    val containerIndex = containerIndexStack.top

    if (
      (containerNode ne startingContainerNode) || (containerIndex == startingContainerIndex)
    ) {
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
      if (ignoreBlocks || canTakeStep(containerNode, containerIndex)) {
        infosetWalkerStepMove(containerNode, containerIndex)
        // took a step, more steps are alloed
        true
      } else {
        // a block prevented us from taking a step, no more steps can be taken
        // during this walk
        false
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
      if (containerIndex < startingContainerIndex) {
        infosetWalkerStepStart()
        // took the start document step, more steps are allowed
        true
      } else {
        infosetWalkerStepEnd()
        // we successfully took a step, but there are no more steps to take
        // after this
        false
      }
    }
  }

  private def canTakeStep(containerNode: DINode, containerIndex: Int): Boolean = {
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
      // element at the child index of this container

      if (containerIndex < containerNode.numChildren) {
        // There is a child element at this index. Taking a step would create
        // the events for, and moved passed, this element.
        val elem = containerNode.child(containerIndex)
        if (elem.infosetWalkerBlockCount > 0) {
          // This element has a block associated with it, likely meaning we are
          // speculatively parsing this element and so it may or may not exist.
          // We cannot walk into it until we figure it out and the block is
          // removed.
          false
        } else if (elem.isSimple && !elem.isFinal) {
          // This is a simple element that is not final, i.e. we are still
          // doing some parsing for this simple element. Wait until we finish
          // that parse before stepping into it.
          false
        } else if (elem.isComplex && elem.numChildren == 0 && !elem.isFinal) {
          // This is a complex element that has no children and is not final.
          // This means we don't yet know if it's just an empty complex element
          // or if it's a complex element that is going to be nilled. We'll
          // know for sure once it has children or is made final, but for now,
          // do not step into it.
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

  @inline
  private def moveToFirstChild(newContainer: DINode): Unit = {
    containerNodeStack.push(newContainer)
    containerIndexStack.push(0)
  }

  @inline
  private def moveToContainer(): Unit = {
    containerNodeStack.pop
    containerIndexStack.pop()
  }

  @inline
  private def moveToNextSibling(): Unit = {
    val top = containerIndexStack.top
    containerIndexStack.setTop(top + 1)
  }

  private def doOutputter(outputterFunc: => Unit, desc: String, context: ThrowsSDE): Unit = {
    try {
      outputterFunc
    } catch {
      case e: Exception => {
        // FIXME: DAFFODIL-2884 This escalates a parser data exception to an SDE
        //  Which breaks if string-as-xml encounters a string that is malformed XML.
        //  We get the error thrown by the xml parser here outside of parsing, which is
        //  too late.
        val cause = e.getCause
        val msg = if (cause == null) e.toString else cause.toString
        context.SDE("Failed to %s: %s", desc, msg)
      }
    }
  }

  /**
   * Start the document. Note that because the top of container index is
   * initialized to one less that the starting index, we also call
   * moveToNextSibling to increment the starting index to the correct
   * position.
   */
  private def infosetWalkerStepStart(): Unit = {
    doOutputter(outputter.startDocument(), "start infoset document", startingContainerNode.erd)
    moveToNextSibling()
  }

  /**
   * End document and clean up state. Setting finished to true causes
   * the next step to be None, walk() will return, and the caller
   * should not call walk() again because it is finished.
   */
  private def infosetWalkerStepEnd(): Unit = {
    doOutputter(outputter.endDocument(), "end infoset document", startingContainerNode.erd)
    containerNodeStack = null
    containerIndexStack = null
    finished = true
  }

  /**
   * Output start/end events for DIComplex/DIArray/DISimple, and mutate state
   * so we are looking at the next node in the infoset.
   */
  private def infosetWalkerStepMove(containerNode: DINode, containerIndex: Int): Unit = {
    if (containerIndex < containerNode.numChildren) {
      // This block means we need to create a start event for the element
      // at containerIndex. Once we create that event we
      // need to mutate the state of the InfosetWalker so that the next time we
      // take a step we are looking at the next element. If this is a complex
      // type, that next element is the first child. If this is a simple type,
      // that next element is the next sibling of this element. We will mutate
      // the state accordingly.

      val child = containerNode.child(containerIndex)

      if (child.isSimple) {
        if (!child.isHidden || walkHidden) {
          val simple = child.asInstanceOf[DISimple]
          doOutputter(outputter.startSimple(simple), "start infoset simple element", simple.erd)
          doOutputter(outputter.endSimple(simple), "end infoset simple element", simple.erd)
        }
        // now we can remove this simple element to free up memory
        containerNode.freeChildIfNoLongerNeeded(containerIndex, releaseUnneededInfoset)
        moveToNextSibling()
      } else {
        // must be complex or array, exact same logic for both
        if (!child.isHidden || walkHidden) {
          if (child.isComplex) {
            val complex = child.asInstanceOf[DIComplex]
            doOutputter(
              outputter.startComplex(complex),
              "start infoset complex element",
              complex.erd
            )
          } else {
            val array = child.asInstanceOf[DIArray]
            doOutputter(outputter.startArray(array), "start infoset array", array.erd)
          }
          moveToFirstChild(child)
        } else {
          moveToNextSibling()
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
      if (containerNode.isComplex) {
        val complex = containerNode.asInstanceOf[DIComplex]
        doOutputter(outputter.endComplex(complex), "end infoset complex element", complex.erd)
      } else {
        val array = containerNode.asInstanceOf[DIArray]
        doOutputter(outputter.endArray(array), "end infoset array", array.erd)
      }

      // we've ended this array/complex associated with the container, so we
      // now want to move to the parent container, potentially free up the
      // memory associated with this container, and then move to the next
      // sibling of this container
      moveToContainer()
      containerNodeStack.top.freeChildIfNoLongerNeeded(
        containerIndexStack.top,
        releaseUnneededInfoset
      )
      moveToNextSibling()
    }
  }

}
