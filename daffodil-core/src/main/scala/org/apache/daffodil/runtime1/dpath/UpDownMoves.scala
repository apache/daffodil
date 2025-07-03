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

package org.apache.daffodil.runtime1.dpath

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.InfosetNoSuchChildElementException

/**
 * Moves to above the root element so that an absolute path
 * that begins with the name of the root element will descend into
 * the root element.
 */
case object ToRoot extends RecipeOp {
  override def run(dstate: DState): Unit = {
    val rootDoc = dstate.currentElement.toRootDoc
    dstate.setCurrentNode(rootDoc)
  }
}
case object SelfMove extends RecipeOp {
  override def run(dstate: DState): Unit = {
    // do this entirely so it will fail at constant compile time
    // also serves as a sort of assertion check.
    dstate.selfMove()
  }
}

case object UpMove extends RecipeOp {
  override def run(dstate: DState): Unit = {
    val now = dstate.currentElement
    val n = now.toParent
    dstate.setCurrentNode(n)
  }
}

case object UpMoveArray extends RecipeOp {
  override def run(dstate: DState): Unit = {
    val now = dstate.currentElement
    Assert.invariant(now.toParent.maybeArray.isDefined)
    val n = now.toParent.maybeArray.get
    dstate.setCurrentNode(n.asInstanceOf[DIArray])
  }
}

/**
 * Down to a non-array element. Can be optional or scalar.
 */
case class DownElement(nqn: NamedQName) extends RecipeOp {

  override def run(dstate: DState): Unit = {
    val now = dstate.currentComplex
    // TODO PE ? if doesn't exist should be a processing error.
    // It will throw and so will be a PE, but may be poor diagnostic.
    val c = dstate.withRetryIfBlocking(now.getChild(nqn, dstate.tunable))
    dstate.setCurrentNode(c.asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(nqn.toQNameString)
  }

}

/**
 * Move down to an occurrence of an array element.
 */
case class DownArrayOccurrence(nqn: NamedQName, indexRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(indexRecipe) {

  val childNamedQName = nqn

  override def run(dstate: DState): Unit = {
    val savedCurrentElement = dstate.currentComplex
    indexRecipe.run(dstate)
    val index = dstate.index
    // restore the saved node since the above .run set it to null. This is
    // necessary since one of the following functions could throw, leaving the
    // current node to null. And future calls depend on a current node to be set
    dstate.setCurrentNode(savedCurrentElement)
    val childArrayElementERD =
      dstate.withRetryIfBlocking(
        savedCurrentElement.getChildArray(nqn, dstate.tunable).asInstanceOf[DIArray].erd
      )
    val arr = dstate.withRetryIfBlocking(
      savedCurrentElement.getChildArray(childArrayElementERD, dstate.tunable)
    )
    val occurrence =
      dstate.withRetryIfBlocking(arr(index)) // will throw on out of bounds
    dstate.setCurrentNode(occurrence.asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(new scala.xml.Text(nqn.toQNameString) ++ indexRecipe.toXML)
  }

}

/*
 * down to an array object containing all occurrences
 */
case class DownArray(nqn: NamedQName) extends RecipeOp {

  override def run(dstate: DState): Unit = {
    val now = dstate.currentComplex
    val arr = dstate.withRetryIfBlocking(now.getChildArray(nqn, dstate.tunable))
    Assert.invariant(arr ne null)
    dstate.setCurrentNode(arr.asInstanceOf[DIArray])
  }

  override def toXML = {
    toXML(nqn.toQNameString)
  }

}

case class DownArrayExists(nqn: NamedQName) extends RecipeOp {

  override def run(dstate: DState): Unit = {
    val now = dstate.currentComplex
    val arr = dstate.withRetryIfBlocking(now.getChildArray(nqn, dstate.tunable))

    if ((arr eq null) || arr.length == 0)
      throw new InfosetNoSuchChildElementException(now, nqn)
  }

  override def toXML = {
    toXML(nqn.toQNameString)
  }

}
