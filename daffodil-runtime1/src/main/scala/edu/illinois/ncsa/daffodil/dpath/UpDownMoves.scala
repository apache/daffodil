/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.InfosetNoSuchChildElementException
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData

case object ToRoot extends RecipeOp {
  override def run(dstate: DState) {
    var now = dstate.currentElement
    var parent = now.diParent
    while (parent ne null) {
      now = parent
      parent = now.diParent
    }
    dstate.setCurrentNode(now)
  }
}
case object SelfMove extends RecipeOp {
  override def run(dstate: DState) {
    // do this entirely so it will fail at constant compile time
    // also serves as a sort of assertion check.
    dstate.selfMove()
  }
}

case object UpMove extends RecipeOp {
  override def run(dstate: DState) {
    val now = dstate.currentElement
    val n = {
      Assert.invariant(now.diParent ne null) // UpMove past root. Should never happen since an expression like that won't typecheck statically.
      now.diParent
    }
    dstate.setCurrentNode(n)
  }
}

/**
 * Down to a non-array element. Can be optional or scalar.
 */
case class DownElement(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    // TODO PE ? if doesn't exist should be a processing error.
    // It will throw and so will be a PE, but may be poor diagnostic.
    dstate.setCurrentNode(now.getChild(info).asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(info.name)
  }

}

/**
 * Move down to an occurrence of an array element.
 */
case class DownArrayOccurrence(info: DPathElementCompileInfo, indexRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(indexRecipe) {

  val childSlot = info.slotIndexInParent

  override def run(dstate: DState) {
    val savedCurrentElement = dstate.currentComplex
    indexRecipe.run(dstate)
    val index = dstate.index
    val childArrayElementERD: ElementRuntimeData = savedCurrentElement.erd.childERDs(childSlot)
    val arr = savedCurrentElement.getChildArray(childArrayElementERD)
    val occurrence = arr.getOccurrence(index) // will throw on out of bounds
    dstate.setCurrentNode(occurrence.asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(new scala.xml.Text(info.name) ++ indexRecipe.toXML)
  }

}

/*
 * down to an array object containing all occurrences
 */
case class DownArray(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    val arr = now.getChildArray(info)
    Assert.invariant(arr ne null)
    dstate.setCurrentNode(arr.asInstanceOf[DIArray])
  }

  override def toXML = {
    toXML(info.name)
  }

}

case class DownArrayExists(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    val arr = now.getChildArray(info)

    if ((arr eq null) || arr.length == 0) throw new InfosetNoSuchChildElementException(now, info)
  }

  override def toXML = {
    toXML(info.name)
  }

}
