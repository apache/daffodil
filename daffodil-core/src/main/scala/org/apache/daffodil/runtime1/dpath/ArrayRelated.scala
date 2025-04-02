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

import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.runtime1.infoset.InfosetNoInfosetException

case class FNCount(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe)
  with ExistsKind {

  override def run(dstate: DState): Unit = {
    val res = exists(recipe, dstate)
    if (res) {
      if (dstate.currentNode.isArray) {
        dstate.mode match {
          case UnparserBlocking | UnparserNonBlocking => {
            // we are unparsing, so asking for an array count is asking for the
            // final count, not however many are in there at this point
            // so we have to know if the array is closed or not.
            dstate.setCurrentValue(dstate.finalArrayLength)
          }
          case _: ParserMode => {
            dstate.setCurrentValue(dstate.arrayLength)
          }
        }
      } else {
        // optional element is known to exist
        dstate.setCurrentValue(1)
      }
    } else {
      dstate.setCurrentValue(0)
    }
  }
}

/**
 * Returns \$arg if it contains exactly one item. Otherwise, raises an error
 */
case object FNExactlyOne extends RecipeOp {
  override def run(dstate: DState): Unit = {

    // This was commented out in 6c9612e9f711beb1d8e4239ef9a473eb9c64a32f, which as the commit message:
    //
    //  Implements fn:exactly-one function, but comments out the functionality as the function
    //  is useless until query-style path expressions are allowed/implemented. Yields a subsetError
    //  until such time.
    //
    //  // Original implementation with boolean return-type
    //  //    val bool = dstate.arrayLength == 1
    //  //    dstate.setCurrentValue(bool)
    //
    //  // New implementation as stated in tickets DFDL-1085/1087
    //  val hasExactlyOne = dstate.arrayLength == 1
    //  if (hasExactlyOne) {
    //    val array = dstate.currentArray
    //    val item = array.getOccurrence(1).asInstanceOf[DINode]
    //
    //    dstate.setCurrentNode(item)
    //  }
    //  else { throw new Exception("fn:exactly-one called with a sequence containing zero or more than one item.") }

    // The reason queries are needed is that without them, well, the answer to this has to be true, as
    // there are no functions which return zero nor more than one node.
    dstate.SDE("Subset - fn:exactly-one is not implemented by Daffodil.")
  }
}

case object DFDLOccursIndex extends RecipeOp {
  override def run(dstate: DState): Unit = {
    if (dstate.isCompile) {
      throw new InfosetNoInfosetException(Nope)
    }
    dstate.setCurrentValue(dstate.occursIndex)
  }
}
