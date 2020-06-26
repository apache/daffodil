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

package org.apache.daffodil.dpath

import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.InfosetCommon
import org.apache.daffodil.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.parsers.ParseError
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One

case class DFDLXTrace(recipe: CompiledDPath, msg: String)
  extends RecipeOpWithSubRecipes(recipe) {

  private def asXMLString(ie: InfosetCommon) = {
    val bos = new java.io.ByteArrayOutputStream()
    val xml = new XMLTextInfosetOutputter(bos, true)
    ie.visit(xml, false)
    xml.endDocument() // causes the outputter to flush to the stream
    bos.toString("UTF-8")
  }

  override def run(dstate: DState): Unit = {
    recipe.run(dstate)
    val nodeString: String = dstate.currentNode match {
      case _: DISimple | null => {
        // if there is no current node (null case) then there must be a
        // current value.
        val v = dstate.currentValue
        dstate.setCurrentValue(v)
        v.toString()
      }
      case other: InfosetCommon => "\n" + asXMLString(other)
    }
    System.err.println("trace " + msg + ":" + nodeString)
  }

  // This is toXML for the case class object, not the infoset node it is
  // dealing with.
  override def toXML = toXML(recipe.toXML)

}

case object DAFError extends RecipeOp {

  override def run(dstate: DState): Unit = {
    val maybeSFL =
      if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
      else Nope
    dstate.mode match {
      case UnparserNonBlocking | UnparserBlocking =>
        UnparseError(maybeSFL, dstate.contextLocation, "The error function was called.")
      case _: ParserMode => {
        val fe = new FNErrorFunctionException(maybeSFL, dstate.contextLocation, "The error function was called.")
        throw fe
      }
    }
  }
}

case class DFDLXLookAhead(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes) {

  def computeValue(arg1: DataValuePrimitive, arg2: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    val offset = arg1.getLong
    val lBitSize = arg2.getLong

    /*
     * Since dfdlx:lookAhead is defined to take unsigned arguements, the DPath interperater
     * will error out on the cast if a negative arguement is supplied, so we do not need to SDE here.
     */

    Assert.invariant(offset >= 0)
    Assert.invariant(lBitSize >= 0)

    val totalLookahead = offset + lBitSize
    val maxLookahead = dstate.tunable.maxLookaheadFunctionBits
    if (totalLookahead > maxLookahead) {
      dstate.SDE("Look-ahead distance of %s bits exceeds implementation defined limit of %s bits", totalLookahead, maxLookahead)
    }
    //Safe since we guard on totalLookahead
    val bitSize = lBitSize.toInt

    if (!dstate.parseOrUnparseState.isDefined) {
      Assert.invariant(dstate.isCompile)
      /*
        * This is an expected code path.
        * Throwing an exception is how we indicated that this expression
        * cannot be reduced to a constant at compile time.
        */
      throw new IllegalStateException("No input stream at compile time")
    }
    if (dstate.parseOrUnparseState.get.isInstanceOf[PState]) {
      val pstate = dstate.parseOrUnparseState.get.asInstanceOf[PState]
      val dis = pstate.dataInputStream
      if (!dis.isDefinedForLength(totalLookahead)) {
        val maybeSFL =
          if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
          else Nope
        throw new ParseError(maybeSFL, dstate.contextLocation, Nope,
          One("Insufficient bits available to satisfy dfdlx:lookAhead(%s,%s)."),
          offset, bitSize, totalLookahead)
      }
      val mark = dis.markPos
      dis.skip(offset, pstate)
      val ans: DataValuePrimitive = if (bitSize > 63) {
        dis.getUnsignedBigInt(bitSize, pstate)
      } else if (bitSize == 0) {
        JBigInt.ZERO
      } else {
        JBigInt.valueOf(dis.getUnsignedLong(bitSize, pstate).longValue)
      }
      dis.resetPos(mark)
      ans
    } else {
      Assert.invariant(dstate.parseOrUnparseState.get.isInstanceOf[UState])
      dstate.SDE("Cannot call dfdlx:lookAhead() during unparse")
    }
  }
}
