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

import java.lang.{ Long => JLong }

import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueLong
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.LengthState

import passera.unsigned.ULong

sealed abstract class DFDLLengthFunctionBase(kind: String, recipes: List[CompiledDPath])
  extends FNTwoArgsNodeAndValue(recipes) {

  protected def lengthState(elt: DIElement): LengthState

  protected def getLength(elt: DIElement, units: LengthUnits, dstate: DState): ULong = {

    val len: ULong =
      DState.withRetryIfBlocking(dstate) {
        units match {
          case LengthUnits.Bits => lengthState(elt).lengthInBits
          case LengthUnits.Bytes => lengthState(elt).lengthInBytes
          case LengthUnits.Characters => {
            //
            // TODO: We could warn about taking lengthInCharacters of something
            // that isn't all text, but it's not required that it be purely
            // text. DFDL lets you mix text and binary and then search it for
            // delimiters or run regex patterns to parse it. You have to know what
            // you are doing.
            // Furthermore, in fixed-width encodings, this length can be computed
            // from the length-in-bits by just dividing by a codepoint width
            // code point width.
            //
            val nyi = new IllegalArgumentException(
              "dfdl:%sLength's second argument of 'characters' is not yet supported.".format(
                kind
              )
            )
            elt.erd.SDE(nyi)
            // lengthState(elt).lengthInCharacters
          }
        }
      }
    len
  }

  override def computeValue(
    anyNode: DataValuePrimitive,
    str: DataValuePrimitive,
    dstate: DState
  ): DataValueLong = {

    val elt = anyNode.getAnyRef match {
      case e: DIElement => e
      case _ =>
        throw new IllegalArgumentException(
          "dfdl:%sLength's first argument must be an Infoset Element. Argument was: %s".format(
            kind,
            anyNode
          )
        )
    }

    val units = str.getAnyRef match {
      case s: String => LengthUnits(s, elt.runtimeData)
      case _ =>
        throw new IllegalArgumentException(
          "dfdl:%sLength's second argument must be one of the strings 'bits', 'bytes', or 'characters', but was: %s."
            .format(kind, str)
        )
    }

    val jLen: JLong = getLength(elt, units, dstate).longValue
    jLen
  }
}

case class DFDLContentLength(recipes: List[CompiledDPath])
  extends DFDLLengthFunctionBase("content", recipes) {

  override protected def lengthState(elt: DIElement) = elt.contentLength

}

case class DFDLValueLength(recipes: List[CompiledDPath])
  extends DFDLLengthFunctionBase("value", recipes) {

  override protected def lengthState(elt: DIElement) =
    elt.valueLength

}
