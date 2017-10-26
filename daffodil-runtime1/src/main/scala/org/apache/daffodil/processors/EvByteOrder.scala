/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors

import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.dsom._
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.equality._
import org.apache.daffodil.io.NonByteSizeCharset

/**
 * Runtime valued properties that are enums would all work like ByteOrder here.
 */
class ByteOrderEv(expr: CompiledExpression[String], erd: ElementRuntimeData)
  extends EvaluatableConvertedExpression[String, ByteOrder](
    expr,
    ByteOrder,
    erd)
  with InfosetCachedEvaluatable[ByteOrder] {
  override lazy val runtimeDependencies = Nil

}

/**
 * Singleton Ok is for returning from checks and such which will either throw SDE
 * or succeed. It means "check succeeded"
 */
class Ok private () extends Serializable {
  override def toString = "Ok"
}
object Ok extends Ok()

class CheckByteAndBitOrderEv(t: TermRuntimeData, bitOrder: BitOrder, maybeByteOrder: Maybe[ByteOrderEv])
  extends Evaluatable[Ok](t)
  with InfosetCachedEvaluatable[Ok] { // can't use unit here, not <: AnyRef

  override lazy val runtimeDependencies = maybeByteOrder.toList

  final protected def compute(state: ParseOrUnparseState): Ok = {
    if (maybeByteOrder.isEmpty) return Ok
    val byteOrderEv = maybeByteOrder.get
    val byteOrder = byteOrderEv.evaluate(state)
    bitOrder match {
      case BitOrder.MostSignificantBitFirst => // ok
      case BitOrder.LeastSignificantBitFirst =>
        if (byteOrder =:= ByteOrder.BigEndian) {
          t.schemaDefinitionError("Bit order 'leastSignificantBitFirst' requires byte order 'littleEndian', but byte order was '%s'.", byteOrder)
        }
    }

    Ok
  }
}

class CheckBitOrderAndCharsetEv(t: TermRuntimeData, bitOrder: BitOrder, charsetEv: CharsetEv)
  extends Evaluatable[Ok](t)
  with InfosetCachedEvaluatable[Ok] { // can't use unit here, not <: AnyRef

  override lazy val runtimeDependencies = List(charsetEv)

  final protected def compute(state: ParseOrUnparseState): Ok = {
    val dfdlCS = charsetEv.evaluate(state)
    dfdlCS.charset match {
      case nbsc: NonByteSizeCharset =>
        if (nbsc.requiredBitOrder !=:= bitOrder) {
          t.schemaDefinitionError("Encoding '%s' requires bit order '%s', but bit order was '%s'.", dfdlCS.charsetName, nbsc.requiredBitOrder, bitOrder)
        }
      case _ => // do nothing
    }

    Ok
  }
}
