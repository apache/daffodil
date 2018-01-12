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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.equality._

/**
 * Runtime valued properties that are enums would all work like ByteOrder here.
 */
class ByteOrderEv(override val expr: CompiledExpression[String], erd: ElementRuntimeData)
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

class CheckByteAndBitOrderEv(t: TermRuntimeData, bitOrder: BitOrder)
  extends Evaluatable[Ok](t)
  with InfosetCachedEvaluatable[Ok] { // can't use unit here, not <: AnyRef

  override lazy val runtimeDependencies = Nil

  override final protected def compute(state: ParseOrUnparseState): Ok = {
    t match {
      case erd: ElementRuntimeData => {
        if (erd.maybeByteOrderEv.isDefined) {
          val byteOrderEv = erd.maybeByteOrderEv.get
          val byteOrder = byteOrderEv.evaluate(state)
          bitOrder match {
            case BitOrder.MostSignificantBitFirst => // ok
            case BitOrder.LeastSignificantBitFirst =>
              if (byteOrder =:= ByteOrder.BigEndian) {
                t.schemaDefinitionError("Bit order 'leastSignificantBitFirst' requires byte order 'littleEndian', but byte order was '%s'.", byteOrder)
              }
          }
        }
      }
      case _ => // ok. No checks required.
    }
    Ok
  }
}

class CheckBitOrderAndCharsetEv(t: TermRuntimeData, bitOrder: BitOrder, charsetEv: CharsetEv)
  extends Evaluatable[Ok](t)
  with InfosetCachedEvaluatable[Ok] { // can't use unit here, not <: AnyRef

  override lazy val runtimeDependencies = List(charsetEv)

  override final protected def compute(state: ParseOrUnparseState): Ok = {
    val bitsCharset = charsetEv.evaluate(state)
    //
    // If the encoding is byte aligned then bit order doesn't matter
    // but otherwise it does.
    //
    // This is checking for the situation where we are within a byte,
    // and the character encoding uses say, MSBF, but we were left
    // off at a LSBF bit position. Or vice versa.
    //
    if (bitsCharset.mandatoryBitAlignment != 8
      && (bitsCharset.requiredBitOrder !=:= bitOrder)
      && state.bitPos1b % 8 != 1 // real runtime check
      // we check that last because the others might fail at compile time for this Ev
      // which would mean no possible error message at runtime, and
      // therefore faster speed for this Ev.
      ) {
      t.schemaDefinitionError("Encoding '%s' requires bit order '%s', but bit order was '%s'.", bitsCharset.name, bitsCharset.requiredBitOrder, bitOrder)
    }
    Ok
  }
}
