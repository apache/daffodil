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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.schema.annotation.props.gen._
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.dsom._

/**
 * Runtime valued properties that are enums would all work like ByteOrder here.
 */
class ByteOrderEv(override val expr: CompiledExpression[String], eci: DPathElementCompileInfo)
  extends EvaluatableConvertedExpression[String, ByteOrder](expr, ByteOrder, eci)
  with InfosetCachedEvaluatable[ByteOrder] {
  override def runtimeDependencies = Vector()

}

/**
 * Singleton Ok is for returning from checks and such which will either throw SDE
 * or succeed. It means "check succeeded"
 */
class Ok private () extends Serializable {
  override def toString = "Ok"
}
object Ok extends Ok()

class CheckByteAndBitOrderEv(
  t: DPathCompileInfo,
  bitOrder: BitOrder,
  maybeByteOrderEv: Maybe[ByteOrderEv]
) extends Evaluatable[Ok](t)
  with InfosetCachedEvaluatable[Ok] { // can't use unit here, not <: AnyRef

  override def runtimeDependencies = Vector()

  override final protected def compute(state: ParseOrUnparseState): Ok = {
    t match {
      case eci: DPathElementCompileInfo => {
        if (maybeByteOrderEv.isDefined) {
          val byteOrderEv = maybeByteOrderEv.get
          val byteOrder = byteOrderEv.evaluate(state)
          bitOrder match {
            case BitOrder.MostSignificantBitFirst => // ok
            case BitOrder.LeastSignificantBitFirst =>
              if (byteOrder =:= ByteOrder.BigEndian) {
                t.schemaDefinitionError(
                  "Bit order 'leastSignificantBitFirst' requires byte order 'littleEndian', but byte order was '%s'.",
                  byteOrder
                )
              }
          }
        }
      }
      case _ => // ok. No checks required.
    }
    Ok
  }
}

class CheckBitOrderAndCharsetEv(t: DPathCompileInfo, bitOrder: BitOrder, charsetEv: CharsetEv)
  extends Evaluatable[Ok](t)
  with InfosetCachedEvaluatable[Ok] { // can't use unit here, not <: AnyRef

  override def runtimeDependencies = Vector(charsetEv)

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
    if (
      bitsCharset.mandatoryBitAlignment != 8
      && (bitsCharset.requiredBitOrder !=:= bitOrder)
      && state.bitPos1b % 8 != 1 // real runtime check
      // we check that last because the others might fail at compile time for this Ev
      // which would mean no possible error message at runtime, and
      // therefore faster speed for this Ev.
    ) {
      t.schemaDefinitionError(
        "Encoding '%s' requires bit order '%s', but bit order was '%s'.",
        bitsCharset.name,
        bitsCharset.requiredBitOrder,
        bitOrder
      )
    }
    Ok
  }
}
