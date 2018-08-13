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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.processors.ElementRuntimeData
import java.lang.{ Long => JLong, Boolean => JBoolean }
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.util.Numbers
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.MaybeULong
import passera.unsigned.ULong

class BinaryBooleanParser(
  val e: ElementRuntimeData,
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits,
  val lengthKind: LengthKind)
  extends PrimParser() {

  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getBitLength(s: ParseOrUnparseState): Int = {
    val nBytesAsJLong = lengthEv.evaluate(s)
    val nBytes = Numbers.asInt(nBytesAsJLong)
    lengthKind match {
      case LengthKind.Implicit => nBytes
      case _ => nBytes * toBits
    }
  }

  override def context = e
  override lazy val runtimeDependencies = Vector(lengthEv)

  override def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (nBits < 1 || nBits > 32) {
      PE(start, "Number of bits %d out of range, must be between 1 and 32 bits.", nBits)
      return
    }

    Assert.invariant(binaryBooleanTrueRep.isEmpty || binaryBooleanTrueRep.getULong >= ULong(0))
    Assert.invariant(binaryBooleanFalseRep >= ULong(0))

    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis.remainingBits)
      return
    }

    val sl = dis.getUnsignedLong(nBits, start)

    val bool: JBoolean =
      if (!binaryBooleanTrueRep.isDefined) {
        if (binaryBooleanFalseRep == sl) false
        else true
      } else {
        if (binaryBooleanTrueRep.getULong == sl) true
        else if (binaryBooleanFalseRep == sl) false
        else {
          PE(start, "Convert to xs:boolean: Cannot parse boolean from '%s'", sl)
          return
        }
      }

    start.simpleElement.overwriteDataValue(bool)
  }
}
