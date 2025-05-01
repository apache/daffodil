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

package org.apache.daffodil.runtime1.processors.parsers

import java.lang.{ Boolean => JBoolean, Long => JLong }

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable

import passera.unsigned.ULong

abstract class BinaryBooleanParserBase(
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  lengthUnits: LengthUnits
) extends PrimParser {

  def getBitLength(state: PState): Int

  lazy val toBits = lengthUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ =>
      context.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  override def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (nBits < 1 || nBits > 32) {
      PE(
        start,
        "Number of bits %d out of range for xs:boolean, must be between 1 and 32 bits.",
        nBits
      )
      return
    }

    Assert.invariant(binaryBooleanTrueRep.isEmpty || binaryBooleanTrueRep.getULong >= ULong(0))
    Assert.invariant(binaryBooleanFalseRep >= ULong(0))

    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis)
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
          PE(start, "Unable to parse xs:boolean from binary: %s", sl)
          return
        }
      }

    start.simpleElement.overwriteDataValue(bool)
  }
}

class BinaryBooleanParser(
  override val context: ElementRuntimeData,
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  lengthEv: Evaluatable[JLong],
  lengthUnits: LengthUnits,
  lengthKind: LengthKind
) extends BinaryBooleanParserBase(binaryBooleanTrueRep, binaryBooleanFalseRep, lengthUnits) {

  override def runtimeDependencies = Vector(lengthEv)

  override def getBitLength(state: PState): Int = {
    val nBytesAsJLong = lengthEv.evaluate(state)
    val nBytes = Numbers.asInt(nBytesAsJLong)
    lengthKind match {
      case LengthKind.Implicit => nBytes
      case _ => nBytes * toBits
    }
  }
}

class BinaryBooleanBitLimitLengthParser(
  override val context: ElementRuntimeData,
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  lengthUnits: LengthUnits
) extends BinaryBooleanParserBase(binaryBooleanTrueRep, binaryBooleanFalseRep, lengthUnits)
  with BitLengthFromBitLimitMixin {

  override def runtimeDependencies = Vector()

  override def getBitLength(state: PState): Int = {
    getLengthInBits(state).toInt
  }
}
