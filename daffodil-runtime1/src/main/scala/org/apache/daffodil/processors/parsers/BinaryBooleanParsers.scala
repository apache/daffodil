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

import java.lang.{ Long => JLong, Boolean => JBoolean }

import passera.unsigned.ULong

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Numbers

abstract class BinaryBooleanParserBase(
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  lengthUnits: LengthUnits)
  extends PrimParser {

  def getBitLength(state: PState): Int

  lazy val toBits = lengthUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => context.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  override def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (nBits < 1 || nBits > 32) {
      PE(start, "Number of bits %d out of range for xs:boolean, must be between 1 and 32 bits.", nBits)
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
  lengthKind: LengthKind)
  extends BinaryBooleanParserBase(binaryBooleanTrueRep, binaryBooleanFalseRep, lengthUnits) {

  override lazy val runtimeDependencies = Vector(lengthEv)

  override def getBitLength(state: PState): Int = {
    val nBytesAsJLong = lengthEv.evaluate(state)
    val nBytes = Numbers.asInt(nBytesAsJLong)
    lengthKind match {
      case LengthKind.Implicit => nBytes
      case _ => nBytes * toBits
    }
  }
}


class BinaryBooleanPrefixedLengthParser(
  override val context: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends BinaryBooleanParserBase(binaryBooleanTrueRep, binaryBooleanFalseRep, lengthUnits)
  with PrefixedLengthParserMixin {
  
  override def childProcessors: Vector[Processor] = Vector(prefixedLengthParser)
  override val runtimeDependencies = Vector()

  override def getBitLength(state: PState): Int = {
    getPrefixedLengthInBits(state).toInt
  }
}
