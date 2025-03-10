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

package org.apache.daffodil.unparsers.runtime1

import java.lang.{ Long => JLong }

import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.unparsers._

import passera.unsigned.ULong

abstract class BinaryBooleanUnparserBase(
  override val context: ElementRuntimeData,
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  lengthUnits: LengthUnits
) extends PrimUnparser {

  def getBitLength(s: ParseOrUnparseState): Int

  lazy val toBits = lengthUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ =>
      context.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def unparse(state: UState): Unit = {
    val nBits = getBitLength(state)
    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.getBoolean
    val dos = state.getDataOutputStream

    if (nBits < 1 || nBits > 32) {
      UnparseError(
        One(state.schemaFileLocation),
        One(state.currentLocation),
        "Number of bits %d out of range, must be between 1 and 32 bits.",
        nBits
      )
    }

    Assert.invariant(binaryBooleanTrueRep.isEmpty || binaryBooleanTrueRep.getULong >= ULong(0))
    Assert.invariant(binaryBooleanFalseRep >= ULong(0))

    val res =
      if (value) {
        val trueRep =
          if (binaryBooleanTrueRep.isDefined) binaryBooleanTrueRep.getULong
          else ~binaryBooleanFalseRep
        putNumber(dos, trueRep, nBits, state)
      } else {
        putNumber(dos, binaryBooleanFalseRep, nBits, state)
      }

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(
        One(state.schemaFileLocation),
        One(state.currentLocation),
        "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        context.dpathElementCompileInfo.namedQName.toPrettyString,
        nBits,
        dos.maybeRelBitLimit0b.get
      )
    }
  }

  protected def putNumber(
    dos: DataOutputStream,
    value: ULong,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    dos.putULong(value, nBits, finfo)
  }

}

class BinaryBooleanUnparser(
  e: ElementRuntimeData,
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits,
  val lengthKind: LengthKind
) extends BinaryBooleanUnparserBase(
    e,
    binaryBooleanTrueRep,
    binaryBooleanFalseRep,
    lengthUnits
  ) {

  override def runtimeDependencies = Vector(lengthEv)

  override def getBitLength(s: ParseOrUnparseState): Int = {
    val nBytesAsJLong = lengthEv.evaluate(s)
    val nBytes = Numbers.asInt(nBytesAsJLong)
    lengthKind match {
      case LengthKind.Implicit => nBytes
      case _ => nBytes * toBits
    }
  }
}

class BinaryBooleanMinimumLengthUnparser(
  e: ElementRuntimeData,
  binaryBooleanTrueRep: MaybeULong,
  binaryBooleanFalseRep: ULong,
  lengthUnits: LengthUnits
) extends BinaryBooleanUnparserBase(
    e,
    binaryBooleanTrueRep,
    binaryBooleanFalseRep,
    lengthUnits
  ) {

  override def runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = 32

}
