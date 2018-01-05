/* Copyright (c) 2012-2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.util.Maybe._
import java.lang.{ Long => JLong, Boolean => JBoolean }
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.util.Numbers
import org.apache.daffodil.util.MaybeULong
import passera.unsigned.ULong
import org.apache.daffodil.io.FormatInfo

class BinaryBooleanUnparser(e: ElementRuntimeData,
    binaryBooleanTrueRep: MaybeULong,
    binaryBooleanFalseRep: ULong,
    val lengthEv: Evaluatable[JLong],
    val lUnits: LengthUnits,
    val lengthKind: LengthKind)
  extends PrimUnparser() {

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
  
  override lazy val runtimeDependencies = List(lengthEv)


  protected def putNumber(dos: DataOutputStream, value: ULong, nBits: Int, finfo: FormatInfo): Boolean = {
    dos.putULong(value, nBits, finfo)
  }

  def unparse(state: UState): Unit = {
    val nBits = getBitLength(state)
    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[JBoolean]
    val dos = state.dataOutputStream

    if (nBits < 1 || nBits > 32) {
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Number of bits %d out of range, must be between 1 and 32 bits.", nBits)
    }

    Assert.invariant(binaryBooleanTrueRep.isEmpty || binaryBooleanTrueRep.getULong >= ULong(0))
    Assert.invariant(binaryBooleanFalseRep >= ULong(0))

    val res =
      if (value) {
        val trueRep = if (binaryBooleanTrueRep.isDefined) binaryBooleanTrueRep.getULong else ~binaryBooleanFalseRep
        putNumber(dos, trueRep, nBits, state)
      } else {
        putNumber(dos, binaryBooleanFalseRep, nBits, state)
      }

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        e.dpathElementCompileInfo.namedQName.toPrettyString, nBits, dos.maybeRelBitLimit0b.get)
    }
  }

}
