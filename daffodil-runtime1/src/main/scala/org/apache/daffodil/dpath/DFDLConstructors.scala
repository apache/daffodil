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

import java.lang.{ Byte => JByte }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Short => JShort }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.infoset.DataValue.DataValueByte
import org.apache.daffodil.infoset.DataValue.DataValueByteArray
import org.apache.daffodil.infoset.DataValue.DataValueInt
import org.apache.daffodil.infoset.DataValue.DataValueLong
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValueShort
import org.apache.daffodil.util.Numbers.asBigInt

abstract class DFDLConstructorFunction(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {

  def constructorName: String
  def maxHexDigits: Int

  lazy val nfeMsg = "%s cannot be cast to dfdl:" + constructorName + "\ndfdl:" + constructorName + " received an unrecognized type! Must be String, Byte, Short, Integer, Long or a subtype thereof."
  lazy val hexMsg = "dfdl:" + constructorName + " received string violates maximum hex digits.  Received %s expected a max of " + maxHexDigits

  protected def convert(longValue: DataValueLong, dstate: DState): DataValuePrimitive

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    val long:DataValueLong = a.getAnyRef match {
      case _: JByte | _: JShort | _: JInt => IntToLong.computeValue(a, dstate)
      case l: JLong => l
      case s: String if s.startsWith("x") => {
        val hexStr = s.substring(1)
        if (hexStr.length > maxHexDigits) throw new NumberFormatException(hexMsg.format(hexStr.length))
        HexStringToLong.computeValue(hexStr, dstate)
      }
      case s: String => StringToLong.computeValue(s, dstate)
      case bi: BigInt => BigIntToLong.computeValue(bi, dstate)
      case bd: BigDecimal if (bd.isWhole()) => BigIntToLong.computeValue(bd.toBigInt, dstate)
      case bi: JBigInt => BigIntToLong.computeValue(bi, dstate)
      case bd: JBigDecimal if (bd.remainder(JBigDecimal.ONE) == JBigDecimal.ZERO) => BigIntToLong.computeValue(bd.toBigInteger(), dstate)
      case hb: Array[Byte] => {
        val str = "0x" + HexBinaryToString.computeValue(hb, dstate).getString
        throw new NumberFormatException(nfeMsg.format(str))
      }
      case x =>
        throw new NumberFormatException(nfeMsg.format(x))
    }
    convert(long, dstate)
  }
}

/**
 * The argument can
 * also be a long, unsignedLong, or any subtype
 * thereof, and in that case a xs:hexBinary value
 * containing a number of hex digits is produced.
 * The ordering and number of the digits
 * correspond to a binary big-endian twos-
 * complement implementation of the type of the
 * argument. Digits 0-9, A-F are used.
 * The number of digits produced depends on the
 * type of \$arg, being 2, 4, 8 or 16. If \$arg is a
 * literal number then the type is the smallest
 * signed type (long, int, short, byte) that can
 * contain the value.
 * If a literal number is not able to be represented
 * by a long, it is a schema definition error.
 *
 * • dfdl:hexBinary(xs:short(208)) is the hexBinary value "00D0".
 * • dfdl:hexBinary(208) is the hexBinary value "D0".
 * • dfdl:hexBinary(-2084) is the hexBinary value "F7DC".
 *
 */
case class DFDLHexBinary(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with HexBinaryKind {
  val name = "DFDLHexBinary"

  lazy val nfeMsg = "%s cannot be cast to dfdl:hexBinary\ndfdl:hexBinary received an unrecognized type! Must be String, Byte, Short, Integer, Long or a subtype thereof."

  /**
   * If the argument is of some fixed-width type like Byte, Int, Short, Long or
   * the unsigned thereof, then you get hex digits corresponding to 1, 2, 4, or 8 bytes of
   * a binary twos-complement integer value of that type. If the argument is anything
   * else (including a literal number), then you get the smallest number of hex digit pairs
   * that can represent the value.  If you get xs:integer (aka BigInt/Java BigInteger), then
   * I'd say - smallest number of digits that can represent the value.
   *
   * So dfdl:hexBinary(208) is D0, dfdl:hexBinary(xs:integer(208))
   * is also D0 dfdl:hexBinary(xs:short(208)) is 00D0
   *
   */
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueByteArray = {
    val arr = a.getAnyRef match {
      case s: String => {
        // Literal number
        reduce(s)
      }
      case b: JByte => HexBinaryConversions.toByteArray(b)
      case s: JShort => HexBinaryConversions.toByteArray(s)
      case bi: BigInt => {
        // Literal number
        reduce(bi)
      }
      case i: JInt => {
        // Possibly a Literal Number, try to fit it into the smallest
        // value anyway.
        reduce(i)
      }
      case l: JLong => HexBinaryConversions.toByteArray(l)
      case ul: BigDecimal => {
        reduce(ul)
      }
      case bi: JBigInt => reduce(bi)
      case bd: JBigDecimal => reduce(bd)

      case hb: Array[Byte] => hb
      case x => throw new NumberFormatException(nfeMsg.format(x))
    }

    arr
  }
}

case class DFDLByte(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {

  val constructorName = "byte"
  val maxHexDigits = 2

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueByte = LongToByte.computeValue(longValue, dstate)
}

case class DFDLUnsignedByte(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedByte"
  val maxHexDigits = 2

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueShort = LongToUnsignedByte.computeValue(longValue, dstate)
}

case class DFDLShort(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "short"
  val maxHexDigits = 4

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueShort = LongToShort.computeValue(longValue, dstate)
}

case class DFDLUnsignedShort(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedShort"
  val maxHexDigits = 4

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueInt = LongToUnsignedShort.computeValue(longValue, dstate)
}

case class DFDLInt(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "int"
  val maxHexDigits = 8

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueInt = LongToInt.computeValue(longValue, dstate)
}

case class DFDLUnsignedInt(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedInt"
  val maxHexDigits = 8

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueLong = LongToUnsignedInt.computeValue(longValue, dstate)
}

case class DFDLLong(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "long"
  val maxHexDigits = 16

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueLong = longValue
}

case class DFDLUnsignedLong(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedLong"
  val maxHexDigits = 16

  protected def convert(longValue: DataValueLong, dstate: DState): DataValueBigInt = asBigInt(longValue.getLong)

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    val ulong: DataValuePrimitive = a.getAnyRef match {
      case _: JByte | _: JShort | _: JInt => IntegerToUnsignedLong.computeValue(a, dstate)
      case s: String if s.startsWith("x") => {
        val hexStr = s.substring(1)
        if (hexStr.length > maxHexDigits) throw new NumberFormatException(hexMsg.format(hexStr.length))
        HexStringToUnsignedLong.computeValue(hexStr, dstate)
      }
      case s: String => StringToUnsignedLong.computeValue(s, dstate)
      case bi: JBigInt => IntegerToUnsignedLong.computeValue(bi, dstate)
      case bd: JBigDecimal => IntegerToUnsignedLong.computeValue(bd, dstate)
      case bi: BigInt => IntegerToUnsignedLong.computeValue(bi, dstate)
      case bd: BigDecimal => IntegerToUnsignedLong.computeValue(bd, dstate)
      case x =>
        throw new NumberFormatException(nfeMsg.format(x))
    }
    ulong
  }
}
