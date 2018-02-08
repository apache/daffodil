/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.util

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.BinaryNumberCheckPolicy

import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }

object PackedSignCodes {
  def apply(signCodes: String, policy: BinaryNumberCheckPolicy) = {
    val str = signCodes.replaceAll("\\s", "")
    val chars = str.toCharArray()
    Assert.invariant(chars.length == 4)

    //We can convert hex to an integer value simply by subtracting 55
    val positive = policy match {
      case BinaryNumberCheckPolicy.Strict => List(chars(0) - 55)
      case BinaryNumberCheckPolicy.Lax => List(chars(0) - 55, 0xA, 0xC, 0xE, 0xF)
    }
    val negative = policy match {
      case BinaryNumberCheckPolicy.Strict => List(chars(1) - 55)
      case BinaryNumberCheckPolicy.Lax => List(chars(1) - 55, 0xB, 0xD)
    }
    val unsigned = policy match {
      case BinaryNumberCheckPolicy.Strict => List(chars(2) - 55)
      case BinaryNumberCheckPolicy.Lax => List(chars(2) - 55, 0xF)
    }
    val zero_sign = policy match {
      case BinaryNumberCheckPolicy.Strict => List(chars(3) - 55)
      case BinaryNumberCheckPolicy.Lax => List(chars(3) - 55, 0xA, 0xC, 0xE, 0xF, 0x0)
    }

    new PackedSignCodes(positive, negative, unsigned, zero_sign)
  }
}

case class PackedSignCodes(
  positive: List[Int],
  negative: List[Int],
  unsigned: List[Int],
  zero_sign: List[Int]
) {}

object DecimalUtils {

  def packedToBigInteger(num: Array[Byte], signCodes: PackedSignCodes): JBigInteger = {
    val numDigits = num.size * 2  // 2 digits stored per byte
    val outputData = new Array[Char](numDigits-1)
    var outputPos = 0
    var offset = 0

    // Parse and validate the last (sign) bit
    val signNibble = (num(offset +  num.size - 1) & 0x0F)
    val negative = signCodes.negative.contains(signNibble)
    if (!negative && !signCodes.positive.contains(signNibble) &&
      !signCodes.unsigned.contains(signNibble) && !signCodes.zero_sign.contains(signNibble)) {
      throw new NumberFormatException("Invalid sign nibble: " + signNibble)
    }

    while (outputPos < outputData.size - 1) {
      // Parse high nibble
      val highNibble = (num(offset) & 0xFF) >>> 4
      if (highNibble > 0x09) {
        throw new NumberFormatException("Invalid high nibble: " + highNibble)
      }

      outputData(outputPos) = (highNibble | 0x0030).toChar
      outputPos = outputPos + 1

      // Parse low nibble
      val lowNibble = (num(offset) & 0x0F)
      if (lowNibble > 0x09) {
        throw new NumberFormatException("Invalid low nibble: " + lowNibble)
      }

      outputData(outputPos) = (lowNibble | 0x0030).toChar
      outputPos = outputPos + 1
      offset = offset + 1
    }

    // Parse last digit
    val lastNibble = (num(offset) & 0xFF) >>> 4
    if (lastNibble > 0x09) {
      throw new NumberFormatException("Invalid high nibble: " + lastNibble)
    }

    outputData(outputPos) = (lastNibble | 0x0030).toChar

    val jbi = new JBigInteger(new String(outputData))
    if (negative)
      jbi.negate()
    else
      jbi

  }

  def packedToBigDecimal(num: Array[Byte], scale: Int, signCodes: PackedSignCodes): JBigDecimal = {
    return new JBigDecimal(packedToBigInteger(num, signCodes), scale)
  }

  def packedFromBigInteger(bigInt: JBigInteger, minLengthInBits: Int, signCodes: PackedSignCodes): Array[Byte] = {
    val negative = (bigInt.signum != 1)
    val inChars = bigInt.abs.toString.toCharArray
    val numDigits = inChars.length
    // Length required to fit all the digits of the number including sign nibble
    val requiredBitLen = if (numDigits % 2 == 0) ((numDigits + 2) * 4) else ((numDigits + 1) * 4)
    val bitLen = scala.math.max(minLengthInBits, requiredBitLen)
    var offset = 0
    var inPos = 0
    val outArray = new Array[Byte](bitLen/8)
    val leadingZeros = if (numDigits % 2 == 0) (bitLen/4 - numDigits - 1) else (bitLen/4 - numDigits)

    // Add leading double zeros if necessary
    while ((offset * 2) < (leadingZeros - 1)) {
      outArray(offset) = 0x00.asInstanceOf[Byte]
      offset = offset + 1
    }

    // Need odd number of digits, pad with 0x0 if necessary
    if (numDigits % 2 == 0) {
      outArray(offset) = (((0x0 & 0x000F) << 4) + (inChars(inPos) & 0x000F)).asInstanceOf[Byte]
      inPos = inPos + 1
      offset = offset + 1
    }

    while (inPos < numDigits - 1) {
      val firstNibble = (inChars(inPos) & 0x000F) << 4
      inPos = inPos + 1
      val secondNibble = inChars(inPos) & 0x000F
      inPos = inPos + 1
      outArray(offset) = (firstNibble + secondNibble).asInstanceOf[Byte]
      offset = offset + 1
    }

    // Write out the last digit and sign code

    val lastNibble = (inChars(inPos) & 0x000F) << 4
    val signNibble = if (negative) (signCodes.negative(0) & 0x000F) else (signCodes.positive(0) & 0x000F)
    outArray(offset) = (lastNibble + signNibble).asInstanceOf[Byte]

    outArray
  }

  def bcdToBigInteger(bcdNum: Array[Byte]): JBigInteger = {
    val numDigits = bcdNum.size * 2  // 2 digits stored per byte
    val outputData = new Array[Char](numDigits)
    var outputPos = 0
    var offset = 0

    while (offset < bcdNum.size) {
      val highNibble = (bcdNum(offset) & 0xFF) >>> 4
      if (highNibble > 0x09)
        throw new NumberFormatException("Invalid high nibble: " + highNibble)

      outputData(outputPos) = (highNibble | 0x0030).toChar
      outputPos = outputPos + 1

      val lowNibble = (bcdNum(offset) & 0x0F)
      if (lowNibble > 0x09) {
        throw new NumberFormatException("Invalid low nibble: " + lowNibble)
      }

      outputData(outputPos) = (lowNibble | 0x0030).toChar
      outputPos = outputPos + 1
      offset = offset + 1
    }

    new JBigInteger(new String(outputData))
  }

  def bcdToBigDecimal(bcdNum: Array[Byte], scale: Int): JBigDecimal = {
    new JBigDecimal(bcdToBigInteger(bcdNum), scale)
  }

  def bcdFromBigInteger(bigInt: JBigInteger, minLengthInBits: Int): Array[Byte] = {
    val inChars = bigInt.toString.toCharArray
    val numDigits = inChars.length
    // Need to have an even number of digits to fill out a complete byte
    val requiredBitLen = if (numDigits % 2 == 0) (numDigits * 4) else ((numDigits + 1) * 4)
    val bitLen = scala.math.max(minLengthInBits, requiredBitLen)
    var offset = 0
    var inPos = 0
    val outArray = new Array[Byte](bitLen/8)
    val leadingZeros = bitLen/4 - inChars.length

    // Add leading double zeros if necessary
    while ((offset * 2) < (leadingZeros - 1)) {
      outArray(offset) = 0x00.asInstanceOf[Byte]
      offset = offset + 1
    }

    // Need even number of digits, pad with a single 0 if necessary
    if (inChars.length % 2 != 0) {
      outArray(offset) = (((0x0 & 0x000F) << 4) + (inChars(inPos) & 0x000F)).asInstanceOf[Byte]
      offset = offset + 1
      inPos = inPos + 1
    }

    while (inPos < inChars.length) {
      val firstNibble = (inChars(inPos) & 0x000F) << 4
      inPos = inPos + 1
      val secondNibble = inChars(inPos) & 0x000F
      inPos = inPos + 1
      outArray(offset) = (firstNibble + secondNibble).asInstanceOf[Byte]
      offset = offset + 1
    }

    outArray
  }

  def ibm4690ToBigInteger(num: Array[Byte]): JBigInteger = {
    val numDigits = num.size * 2  // 2 digits stored per byte
    val outputData = new Array[Char](numDigits)
    var outputPos = 0
    var offset = 0
    var negative = false
    // We've started parsing non-padding/sign nibbles
    var inDigits = false

    while (offset < num.size) {
      // Parse high nibble
      val highNibble = (num(offset) & 0xFF) >>> 4
      if (highNibble > 0x09) {
        outputData(outputPos) = '0'
        if ((highNibble == 0xD) && !inDigits) {
          negative = true
          inDigits = true
        } else if ((highNibble != 0xF) || inDigits) {
          throw new NumberFormatException("Invalid high nibble: " + highNibble)
        }

        outputPos = outputPos + 1
      }

      if (highNibble <= 0x09) {
        inDigits = true
        outputData(outputPos) = (highNibble | 0x0030).toChar
        outputPos = outputPos + 1
      }

      // Parse low nibble
      val lowNibble = (num(offset) & 0x0F)
      if (lowNibble > 0x09) {
        outputData(outputPos) = '0'
        outputPos = outputPos + 1
        if (lowNibble == 0xD && !inDigits) {
          negative = true
          inDigits = true
        } else if ((lowNibble != 0xF) || inDigits) {
          throw new NumberFormatException("Invalid low nibble: " + lowNibble)
        }
      }

      if (lowNibble <= 0x09) {
        inDigits = true
        outputData(outputPos) = (lowNibble | 0x0030).toChar
        outputPos = outputPos + 1
      }
      offset = offset + 1
    }

    val jbi = new JBigInteger(new String(outputData))
    if (negative)
      jbi.negate()
    else
      jbi
  }

  def ibm4690ToBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = {
    new JBigDecimal(ibm4690ToBigInteger(num), scale)
  }

  def ibm4690FromBigInteger(bigInt: JBigInteger, minLengthInBits: Int): Array[Byte] = {
    val negative = (bigInt.signum != 1)
    val inChars = bigInt.abs.toString.toCharArray
    var wrote_negative = false
    var offset = 0
    var inPos = 0
    val numDigits = if (negative) inChars.length + 1 else inChars.length
    val requiredBitLen = if (numDigits % 2 == 0) (numDigits * 4) else ((numDigits + 1) * 4)
    val bitLen = scala.math.max(minLengthInBits, requiredBitLen)
    val outArray = new Array[Byte](bitLen/8)
    val leadingZeros = if (numDigits % 2 == 0) (bitLen/4 - numDigits) else (bitLen/4 - (numDigits + 1))

    // Add leading double zeros if necessary
    while ((offset * 2) < (leadingZeros - 1)) {
      outArray(offset) = 0xFF.asInstanceOf[Byte]
      offset = offset + 1
    }

    // Need even number of digits, pad with 0xF if necessary
    if (numDigits % 2 != 0) {
      val padNibble = (0xF & 0x000F) << 4
      val signNibble = if (negative) {
        wrote_negative = true
        0xD & 0x000F
      } else {
        inPos = inPos + 1
        inChars(inPos-1) & 0x000F
      }
      outArray(offset) = (padNibble + signNibble).asInstanceOf[Byte]
      offset = offset + 1
    }

    while (inPos < numDigits - 1) {
      val firstNibble = if (negative && !wrote_negative) {
        wrote_negative = true
        (0xD & 0x000F) << 4
      } else {
        inPos = inPos + 1
        (inChars(inPos-1) & 0x000F) << 4
      }
      val secondNibble = inChars(inPos) & 0x000F
      inPos = inPos + 1
      outArray(offset) = (firstNibble + secondNibble).asInstanceOf[Byte]
      offset = offset + 1
    }

    outArray
  }

}
