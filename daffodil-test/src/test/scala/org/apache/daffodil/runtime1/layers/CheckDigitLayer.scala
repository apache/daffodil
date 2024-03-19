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

package org.apache.daffodil.runtime1.layers

import java.nio.ByteBuffer
import java.nio.charset.Charset

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.layers.api.ChecksumLayer

/** Check digit example layer
 *
 * Computes a check digit by adding together the digits of a number and taking the
 * final digit of that sum.
 *
 * The resulting check digit is stored into the DFDL variable:
 *   - checkDigit - an xs:unsignedShort to which the check digit value is assigned.
 * The arguments to this layer algorithm are taken from the three layer DFDL variables that
 * define parameters for this check digit calculation:
 *   - length - an xs:unsignedShort giving the length (in bytes) of the data over
 *   which the check-digit is to be computed.
 *   Only digits in this data region contribute to the check digit value.
 *   Other characters are ignored.
 *   There must be at least one digit in the region or it is a processing error.
 *   - params is a string which can contain 'verbose' (case insensitive) which turns on logging
 *   - digitEncoding is a string which is the name of a character set encoding which
 *   defines the character set of the digits.
 *   This must match the dfdl:encoding for the DFDL element declaration using this layer.
 */
class CheckDigitLayer
  extends ChecksumLayer(
    "checkDigit", // name of the layer, also happens to be the name of the output variable.
    "urn:org.apache.daffodil.layers.checkDigit",
  ) {

  private var params: String = _
  private var digitEncoding: String = _

  /**
   * Lazy so that if the digitEncoding is not a valid charset, that we get
   * a processing error at runtime, not a unrecoverable/fatal error.
   */
  private lazy val charset =
    try {
      Charset.forName(digitEncoding)
    } catch {
      case re: RuntimeException => {
        processingError(re)
        Assert.impossible("prior statement ends with throw")
      }
    }
  private lazy val isVerbose = params.toLowerCase.contains("verbose")

  /**
   * @param length the value of the length DFDL variable
   * @param params the value of the params DFDL variable
   * @param digitEncoding the value of the digitEncoding DFDL variable
   */
  final private[layers] def setLayerVariableParameters(
    length: Short,
    params: String,
    digitEncoding: String,
  ): Unit = {
    setLength(length)
    this.params = params
    this.digitEncoding = digitEncoding
  }

  /**
   * Result DFDL variable value getter. This getter is called to obtain the value for,
   * and populate the DFDL variable named checkDigit, in the layer's target namespace.
   * @return the check digit value
   */
  def getLayerVariableResult_checkDigit: Int = getChecksum

  /**
   * Shared by both parsing and unparsing.
   *
   * Ignores any non-digit character in the argument.
   *
   * The checkDigit is the total of all digits, viewed as a string, the last digit of that total.
   */
  override def compute(isUnparse: Boolean, byteBuffer: ByteBuffer): Int = {
    val s = new String(byteBuffer.array(), charset)
    val digits: Seq[Int] = s.filter { _.isDigit }.map { _.asDigit }
    if (digits.isEmpty)
      processingError("No digits in data to compute a check digit from.")
    val num = digits.sum
    //
    // A variation would be to keep adding the digits together until
    // one gets a sum with only 1 digit and that is the check digit value.
    //
    // This algorithm just adds the digits once, and takes the last digit of the sum
    //
    val checkDigit = num.toString.last.asDigit
    if (isVerbose)
      Logger.log.info(s"Check digit for '$s' is '$checkDigit'")
    checkDigit
  }

}
