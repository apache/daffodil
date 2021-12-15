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

package org.apache.daffodil.layers

import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.util.Logger

import java.nio.ByteBuffer


final class CheckDigitExplicit(
  name: String,
  layerRuntimeInfo: LayerRuntimeInfo,
  outputVar: VariableRuntimeData,
  inputVars: Seq[VariableRuntimeData])
extends ByteBufferExplicitLengthLayerTransform[Int](
  layerRuntimeInfo,
  name,
  inputVars,
  outputVar) {

  /**
   * This layer does not have a fixed constant known length.
   */
  override protected def layerBuiltInConstantLength = None

  //
  // Our example here takes a parameter which is a flag indicating if it is to
  // issue warning messages to help with debugging.
  //
  // But really this is just to show that a layer transform can read variables
  // as well as write them.
  //
  case class Params(isVerbose: Boolean)

  private def parseParams(paramString: String) = {
    if (paramString.toLowerCase.contains("verbose")) Params(isVerbose = true)
    else Params(isVerbose = false)
  }

  case class CheckDigitException(str: String) extends Exception(str)

  /**
   * Shared by both parsing and unparsing.
   *
   * Ignores any non-digit character in the argument.
   *
   * The checkDigit is the total of all digits, viewed as a string, the last digit of that total.
   */
  protected def compute(state: ParseOrUnparseState, isUnparse: Boolean, inputs: Seq[Any], byteBuffer: ByteBuffer) = {
    assert(inputs.length == 1)
    val charset = layerRuntimeInfo.optLayerCharset(state).get
    assert(charset.newDecoder().maxCharsPerByte() == 1) // is a SBCS charset
    val isVerbose = parseParams(inputs(0).asInstanceOf[String]).isVerbose
    val s = new String(byteBuffer.array(), charset)
    val digits: Seq[Int] = s.filter{ _.isDigit }.map{ _.asDigit }
    val num = digits.sum
    val checkDigit = num.toString.last.asDigit
    if (isVerbose)
      Logger.log.info(s"Check digit for '$s' is '$checkDigit'")
    checkDigit
  }
}
