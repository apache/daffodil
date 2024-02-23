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
import java.util.Optional
import scala.collection.JavaConverters._

import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.api.DFDLPrimType
import org.apache.daffodil.runtime1.layers.api.JLayerLengthKind
import org.apache.daffodil.runtime1.layers.api.JLayerLengthUnits
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerChecksumMixin
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.layers.api.LayerVariables

object CheckDigitLayer {
  val name: String = "checkDigit"
  val vars: LayerVariables = LayerVariables(
    prefix = "cd",
    namespace = "urn:org.apache.daffodil.layers.checkDigit",
    variables = Seq(
      // variable name is the same as the layer name "checkDigit"
      (name, DFDLPrimType.Int), // index 0
      // variable name is "checkDigitParams"
      (name + "Params", DFDLPrimType.String), // index 1
    ).asJava,
  )
}

class CheckDigitLayer()
  extends Layer(
    layerName = CheckDigitLayer.name,
    supportedLayerLengthKinds = Seq(JLayerLengthKind.Explicit).asJava,
    supportedLayerLengthUnits = Seq(JLayerLengthUnits.Bytes).asJava,
    isRequiredLayerEncoding = true,
    optLayerVariables = Optional.of(CheckDigitLayer.vars),
  )
  with LayerChecksumMixin {

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

  /**
   * Shared by both parsing and unparsing.
   *
   * Ignores any non-digit character in the argument.
   *
   * The checkDigit is the total of all digits, viewed as a string, the last digit of that total.
   */
  override def compute(
    layerRuntime: LayerRuntime,
    isUnparse: Boolean,
    byteBuffer: ByteBuffer,
  ) = {
    val paramVar = layerRuntime.variable(1)
    val isVerbose = parseParams(layerRuntime.getString(paramVar)).isVerbose
    val s = new String(byteBuffer.array(), layerRuntime.layerCharset.name)
    val digits: Seq[Int] = s.filter { _.isDigit }.map { _.asDigit }
    val num = digits.sum
    //
    // TODO: verify if this is correct. I believe this is supposed to keep adding the digits together until
    //  it gets a sum with only 1 digit and that is the check digit value.
    val checkDigit = num.toString.last.asDigit
    if (isVerbose)
      Logger.log.info(s"Check digit for '$s' is '$checkDigit'")
    checkDigit
  }
}
