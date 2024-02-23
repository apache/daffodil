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

import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.api.DFDLPrimType
import org.apache.daffodil.runtime1.layers.api.ChecksumLayer
import org.apache.daffodil.runtime1.layers.api.LayerCompileInfo
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.layers.api.LayerVariable

class CheckDigitLayer()
  extends ChecksumLayer(
    "checkDigit",
    "urn:org.apache.daffodil.layers.checkDigit",
  ) {

  private def ns = "urn:org.apache.daffodil.layers.checkDigit"

  private var paramVar: LayerVariable = _
  private var checkDigitEncodingVar: LayerVariable = _
  private var charset: Charset = _
  private var isVerbose = false
  private var isInitialized = false

  override def check(lci: LayerCompileInfo): Unit = {
    if (paramVar == null) {
      super.check(lci)
      paramVar = lci.layerVariable(ns, "checkDigitParams")
      checkDigitEncodingVar = lci.layerVariable(ns, "checkDigitEncoding")

      if (paramVar.dfdlPrimType != DFDLPrimType.String)
        lci.schemaDefinitionError(
          "The 'checkDigitParameters' variable is not of type 'xs:string'.",
        )
      if (checkDigitEncodingVar.dfdlPrimType != DFDLPrimType.String)
        lci.schemaDefinitionError(
          "The 'checkDigitEncoding' variable is not of type 'xs:string'.",
        )
    }
  }

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
  ): Int = {
    if (!isInitialized) {
      charset = layerRuntime.getCharset(layerRuntime.getString(checkDigitEncodingVar))
      isVerbose = parseParams(layerRuntime.getString(paramVar)).isVerbose
      isInitialized = true
    }
    val s = new String(byteBuffer.array(), charset)
    val digits: Seq[Int] = s.filter { _.isDigit }.map { _.asDigit }
    if (digits.isEmpty)
      layerRuntime.processingError("No digits in data to compute a check digit from.")
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
