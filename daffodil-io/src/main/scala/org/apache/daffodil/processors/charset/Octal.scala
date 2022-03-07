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

package org.apache.daffodil.processors.charset

import org.apache.daffodil.schema.annotation.props.gen.BitOrder

/**
 * X-DFDL-OCTAL-LSBF occupies only 3 bits with each
 * code unit.
 */
object BitsCharsetOctalLSBF extends {
  override val name = "X-DFDL-OCTAL-LSBF"
  override val bitWidthOfACodeUnit = 3
  override val decodeString = "01234567"
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

final class BitsCharsetOctalLSBFDefinition
  extends BitsCharsetDefinition(BitsCharsetOctalLSBF)

/**
 * X-DFDL-OCTAL-MSBF occupies only 3 bits with each
 * code unit.
 */
object BitsCharsetOctalMSBF extends {
  override val name = "X-DFDL-OCTAL-MSBF"
  override val bitWidthOfACodeUnit = 3
  override val decodeString = "01234567"
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.MostSignificantBitFirst
} with BitsCharsetNonByteSize 

final class BitsCharsetOctalMSBFDefinition
  extends BitsCharsetDefinition(BitsCharsetOctalMSBF)
