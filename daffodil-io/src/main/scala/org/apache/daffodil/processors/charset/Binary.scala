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
 * X-DFDL-BITS-LSBF occupies only 1 bit with each
 * code unit.
 */
object BitsCharsetBinaryLSBF extends {
  override val name = "X-DFDL-BITS-LSBF"
  override val bitWidthOfACodeUnit = 1
  override val decodeString = "01"
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

/**
 * X-DFDL-BITS-MSBF occupies only 1 bit with each
 * code unit.
 */
object BitsCharsetBinaryMSBF extends {
  override val name = "X-DFDL-BITS-MSBF"
  override val bitWidthOfACodeUnit = 1
  override val decodeString = "01"
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.MostSignificantBitFirst
} with BitsCharsetNonByteSize
