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
 * X-DFDL-ISO-88591-8-BIT-PACKED-LSB-FIRST occupies only 8 bits with each
 * code unit.
 */
object BitsCharsetISO885918BitPackedLSBF extends {
  override val name = "X-DFDL-ISO-88591-8-BIT-PACKED-LSB-FIRST"
  override val bitWidthOfACodeUnit = 8
  override val decodeString = (0 to 255).map { _.toChar }.mkString
  override val replacementCharCode = 0x3F
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

/**
 * X-DFDL-ISO-88591-8-BIT-PACKED-MSB-FIRST occupies only 8 bits with each
 * code unit.
 */
object BitsCharsetISO885918BitPackedMSBF extends {
  override val name = "X-DFDL-ISO-88591-8-BIT-PACKED-MSB-FIRST"
  override val bitWidthOfACodeUnit = 8
  override val decodeString = (0 to 255).map { _.toChar }.mkString
  override val replacementCharCode = 0x3F
  override val requiredBitOrder = BitOrder.MostSignificantBitFirst
} with BitsCharsetNonByteSize
