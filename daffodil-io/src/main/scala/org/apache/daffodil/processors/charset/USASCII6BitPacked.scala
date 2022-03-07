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
 * X-DFDL-US-ASCII-6-BIT-PACKED occupies only 6 bits with each
 * code unit.
 */
object BitsCharsetUSASCII6BitPackedLSBF extends {
  override val name = "X-DFDL-US-ASCII-6-BIT-PACKED-LSB-FIRST"
  override val bitWidthOfACodeUnit = 6
  override val decodeString = """@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ !"#$%&'()*+,-./0123456789:;<=>?"""
  override val replacementCharCode = 0x1F
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

final class BitsCharsetUSASCII6BitPackedLSBFDefinition
  extends BitsCharsetDefinition(BitsCharsetUSASCII6BitPackedLSBF)

final class BitsCharsetUSASCII6BitPackedDefinition
  extends BitsCharsetDefinition(BitsCharsetUSASCII6BitPackedLSBF, Some("X-DFDL-US-ASCII-6-BIT-PACKED"))

object BitsCharsetUSASCII6BitPackedMSBF extends {
  override val name = "X-DFDL-US-ASCII-6-BIT-PACKED-MSB-FIRST"
  override val bitWidthOfACodeUnit = 6
  override val decodeString = """@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ !"#$%&'()*+,-./0123456789:;<=>?"""
  override val replacementCharCode = 0x1F
  override val requiredBitOrder = BitOrder.MostSignificantBitFirst
} with BitsCharsetNonByteSize

final class BitsCharsetUSASCII6BitPackedMSBFDefinition
  extends BitsCharsetDefinition(BitsCharsetUSASCII6BitPackedMSBF)
