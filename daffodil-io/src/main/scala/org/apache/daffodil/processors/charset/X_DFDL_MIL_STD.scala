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

/*
 * This file contains special character encodings used by various US military data formats
 * The DFI/DUI numbers may be used to lookup the definition of the encoding by those with access
 * to the relevent specification. In cases where multiple DFI/DUI pairs make use of the same encoding,
 * the decision of which pair to name the encoding after is arbitrary.
 *
 * Unless otherwise stated, characters defined as "NO STATEMENT" are assumed to be padding characters.
 * These characters are translated into a no-break space (U+00A0)
 */

package org.apache.daffodil.processors.charset

import org.apache.daffodil.schema.annotation.props.gen.BitOrder

/**
 * X-DFDL-6-BIT-DFI-264-DUI-001, special 6 bit encoding
 */
object BitsCharset6BitDFI264DUI001 extends {
  override val name = "X-DFDL-6-BIT-DFI-264-DUI-001"
  override val bitWidthOfACodeUnit = 6
  override val decodeString = """ 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD0"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

object BitsCharset6BitDFI311DUI002 extends {
  override val name = "X-DFDL-6-BIT-DFI-311-DUI-002"
  override val bitWidthOfACodeUnit = 6
  override val decodeString = """\u00A0ABCDEFGHIJKLMNOPQRSTUVWXYZ\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD \uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD0123456789\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

object BitsCharset3BitDFI336DUI001 extends {
  override val name = "X-DFDL-3-BIT-DFI-336-DUI-001"
  override val bitWidthOfACodeUnit = 3
  override val decodeString = """12345678"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

object BitsCharset4BitDFI746DUI002 extends {
  override val name = "X-DFDL-4-BIT-DFI-746-DUI-002"
  override val bitWidthOfACodeUnit = 4
  override val decodeString = """ABCDEFGHIJKLMNPQ"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

object BitsCharset3BitDFI746DUI002 extends {
  override val name = "X-DFDL-3-BIT-DFI-746-DUI-002"
  override val bitWidthOfACodeUnit = 3
  override val decodeString = """ABCDEFGH"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

object BitsCharset3BitDFI747DUI001 extends {
  override val name = "X-DFDL-3-BIT-DFI-747-DUI-001"
  override val bitWidthOfACodeUnit = 3
  override val decodeString = """AEGHJKLM"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

object BitsCharset5BitDFI769DUI002 extends {
  override val name = "X-DFDL-5-BIT-DFI-769-DUI-002"
  override val bitWidthOfACodeUnit = 5
  override val decodeString = """01234567ABCDEFGHJKLMNPQRSTUVWXYZ"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize

object BitsCharset5BitDFI1661DUI001 extends {
  override val name = "X-DFDL-5-BIT-DFI-1661-DUI-001"
  override val bitWidthOfACodeUnit = 5
  override val decodeString = """\u00A0ABCDEFGHIJKLMNOPQRSTUVWXYZ\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD"""
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.LeastSignificantBitFirst
} with BitsCharsetNonByteSize
