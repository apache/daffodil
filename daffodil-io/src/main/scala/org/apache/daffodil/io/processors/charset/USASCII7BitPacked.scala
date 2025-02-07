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

package org.apache.daffodil.io.processors.charset

import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder

/**
 * X-DFDL-US-ASCII-7-BIT-PACKED occupies only 7 bits with each
 * code unit.
 */
object BitsCharsetUSASCII7BitPacked extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-US-ASCII-7-BIT-PACKED"
  override lazy val bitWidthOfACodeUnit = 7
  override lazy val decodeString = (0 to 127).map { _.toChar }.mkString
  override lazy val replacementCharCode = 0x3f
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharsetUSASCII7BitPackedDefinition
  extends BitsCharsetDefinition(BitsCharsetUSASCII7BitPacked)
