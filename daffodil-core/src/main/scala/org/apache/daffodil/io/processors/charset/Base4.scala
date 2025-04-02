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
 * Base 4 aka Quarternary
 */

object BitsCharsetBase4LSBF extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-BASE4-LSBF"
  override lazy val bitWidthOfACodeUnit = 2
  override lazy val decodeString = "0123"
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.LeastSignificantBitFirst
}

final class BitsCharsetBase4LSBFDefinition extends BitsCharsetDefinition(BitsCharsetBase4LSBF)

object BitsCharsetBase4MSBF extends BitsCharsetNonByteSize {
  override lazy val name = "X-DFDL-BASE4-MSBF"
  override lazy val bitWidthOfACodeUnit = 2
  override lazy val decodeString = "0123"
  override lazy val replacementCharCode = 0x0
  override lazy val requiredBitOrder = BitOrder.MostSignificantBitFirst
}

final class BitsCharsetBase4MSBFDefinition extends BitsCharsetDefinition(BitsCharsetBase4MSBF)
