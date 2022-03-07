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

package org.apache.daffodil.charsets

import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.processors.charset.BitsCharsetNonByteSize
import org.apache.daffodil.processors.charset.BitsCharsetDefinition

object BitsCharset_ISO_8859_1_Reverse extends{
  override val name = "X-DFDL-ISO-8859-1-8-BIT-PACKED-LSB-FIRST-REVERSE"
  override val bitWidthOfACodeUnit = 8
  override val decodeString = (0 to 255).map { _.toChar }.mkString.reverse
  override val replacementCharCode = 0x0
  override val requiredBitOrder = BitOrder.MostSignificantBitFirst
} with BitsCharsetNonByteSize


final class BitsCharset_ISO_8859_1_Reverse_Definition
  extends BitsCharsetDefinition(BitsCharset_ISO_8859_1_Reverse)
