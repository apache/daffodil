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
import org.apache.daffodil.util.MaybeInt

/**
 * Some encodings are not byte-oriented.
 *
 * X-DFDL-5-BIT-PACKED-LSBF occupies only 5 bits with each
 * code unit.
 *
 */

object DFDL5BitPackedLSBFCharset
  extends NBitsWidth_BitsCharset("X-DFDL-5-BIT-PACKED-LSBF",
    "01234567ABCDEFGHJKLMNPQRSTUVWXYZ",
    5, // width
    BitOrder.LeastSignificantBitFirst,
    0x1D) { // replacement

  override def charToCode(char: Char) = {
    if (char == 'I') MaybeInt(1)
    else if (char == 'O') MaybeInt(0)
    else super.charToCode(char)
  }
}

