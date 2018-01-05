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
 * Some encodings are not byte-oriented.
 *
 * X-DFDL-US-ASCII-6-BIT-PACKED occupies only 6 bits with each
 * code unit.
 *
 */

object USASCII6BitPackedCharset
  extends NBitsWidth_BitsCharset("X-DFDL-US-ASCII-6-BIT-PACKED",
    """@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ !"#$%&'()*+,-./0123456789:;<=>?""",
    6, // width
    BitOrder.LeastSignificantBitFirst,
    0x1F) { // replacement charCode for encoding of unmapped chars.
  //
  // Note: formula for computing string above is
  // def decodeString = ((0 to 63).map { charCode => (if (charCode <= 31) charCode + 64 else charCode).toChar }).mkString
}

