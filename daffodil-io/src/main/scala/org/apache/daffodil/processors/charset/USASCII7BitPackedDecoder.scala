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
 * X-DFDL-US-ASCII-7-BIT-PACKED occupies only 7 bits with each
 * code unit.
 *
 * There are 6 bit and 5 bit encodings in use as well. (One can even think of hexadecimal as
 * a 4-bit encoding of 16 possible characters - might be a cool way to
 * implement packed decimals of various sorts.)
 */

object USASCII7BitPackedCharset
  extends NBitsWidth_BitsCharset("X-DFDL-US-ASCII-7-BIT-PACKED",
    (0 to 127).map { _.toChar }.mkString,
    7,
    BitOrder.LeastSignificantBitFirst,
    0x3F)
