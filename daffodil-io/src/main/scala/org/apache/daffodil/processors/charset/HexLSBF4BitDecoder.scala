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
 * X-DFDL-HEX-LSBF occupies only 4 bits with each
 * code unit.
 *
 */

object HexLSBF4BitCharset
  extends NBitsWidth_BitsCharset("X-DFDL-HEX-LSBF",
    "0123456789ABCDEF",
    4, // width
    BitOrder.LeastSignificantBitFirst,
    0) // replacement charCode for encoding of unmapped chars.

object HexMSBF4BitCharset
  extends NBitsWidth_BitsCharset("X-DFDL-HEX-MSBF",
    "0123456789ABCDEF",
    4, // width
    BitOrder.MostSignificantBitFirst,
    0) // replacement charCode for encoding of unmapped chars.
