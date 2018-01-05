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
 * X-DFDL-OCTAL-LSBF occupies only 3 bits with each
 * code unit.
 *
 */

object OctalLSBF3BitCharset
  extends NBitsWidth_BitsCharset("X-DFDL-OCTAL-LSBF",
    "01234567",
    3, // width
    BitOrder.LeastSignificantBitFirst,
    0) // replacement charCode for encoding of unmapped chars.

object OctalMSBF3BitCharset
  extends NBitsWidth_BitsCharset("X-DFDL-OCTAL-MSBF",
    "01234567",
    3, // width
    BitOrder.MostSignificantBitFirst,
    0) // replacement charCode for encoding of unmapped chars.
