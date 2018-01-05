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
 * A 1-bit wide encoding so you can get down to the bits level
 * but use anything you'd normally use with text, such as regular expressions
 * initiators/terminators, etc.
 */

object X_DFDL_BITS_LSBF
  extends NBitsWidth_BitsCharset("X-DFDL-BITS-LSBF",
    "01",
    1,
    BitOrder.LeastSignificantBitFirst,
    0)

object X_DFDL_BITS_MSBF
  extends NBitsWidth_BitsCharset("X-DFDL-BITS-MSBF",
    "01",
    1,
    BitOrder.MostSignificantBitFirst,
    0)
