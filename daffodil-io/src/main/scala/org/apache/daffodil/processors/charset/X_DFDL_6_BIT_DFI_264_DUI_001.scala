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

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.BitOrder

/**
 * Some encodings are not byte-oriented.
 *
 * X-DFDL-6-BIT-DFI-264-DUI-001
 *
 */

object X_DFDL_6_BIT_DFI_264_DUI_001
  extends NBitsWidth_BitsCharset("X-DFDL-6-BIT-DFI-264-DUI-001",
    " 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD0",
    6, // width
    BitOrder.LeastSignificantBitFirst,
    0) {

  override def checks() = {
    super.checks()
    Assert.invariant(this.charToCode('0').get == 63)
    Assert.invariant(this.charToCode('1').get == 1)
    Assert.invariant(this.charToCode(' ').get == 0) // space
    Assert.invariant(this.charToCode('a').isEmpty) // unmapped
  }
}
