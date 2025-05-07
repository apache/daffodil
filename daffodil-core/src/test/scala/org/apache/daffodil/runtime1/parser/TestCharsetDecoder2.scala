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

package org.apache.daffodil.runtime1.parser

import org.apache.daffodil.io.processors.charset.CharsetUtils

import org.junit.Assert._
import org.junit.Test

class TestCharsetDecoder2 {

  @Test def testIfHasJava7DecoderBug(): Unit = {
    if (CharsetUtils.hasJava7DecoderBug)
      fail("Java 7 Decoder bug detected. Daffodil requires Java 8 (or higher)")
  }
}
