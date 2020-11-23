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

package org.apache.daffodil

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class TestValidatorPatterns {
  @Test def testNoArgsPattern(): Unit = {
    val vname = "vvv"
    s"$vname" match {
      case ValidatorPatterns.DefaultArgPattern(_, _) =>
        fail("should not have matched")
      case ValidatorPatterns.NoArgsPattern(v) =>
        assertEquals(vname, v)
      case _ =>
        fail("did not match")
    }
  }

  @Test def testDefaultArgsPattern(): Unit = {
    val vname = "vvv"
    val varg = "some_default_argument_string"
    s"$vname=$varg" match {
      case ValidatorPatterns.DefaultArgPattern(v, arg) =>
        assertEquals(vname, v)
        assertEquals(varg, arg)
      case ValidatorPatterns.NoArgsPattern(_) =>
        fail("should not have matched")
      case _ =>
        fail("did not match")
    }
  }
}
