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

package org.apache.daffodil.lib

import java.io.FileNotFoundException

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions._

import org.junit.Assert._
import org.junit.Test

class TestImplicits {

  @Test
  def testIntercept1(): Unit = {
    intercept[Abort] {
      Assert.abort("yadda")
    }
  }

  @Test
  def testIntercept2(): Unit = {
    val e = intercept[InterceptFailedException] {
      intercept[Abort] {
        // this will not cause an abort exception
        2 + 2
      }
    }
    assertTrue(e.getMessage().contains("Failed to intercept"))
  }

  @Test
  def testIntercept3(): Unit = {
    val e = intercept[InterceptFailedException] {
      intercept[FileNotFoundException] {
        // an exception is caught, but not the right kind
        throw new Exception("yadda")
      }
    }
    assertTrue(e.getMessage().contains("Expected 'java.io.FileNotFoundException'"))
  }

}
