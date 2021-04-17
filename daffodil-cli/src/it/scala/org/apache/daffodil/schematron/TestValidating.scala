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

package org.apache.daffodil.schematron

import org.junit.Test

object TestValidating {
  val data = "input/uuid.txt"
  val uuid = "xsd/string.dfdl.xsd"
  val never = "sch/never-fails.sch"
  val always = "sch/always-fails.sch"

  val alwaysResult = regexLine("<.+-fails>2f6481e6-542c-11eb-ae93-0242ac130002</.+-fails>")
}
class TestValidating {
  import TestValidating._

  // always fails sch, but no validate flag so it should pass
  @Test def nonShouldPass(): Unit = withShell() {
    s"parse -s {{$uuid}} {$data}" -> alwaysResult
  }

  // always fails sch, with validate flag should fail
  @Test def failShouldFail(): Unit = withShell(FailureErrorCode) {
    s"parse --validate schematron={{$always}} -s {{$uuid}} {$data}" -> alwaysResult
  }

  // never fails sch, with validate flag should pass
  @Test def passShouldPass(): Unit = withShell() {
    s"parse --validate schematron={{$never}} -s {{$uuid}} {$data}" -> alwaysResult
  }
}
