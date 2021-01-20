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

import net.sf.expectit.matcher.Matchers.sequence
import org.junit.Test

import java.util.UUID

class TestEmbedded {
  @Test def alwaysFails(): Unit = withShell(FailureErrorCode) {
    val data = mktmp(UUID.randomUUID.toString)
    val schema = "xsd/always-fails-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> lineEndsWith("</always-fails>")
  }

  @Test def unitPriceWithoutValidation(): Unit = withShell() {
    val data = mktmp("widget,monday,1,$5.00,$5.00")
    val schema = "xsd/unit_price.dfdl.xsd"
    s"parse -r list -s {{$schema}} $data" -> lineEndsWith("</ex:list>")
  }

  @Test def unitPriceWithValidation(): Unit = withShell(FailureErrorCode, JoinStdError) {
    val data = mktmp("widget,monday,1,$5.00,$6.00")
    val schema = "xsd/unit_price.dfdl.xsd"
    s"parse -r list --validate schematron={{$schema}} -s {{$schema}} $data" -> sequence(
      lineEndsWith("</ex:list>"),
      anyLines(3)
    )
  }

  @Test def unitPriceWithValidationCheckMessage(): Unit = withShell(FailureErrorCode, JoinStdError) {
    val data = mktmp("widget,monday,5,$5.00,$25.00||gadget,tuesday,1,$10.00,$11.00")
    val schema = "xsd/unit_price.dfdl.xsd"
    s"parse -r list --validate schematron={{$schema}} -s {{$schema}} $data" -> sequence(
      lineEndsWith("</ex:list>"),
      lineEndsWith("[error] Validation Error: wrong unit price for gadget, tuesday"),
      anyLines(2)
    )
  }

  @Test def extends1(): Unit = withShell() {
    val data = mktmp("bob;l;smith")
    val schema = "xsd/extends-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> lineEndsWith("</name>")
  }

  @Test def extends2(): Unit = withShell() {
    val data = mktmp("ob;;smith")
    val schema = "xsd/extends-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> lineEndsWith("</name>")
  }

  @Test def extends3(): Unit = withShell(FailureErrorCode, JoinStdError) {
    val data = mktmp(";;smith")
    val schema = "xsd/extends-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> validationError("first is blank")
  }

  @Test def extends4(): Unit = withShell(FailureErrorCode, JoinStdError) {
    val data = mktmp("bob;l;")
    val schema = "xsd/extends-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> validationError("last is blank")
  }

  @Test def extends5(): Unit = withShell(FailureErrorCode, JoinStdError) {
    val data = mktmp(";l;")
    val schema = "xsd/extends-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> sequence(
      validationError("last is blank"),
      validationError("first is blank")
    )
  }

  @Test def testWithNs1(): Unit = withShell() {
    val data = mktmp("0;1")
    val schema = "xsd/with-ns-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> lineEndsWith("</myns:interval>")
  }

  @Test def testWithNs2(): Unit = withShell(FailureErrorCode, JoinStdError) {
    val data = mktmp("2;1")
    val schema = "xsd/with-ns-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> validationError()
  }

  @Test def testWithNs3(): Unit = withShell(FailureErrorCode, JoinStdError) {
    val data = mktmp("0;0")
    val schema = "xsd/with-ns-1.dfdl.xsd"
    s"parse --validate schematron={{$schema}} -s {{$schema}} $data" -> validationError()
  }
}
