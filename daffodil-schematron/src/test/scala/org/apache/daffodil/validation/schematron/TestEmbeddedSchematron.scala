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

package org.apache.daffodil.validation.schematron

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import java.util.UUID

class TestEmbeddedSchematron extends EmbeddedTesting {
  @Test def variation1(): Unit = withSchema("xsd/embedded-1.dfdl.xsd") { f =>
    assertTrue(f.parse(UUID.randomUUID.toString).validated)
    assertFalse(f.parse(UUID.randomUUID.toString.drop(1)).validated)
  }

  @Test def variation2(): Unit = withSchema("xsd/embedded-2.dfdl.xsd") { f =>
    assertTrue(f.parse(UUID.randomUUID.toString).validated)
    assertFalse(f.parse(UUID.randomUUID.toString.drop(1)).validated)
  }

  @Test def variation3(): Unit = withSchema("xsd/embedded-3.dfdl.xsd") { f =>
    f.parse(UUID.randomUUID.toString).diagnostics.foreach(println)
    assertTrue(f.parse(UUID.randomUUID.toString).validated)
    f.parse(UUID.randomUUID.toString).diagnostics.foreach(println)
    assertFalse(f.parse(UUID.randomUUID.toString.drop(1)).validated)
  }

  @Test def testNeverFails(): Unit = withSchema("xsd/never-fails-1.dfdl.xsd") { f =>
    val good = UUID.randomUUID.toString
    assertTrue(f.parse(good).validated)
  }

  @Test def testAlwaysFails(): Unit = withSchema("xsd/always-fails-1.dfdl.xsd") { f =>
    val good = UUID.randomUUID.toString
    assertFalse(f.parse(good).validated)
  }

  @Test def testExtends(): Unit = withSchema("xsd/extends-1.dfdl.xsd") { f =>
    assertTrue(f.parse("bob;l;smith").validated)
    assertTrue(f.parse("bob;;smith").validated)
    assertFalse(f.parse(";;smith").validated)
    assertFalse(f.parse("bob;l;").validated)
    assertFalse(f.parse(";l;").validated)
  }

  @Test def testNoNs1(): Unit = withSchema("xsd/without-ns-1.dfdl.xsd") { f =>
    assertTrue(f.parse("0;1", Always).validated)
    assertFalse(f.parse("2;1").validated)
    assertFalse(f.parse("0;0").validated)
  }

  @Test def testWithNs1(): Unit = withSchema("xsd/with-ns-1.dfdl.xsd") { f =>
    assertTrue(f.parse("0;1", Always).validated)
    assertFalse(f.parse("2;1").validated)
    assertFalse(f.parse("0;0").validated)
  }
}
