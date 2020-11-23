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

package org.apache.daffodil.validation

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class TestValidatorsSPI {
  val schema = getClass.getResource("/test/validation/testSchema1.dfdl.xsd").toURI.toString
  val infoset = getClass.getResourceAsStream("/test/validation/testData1Infoset.xml")

  @Test def testValidatorGetNotFoundThrows(): Unit = {
    assertThrows(classOf[ValidatorNotRegisteredException], () => Validators.get("dont exist"))
  }

  @Test def testValidatorFindNotFoundNone(): Unit = {
    assertTrue(Validators.find("dont exist").isEmpty)
  }

  @Test def testValidatorNonExists(): Unit = {
    assertFalse(Validators.isRegistered("dont exist"))
  }

  @Test def testDefaultIsRegistered(): Unit = {
    val defaultName = XercesValidator.name
    val defaultF = Validators.get(defaultName)

    assertEquals(defaultName, defaultF.name())
    assertTrue(Validators.find(defaultName).nonEmpty)
    assertTrue(Validators.isRegistered(defaultName))
  }

  @Test def testPassingValidator(): Unit = {
    val f = Validators.get(PassingValidator.name)
    val v = f.make(XercesValidatorFactory.makeConfig(Seq(schema)))
    val r = v.validateXML(infoset)

    assertTrue(r.warnings.isEmpty)
    assertTrue(r.errors.isEmpty)
  }

  @Test def testFailingValidator(): Unit = {
    val f = Validators.get(FailingValidator.name)
    val v = f.make(XercesValidatorFactory.makeConfig(Seq(schema)))
    val r = v.validateXML(infoset)

    assertTrue(r.warnings.isEmpty)
    assertFalse(r.errors.isEmpty)

    assertEquals(r.errors.iterator().next().getMessage, "boom")
  }
}
