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

import java.nio.file.Paths
import java.util.Properties

import org.apache.daffodil.api
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.util.Misc

import org.junit.Test

class TestValidatorFactory {
  @Test def testMakeFactory(): Unit = {
    SchematronValidatorFactory.makeValidator({
      val props = new Properties()
      val uri = Misc.getRequiredResource("sch/schematron-1.sch")
      props.setProperty("schematron", uri.toString)
      props
    })
  }

  @Test def testSchNotFound(): Unit = {
    intercept[api.validation.ValidatorInitializationException] {
      SchematronValidatorFactory.makeValidator({
        val props = new Properties()
        val uri = Paths.get("sch/schematron-xxx.sch").toUri
        props.setProperty("schematron", uri.toString)
        props
      })
    }
  }
}
