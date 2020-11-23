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

import java.io.ByteArrayInputStream

import com.typesafe.config.ConfigFactory
import org.apache.daffodil.validation.Validators
import org.junit.Test

import scala.io.Source

class TestSpiLoading {
  @Test def load(): Unit = {
    val vf = Validators.get(SchematronValidator.name)
    val v = vf.make(ConfigFactory.parseString("""
        |schematron = sch/schematron-2.sch
        |""".stripMargin))
    val xml = Source.fromResource("xml/article-2.xml").mkString

    val result = v.validateXML(new ByteArrayInputStream(xml.getBytes))
    result.errors.forEach(e => println(s"Fail: ${e.getMessage}"))
    assert(result.errors.size() == 2)
  }
}
