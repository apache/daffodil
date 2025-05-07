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
import scala.io.Source

import org.apache.daffodil.validation.schematron.SchSource.Sch

import net.sf.saxon.TransformerFactoryImpl
import org.junit.Assert.assertNotNull
import org.junit.Test

/**
 * basic function tests adapted from the Camel Schematron implementation
 * https://github.com/apache/camel/blob/main/components/camel-schematron/src/test/java/org/apache/camel/component/schematron
 */
class TestBasicValidation {

  @Test def testValidXML(): Unit = {
    val schFile = "sch/schematron-1.sch"
    val sch = Source.fromResource(schFile).mkString
    val xml = Source.fromResource("xml/article-1.xml").mkString
    val p =
      SchematronValidatorFactory.makeValidator(
        new ByteArrayInputStream(sch.getBytes),
        schFile,
        Sch
      )

    val result = p.validateXML(new ByteArrayInputStream(xml.getBytes))
    assert(result.getErrors.isEmpty)
  }

  @Test def testInvalidXML(): Unit = {
    val schFile = "sch/schematron-1.sch"
    val sch = Source.fromResource(schFile.mkString).mkString
    val xml = Source.fromResource("xml/article-2.xml").mkString
    val p =
      SchematronValidatorFactory.makeValidator(
        new ByteArrayInputStream(sch.getBytes),
        schFile,
        Sch
      )

    val result = p.validateXML(new ByteArrayInputStream(xml.getBytes))
    result.getErrors.forEach(e => println(s"Fail: ${e.getMessage}"))
    assert(result.getErrors.size() == 2)
  }

  @Test def testInvalidXML2(): Unit = {
    val schFile = "sch/schematron-3.sch"
    val sch = Source.fromResource(schFile).mkString
    val xml = Source.fromResource("xml/article-3.xml").mkString
    val p =
      SchematronValidatorFactory.makeValidator(
        new ByteArrayInputStream(sch.getBytes),
        schFile,
        Sch
      )

    val result = p.validateXML(new ByteArrayInputStream(xml.getBytes))
    result.getErrors.forEach(e => println(s"Fail: ${e.getMessage}"))
    assert(result.getErrors.size() == 1)
  }

  @Test def testInstantiateAnInstanceOfTemplates(): Unit = {
    val factory = new TransformerFactoryImpl()
    val schFile = "sch/schematron-1.sch"
    val sch = getClass.getClassLoader.getResourceAsStream(schFile)
    val rules = Transforms.from(sch, schFile, Sch, factory)
    assertNotNull(rules)
  }
}
