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
import net.sf.saxon.TransformerFactoryImpl
import org.apache.daffodil.validation.schematron.SchSource.Sch
import org.junit.Assert.assertNotNull
import org.junit.Test

import javax.xml.transform.URIResolver
import javax.xml.transform.stream.StreamSource
import scala.io.Source


/**
 * basic function tests adapted from the Camel Schematron implementation
 * https://github.com/apache/camel/blob/main/components/camel-schematron/src/test/java/org/apache/camel/component/schematron
 */
class TestBasicValidation {

  @Test def testValidXML(): Unit = {
    val sch = Source.fromResource("sch/schematron-1.sch").mkString
    val xml = Source.fromResource("xml/article-1.xml").mkString
    val p = SchematronValidatorFactory.makeValidator(new ByteArrayInputStream(sch.getBytes), Sch)

    val result = p.validateXML(new ByteArrayInputStream(xml.getBytes))
    assert(result.errors.isEmpty)
  }

  @Test def testInvalidXML(): Unit = {
    val sch = Source.fromResource("sch/schematron-2.sch").mkString
    val xml = Source.fromResource("xml/article-2.xml").mkString
    val p = SchematronValidatorFactory.makeValidator(new ByteArrayInputStream(sch.getBytes), Sch)

    val result = p.validateXML(new ByteArrayInputStream(xml.getBytes))
    result.errors.forEach(e => println(s"Fail: ${e.getMessage}"))
    assert(result.errors.size() == 2)
  }

  @Test def testInvalidXML2(): Unit = {
    val sch = Source.fromResource("sch/schematron-3.sch").mkString
    val xml = Source.fromResource("xml/article-3.xml").mkString
    val p = SchematronValidatorFactory.makeValidator(new ByteArrayInputStream(sch.getBytes), Sch, Some(new CustomResolver))

    val result = p.validateXML(new ByteArrayInputStream(xml.getBytes))
    result.errors.forEach(e => println(s"Fail: ${e.getMessage}"))
    assert(result.errors.size() == 1)
  }

  @Test def testInstantiateAnInstanceOfTemplates(): Unit = {
    val resolver = new ClassPathUriResolver(Schematron.templatesRootDir, None)
    val factory = new TransformerFactoryImpl()
    factory.setURIResolver(resolver)
    val sch = getClass.getClassLoader.getResourceAsStream("sch/schematron-1.sch")
    val rules = Transforms.from(sch, Sch, factory)
    assertNotNull(rules)
  }

  class CustomResolver extends URIResolver {
    override def resolve(href: String, base: String) = new StreamSource(
      getClass.getClassLoader.getResourceAsStream(s"custom-resolver/$href"))
  }
}
