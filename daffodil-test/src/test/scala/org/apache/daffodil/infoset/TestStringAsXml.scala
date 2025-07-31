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

package org.apache.daffodil.runtime1.infoset
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.iapi.URISchemaSource
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL.DataProcessor

import org.apache.commons.io.IOUtils
import org.junit.Assert._
import org.junit.Test

class TestStringAsXml {

  private def compileSchema(dfdlSchemaURI: URI, validatingSchema: URI = null) = {
    val c = Compiler()
    val pf = c.compileSource(
      URISchemaSource(Misc.uriToDiagnosticFile(dfdlSchemaURI), dfdlSchemaURI)
    )
    val dp = pf.onPath("/")
    val schema = if (validatingSchema != null) validatingSchema else dfdlSchemaURI
    dp.withValidation("xerces", schema)

  }

  private def doParse(dp: DataProcessor, data: InputStream) = {
    val parseIn = InputSourceDataInputStream(data)
    val parseBos = new ByteArrayOutputStream()
    val parseOut = new XMLTextInfosetOutputter(parseBos, pretty = true)
    val parseRes = dp.parse(parseIn, parseOut)
    val parseDiags = parseRes.getDiagnostics.asScala.map(_.toString)
    val parseStrOpt = if (parseRes.isProcessingError) None else Some(parseBos.toString("UTF-8"))
    (parseDiags, parseStrOpt)
  }

  private def doUnparse(dp: DataProcessor, infoset: InputStream) = {
    val unparseIn = new XMLTextInfosetInputter(infoset)
    val unparseBos = new ByteArrayOutputStream()
    val unparseOut = java.nio.channels.Channels.newChannel(unparseBos)
    val unparseRes = dp.unparse(unparseIn, unparseOut)
    val unparseDiags = unparseRes.getDiagnostics.asScala.map(_.toString)
    val unparseStrOpt =
      if (unparseRes.isProcessingError) None else Some(unparseBos.toString("UTF-8"))
    (unparseDiags, unparseStrOpt)
  }

  private val parsingSchema: URI = Misc.getRequiredResource(
    "/org/apache/daffodil/infoset/stringAsXml/namespaced/xsd/binMessage.dfdl.xsd"
  )

  @Test def test_stringAsXml_01_a(): Unit = {
    // validate the infoset using the handwritten WithPayload schema
    val validatingSchema =
      Misc.getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/xsd/binMessageWithXmlPayload.xsd"
      )
    val dp = compileSchema(
      parsingSchema,
      validatingSchema
    ).asInstanceOf[DataProcessor]
    val parseData = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat"
      )
      .toURL
      .openStream
    val (parseDiags, Some(parseInfosetActual)) = doParse(dp, parseData) match {
      case (parseDiags, Some(parseInfosetActual)) => (parseDiags, Some(parseInfosetActual))
      case _ => fail(); null
    }
    val parseInfosetExpected = {
      val is = Misc
        .getRequiredResource(
          "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml"
        )
        .toURL
        .openStream
      IOUtils.toString(is, StandardCharsets.UTF_8)
    }
    // diagnostic from full validation
    assertTrue(parseDiags.exists(_.contains("Value '=invalid field' is not facet-valid")))
    // we still get the expected infoset, replace CRLF with LF because of git windows autocrlf
    assertEquals(
      parseInfosetExpected.replace("\r\n", "\n"),
      parseInfosetActual.replace("\r\n", "\n")
    )
  }

  @Test def test_stringAsXml_01_b(): Unit = {
    val dp = compileSchema(
      parsingSchema,
      parsingSchema
    ).asInstanceOf[DataProcessor]
    val parseData = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat"
      )
      .toURL
      .openStream
    val (parseDiags, Some(parseInfosetActual)) = doParse(dp, parseData) match {
      case (parseDiags, Some(parseInfosetActual)) => (parseDiags, Some(parseInfosetActual))
      case _ => fail(); null
    }
    val parseInfosetExpected = {
      val is = Misc
        .getRequiredResource(
          "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml"
        )
        .toURL
        .openStream
      IOUtils.toString(is, StandardCharsets.UTF_8)
    }
    // diagnostic from full validation
    assertTrue(parseDiags.exists(_.contains("Element 'xmlStr' is a simple type")))
    // we still get the expected infoset, replace CRLF with LF because of git windows autocrlf
    assertEquals(
      parseInfosetExpected.replace("\r\n", "\n"),
      parseInfosetActual.replace("\r\n", "\n")
    )
  }

  @Test def test_stringAsXml_02(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val unparseInfoset = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml"
      )
      .toURL
      .openStream
    val unparseDataActual = doUnparse(dp, unparseInfoset) match {
      case (_, Some(unparseDataActual)) => unparseDataActual
      case _ => fail(); null
    }
    val unparseDataExpected = {
      val is = Misc
        .getRequiredResource(
          "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml.dat"
        )
        .toURL
        .openStream
      IOUtils.toString(is, StandardCharsets.UTF_8)
    }
    assertEquals(unparseDataExpected, unparseDataActual)
  }

  @Test def test_stringAsXml_03(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val infoset1 = {
      val is = Misc
        .getRequiredResource(
          "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_02.xml"
        )
        .toURL
        .openStream
      IOUtils.toString(is, StandardCharsets.UTF_8)
    }
    val data1 =
      doUnparse(dp, new ByteArrayInputStream(infoset1.getBytes(StandardCharsets.UTF_8))) match {
        case (_, Some(data1)) => data1
        case _ => fail(); null
      }
    val infoset2 =
      doParse(dp, new ByteArrayInputStream(data1.getBytes(StandardCharsets.UTF_8))) match {
        case (_, Some(infoset2)) => infoset2
        case _ => fail(); null
      }
    val data2 =
      doUnparse(dp, new ByteArrayInputStream(infoset2.getBytes(StandardCharsets.UTF_8))) match {
        case (_, Some(data2)) => data2
        case _ => fail(); null
      }
    // unparsing canonicalizes the XML infoset payload. The original infoset is
    // not canoncialized, so the parsed infoset should not match the original.
    // But both unprsed data should match because the first unparse
    // canonicailzed the XML, and the second unparse should have made not
    // additional changes since it's already canonicalized
    assertNotEquals(infoset1, infoset2)
    assertEquals(data1, data2)
  }

  @Test def test_stringAsXml_04(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val parseData = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_03.dat"
      )
      .toURL
      .openStream
    val (parseDiags, _) = doParse(dp, parseData)
    assertTrue(parseDiags.find(_.contains("Unexpected character")).isDefined)
  }

  @Test def test_stringAsXml_05(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val unparseInfoset = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_04.xml"
      )
      .toURL
      .openStream
    val (unparseDiags, _) = doUnparse(dp, unparseInfoset)
    assertTrue(unparseDiags.find(_.contains("Unexpected character")).isDefined)
  }

  @Test def test_stringAsXml_06(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val unparseInfoset = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_05.xml"
      )
      .toURL
      .openStream
    val (unparseDiags, _) = doUnparse(dp, unparseInfoset)
    assertTrue(unparseDiags.find(_.contains("Expected start of stringAsXml")).isDefined)
  }

  @Test def test_stringAsXml_07(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val unparseInfoset = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_06.xml"
      )
      .toURL
      .openStream
    val (unparseDiags, _) = doUnparse(dp, unparseInfoset)
    assertTrue(
      unparseDiags
        .find(_.contains("Expected end of element following end of stringAsXml"))
        .isDefined
    )
  }

  @Test def test_stringAsXml_08(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val unparseInfoset = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_07.xml"
      )
      .toURL
      .openStream
    val (unparseDiags, _) = doUnparse(dp, unparseInfoset)
    assertTrue(unparseDiags.find(_.contains("Illegal content for simple element")).isDefined)
  }

  @Test def test_stringAsXml_09(): Unit = {
    val dp = compileSchema(parsingSchema).asInstanceOf[DataProcessor]
    val unparseInfoset = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_08.dat"
      )
      .toURL
      .openStream
    val (unparseDiags, _) = doParse(dp, unparseInfoset)
    assertTrue(unparseDiags.find(_.contains("Undeclared general entity \"name\"")).isDefined)
  }

  @Test def test_stringAsXml_10(): Unit = {
    val parsingSchema = Misc.getRequiredResource(
      "/org/apache/daffodil/infoset/stringAsXml/nonamespace/xsd/binMessage.dfdl.xsd"
    )
    // validate the infoset using the handwritten WithPayload schema
    val validatingSchema = Misc.getRequiredResource(
      "/org/apache/daffodil/infoset/stringAsXml/nonamespace/xsd/binMessageWithXmlPayload.xsd"
    )

    val dp = compileSchema(
      parsingSchema,
      validatingSchema
    ).asInstanceOf[DataProcessor]
    val parseData = Misc
      .getRequiredResource(
        "/org/apache/daffodil/infoset/stringAsXml/nonamespace/binMessage_01.dat"
      )
      .toURL
      .openStream
    val (parseDiags, Some(parseInfosetActual)) = doParse(dp, parseData) match {
      case (parseDiags, Some(parseInfosetActual)) => (parseDiags, Some(parseInfosetActual))
      case _ => fail(); null
    }
    val parseInfosetExpected = {
      val is = Misc
        .getRequiredResource(
          "/org/apache/daffodil/infoset/stringAsXml/nonamespace/binMessage_01.dat.xml"
        )
        .toURL
        .openStream
      IOUtils.toString(is, StandardCharsets.UTF_8)
    }
    // diagnostic from full validation
    assertTrue(
      parseDiags.exists(_.contains("Value '=invalid field' is not facet-valid"))
    )
    // we still get the expected infoset, use compareAndReport so prefix differences
    // don't matter
    XMLUtils.compareAndReport(
      scala.xml.XML.loadString(parseInfosetExpected),
      scala.xml.XML.loadString(parseInfosetActual)
    )
  }

}
