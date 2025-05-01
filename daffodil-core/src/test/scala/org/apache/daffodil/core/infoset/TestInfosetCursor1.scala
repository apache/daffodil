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

package org.apache.daffodil.core.infoset

// workaround for scala 3 bug where there are false positives with
// unused imports: https://github.com/scala/scala3/issues/22692
import javax.xml.stream._

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

import org.jdom2.input.JDOMParseException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.xml.sax.XMLReader

class TestInfosetInputter1 {

  def infosetInputter(testSchema: scala.xml.Node, is: java.io.InputStream) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val rootERD = u.ssrd.elementRuntimeData
    val ic = new InfosetInputter(new XMLTextInfosetInputter(is))
    ic.initialize(rootERD, u.tunables)
    ic
  }

  @Test def testInfosetInputterOnBadData(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
    )

    val str = "this is not XML"
    val is = new java.io.ByteArrayInputStream(str.getBytes("UTF-8"))
    val ic = infosetInputter(sch, is)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("prolog")) // content not allowed in prolog of an XML document.
  }

  @Test def testInfosetInputterOnBadData2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
    )

    val str = """<this pretends to be xml"""
    val is = new java.io.ByteArrayInputStream(str.getBytes("UTF-8"))
    val ic = infosetInputter(sch, is)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get

    assertTrue(msg.contains("Unexpected character")) // expects an equal sign for an attribute
  }

  @Test def testInfosetInputterOnBadData3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
    )

    val str = "\u0000\u0000\uFFFF\uFFFF"
    val is = new java.io.ByteArrayInputStream(str.getBytes("UTF-8"))
    val ic = infosetInputter(sch, is)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("Illegal content")) // content not allowed in prolog.
    assertTrue(msg.contains("Invalid UTF-8 character"))
  }

  private val rootFoo = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"/>,
    <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
  )

  /**
   * This test characterizes the behavior of the Woodstox XML library with respect to
   * DOCTYPE declarations (aka DTDs), and shows that the XMLTextInfosetInputter
   * detects and disallows them.
   */
  @Test def testXMLTextInfosetInputterDisallowsDocType(): Unit = {

    //
    // First we show that woodstox can read with a DTD present.
    // This will end with an error about the DTD, showing that Woodstox is
    // recognizing and processing the DTD.
    //
    var is = Misc.getRequiredResource("test/xmlDocWithBadDTD.xml").toURL.openStream()
    var fact = new com.ctc.wstx.stax.WstxInputFactory()
    fact.setProperty(XMLInputFactory.IS_COALESCING, true)
    fact.setEventAllocator(com.ctc.wstx.evt.DefaultEventAllocator.getDefaultInstance)

    // Woodstox has it's own properties for controlling DTDs
    // Allow DTD here, so we have a positive example of woodstock allowing
    // them, so we can contrast to when we disable them later.
    //
    fact.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, true)
    fact.setProperty(XMLInputFactory.SUPPORT_DTD, true)
    fact.setProperty(XMLInputFactory.IS_VALIDATING, true) // validate with DTD

    var rdr = fact.createXMLStreamReader(is)
    assertTrue(rdr.hasNext)
    var ev = rdr.next()
    assertEquals(XMLStreamConstants.COMMENT, ev)
    ev = rdr.next()
    assertEquals(XMLStreamConstants.COMMENT, ev)
    ev = rdr.next()
    assertEquals(XMLStreamConstants.DTD, ev)
    val woodstoxErr = intercept[com.ctc.wstx.exc.WstxParsingException] {
      rdr.next()
    }
    val woodstoxMsg = woodstoxErr.getMessage()
    // This proves woodstox was trying to process the DTD.
    assertTrue(woodstoxMsg.contains("notFound.dtd"))

    //
    // Now we show how to configure Woostox so the same
    // Document with DTD is rejected.
    //
    // Our first try, expecting Woodstox to throw an error itself, doesn't
    // work. Woodstock can ignore the DTD, but still traverses it
    // creating a DTD StAX event.
    //
    fact = new com.ctc.wstx.stax.WstxInputFactory()
    fact.setProperty(XMLInputFactory.IS_COALESCING, true)
    fact.setEventAllocator(com.ctc.wstx.evt.DefaultEventAllocator.getDefaultInstance)

    // Woodstox has it's own properties for turning off DTDs to provide
    // secure XML processing.
    fact.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
    fact.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    fact.setProperty(XMLInputFactory.IS_VALIDATING, false)

    is = Misc.getRequiredResource("test/xmlDocWithBadDTD.xml").toURL.openStream()
    rdr = fact.createXMLStreamReader(is)
    assertTrue(rdr.hasNext)
    ev = rdr.next()
    assertEquals(XMLStreamConstants.COMMENT, ev)
    ev = rdr.next()
    assertEquals(XMLStreamConstants.COMMENT, ev)
    ev = rdr.next()
    assertEquals(XMLStreamConstants.DTD, ev) // Aha. It still allows the DTD.
    // This DTD is bad, so if it was being processed, we would fail here.
    ev = rdr.next()
    // but we don't fail, we skip past it and encounter the root element
    // following.
    assertEquals(XMLStreamConstants.START_ELEMENT, ev)
    assertEquals("root", rdr.getLocalName)
    //
    // That above shows that regardless of turning off DTDs, we're still
    // tolerating them, and have to explicitly detect them in the
    // XMLTextInfosetInputter
    //
    // So here we show that the XMLTextInfosetInputter is explicitly
    // detecting the DTD event, and issuing a diagnostic message
    // specifically about DTD/DOCTYPE not being supported.
    //

    is = Misc.getRequiredResource("test/xmlDocWithBadDTD.xml").toURL.openStream()
    val ic = infosetInputter(rootFoo, is)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("Illegal content"))
    assertTrue(msg.contains("DOCTYPE"))
  }

  @Test
  def test_JDOMDisallowsDocType(): Unit = {

    val is = Misc.getRequiredResource("test/xmlDocWithBadDTD.xml").toURL.openStream()
    val builder = new org.jdom2.input.SAXBuilder() {
      override protected def createParser(): XMLReader = {
        val rdr = super.createParser()
        XMLUtils.setSecureDefaults(rdr)
        rdr
      }
    }
    val exc = intercept[JDOMParseException] {
      builder.build(is)
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("DOCTYPE is disallowed"))
  }
}
