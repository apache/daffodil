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

import java.nio.channels.Channels

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.iapi.DFDL.ParseResult
import org.apache.daffodil.runtime1.iapi.DFDL.UnparseResult
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.unparsers.UStateMain

import org.apache.commons.io.output.NullOutputStream
import org.junit.Assert._
import org.junit.Test

object TestInfosetFree {

  /**
   * Compiles an infoset with infoset releasing disabled. By disabling the
   * releasing, the Scala infoset is marked with attributes signifying which
   * elements *would* have been freed if we didn't disable it. This allows for
   * a reliable way to validate which elements were freed.
   *
   * This first parses and unparses data, without freeing any infoset
   * nodes--only marking as described above. It then walks both infosets,
   * configured to add information to the infoset about which elements were
   * freed. We validate that both the parse and unparse results are the
   * same--any infoset elements freed during a parse should also be freed
   * during unparse. We then return the infoset for the test to compare against
   * the expected value.
   */
  def test(schema: scala.xml.Elem, bytes: Array[Byte]): scala.xml.Node = {

    val compiler = Compiler()
      .withTunable("releaseUnneededInfoset", "false")

    val pf = compiler.compileNode(schema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("pf compile errors: " + msgs)
    }
    val dp = pf.onPath("/")
    if (dp.isError) {
      val msgs = dp.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("dp compile errors: " + msgs)
    }

    // Parse the data, note that we do not set showFreedInfo here because
    // DINodes aren't freed until *after* the infoset walker walks them. So the
    // maybeFreed state hasn't been set yet when the infoset outputter gets the
    // events. We must walk the infoset again after the parse is complete
    val parseInput = InputSourceDataInputStream(bytes)
    val parseOutputter = new ScalaXMLInfosetOutputter()
    val parseResult = dp.parse(parseInput, parseOutputter).asInstanceOf[ParseResult]
    if (parseResult.isError) {
      val msgs = parseResult.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("parse errors: " + msgs)
    }

    val unparseInputter = new ScalaXMLInfosetInputter(parseOutputter.getResult())
    val unparseOutput = Channels.newChannel(NullOutputStream.INSTANCE)
    val unparseResult = dp.unparse(unparseInputter, unparseOutput).asInstanceOf[UnparseResult]
    if (unparseResult.isError) {
      val msgs = unparseResult.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("unparse errors: " + msgs)
    }

    // now walk the parse and unparse infosets and convert them to Scala XML
    // with the showFreedInfoset set

    def docToXML(doc: DIDocument): scala.xml.Node = {
      val detailedOutputter =
        new ScalaXMLInfosetOutputter(showFreedInfo = true)

      val infosetWalker = InfosetWalker(
        doc,
        new InfosetOutputter(detailedOutputter),
        walkHidden = true, // let's ensure any hidden elements are free
        ignoreBlocks = true, // there should be no blocks, but ignore them just to be sure
        releaseUnneededInfoset = false
      ) // do not free the infoset
      infosetWalker.walk(lastWalk = true)

      detailedOutputter.getResult()
    }

    val parseDoc = parseResult.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument]
    val unparseDoc = unparseResult.resultState.asInstanceOf[UStateMain].documentElement

    val parseXML = docToXML(parseDoc)
    val unparseXML = docToXML(unparseDoc)

    if (parseXML.toString != unparseXML.toString) {
      fail("parse and unparse XML did not match, infoset not freed the same")
    }

    parseXML
  }
}

class TestInfosetFree {

  @Test def testInfosetFree1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />,
      <dfdl:format ref="tns:GeneralFormat" />,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e" type="xs:int" maxOccurs="unbounded"
              dfdl:length="1" dfdl:lengthKind="explicit" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val actualXML = TestInfosetFree.test(testSchema, "123".getBytes)

    // all elements freed, including array elements and the array itself
    val expectedXML =
      <root freed="self" xmlns="http://example.com">
        <e freed="self+array">1</e>
        <e freed="self+array">2</e>
        <e freed="self+array">3</e>
      </root>

    assertEquals(scala.xml.Utility.trim(expectedXML).toString, actualXML.toString)
  }

  @Test def testInfosetFree2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />,
      <dfdl:format ref="tns:GeneralFormat" />,
      <xs:group name="hidden">
        <xs:sequence>
          <xs:element name="e" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"
            dfdl:outputValueCalc="{ 1 }" />
        </xs:sequence>
      </xs:group>
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:hiddenGroupRef="tns:hidden" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val actualXML = TestInfosetFree.test(testSchema, "1".getBytes)

    // all elements freed, including hidden elements
    val expectedXML =
      <root freed="self" xmlns="http://example.com">
        <e freed="self">1</e>
      </root>

    assertEquals(scala.xml.Utility.trim(expectedXML).toString, actualXML.toString)
  }

  @Test def testInfosetFree3(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />,
      <dfdl:format ref="tns:GeneralFormat" />,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="fieldLen" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1" />
            <xs:element name="field" type="xs:int" maxOccurs="unbounded"
              dfdl:lengthKind="explicit" dfdl:length="{ ../tns:fieldLen }" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val actualXML = TestInfosetFree.test(testSchema, "1123".getBytes)

    // all elements freed, execpted for fieldLen because it is used in an expression
    val expectedXML =
      <root freed="self" xmlns="http://example.com">
        <fieldLen>1</fieldLen>
        <field freed="self+array">1</field>
        <field freed="self+array">2</field>
        <field freed="self+array">3</field>
      </root>

    assertEquals(scala.xml.Utility.trim(expectedXML).toString, actualXML.toString)
  }

  @Test def testInfosetFree4(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />,
      <dfdl:format ref="tns:GeneralFormat" />,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="fieldCount" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"
              dfdl:outputValueCalc="{ fn:count(../tns:field) }" />
            <xs:element name="field" type="xs:int" maxOccurs="unbounded"
              dfdl:occursCountKind="expression" dfdl:occursCount="{ ../tns:fieldCount }"
              dfdl:lengthKind="explicit" dfdl:length="1" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val actualXML = TestInfosetFree.test(testSchema, "3123".getBytes)

    // Most elements are not freed. Both fieldCount and field are used in an
    // expression. Even though only the count of the field array is needed, and
    // we could theoretically free the field elements but not the array, they
    // are not currently freed. Our usedInAnExpression logic is not yet
    // sophisticated enough to differentiate between how elements are used in
    // an expression and how that affect releasability
    val expectedXML =
      <root freed="self" xmlns="http://example.com">
        <fieldCount>3</fieldCount>
        <field>1</field>
        <field>2</field>
        <field>3</field>
      </root>

    assertEquals(scala.xml.Utility.trim(expectedXML).toString, actualXML.toString)
  }

  @Test def testInfosetFree5(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />,
      <dfdl:format ref="tns:GeneralFormat" />,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="fieldCount" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"
              dfdl:outputValueCalc="{ fn:count(../tns:fieldLen) }" />
            <xs:element name="fieldLen" type="xs:int" maxOccurs="unbounded"
              dfdl:occursCountKind="expression" dfdl:occursCount="{ ../tns:fieldCount }"
              dfdl:lengthKind="explicit" dfdl:length="1" />
            <xs:element name="field" type="xs:int" maxOccurs="unbounded"
              dfdl:occursCountKind="expression" dfdl:occursCount="{ ../tns:fieldCount }"
              dfdl:lengthKind="explicit" dfdl:length="{ ../tns:fieldLen[dfdl:occursIndex()] }" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val actualXML = TestInfosetFree.test(testSchema, "3123122333".getBytes)

    // fieldCount and fieldLen are not freed, both are used in expressions.
    // Individual field's are not use in expressions, so we can free them. Note
    // that fieldLen does not have an outputValueCalc, which in practice one
    // might use so that the fieldLen values match the field lengths on
    // unparse. This shows that if one were to exclude some outputValueCalc
    // elements, more elements can be freed and streamabilty can be improved.
    val expectedXML =
      <root freed="self" xmlns="http://example.com">
        <fieldCount>3</fieldCount>
        <fieldLen>1</fieldLen>
        <fieldLen>2</fieldLen>
        <fieldLen>3</fieldLen>
        <field freed="self+array">1</field>
        <field freed="self+array">22</field>
        <field freed="self+array">333</field>
      </root>

    assertEquals(scala.xml.Utility.trim(expectedXML).toString, actualXML.toString)
  }

}
