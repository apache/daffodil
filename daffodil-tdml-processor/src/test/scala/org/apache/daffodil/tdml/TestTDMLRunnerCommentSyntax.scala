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

package org.apache.daffodil.tdml

import org.apache.daffodil.xml.XMLUtils
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLRunnerCommentSyntax {
  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  @Test def testCommentsInScalaLiteralXML() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <!-- comment -->
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <!-- comment -->
          <dfdl:format ref="tns:GeneralFormat" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <!-- comment -->
          <xsd:element name="array" type="tns:arrayType" dfdl:lengthKind="implicit"/>
          <!-- comment -->
          <xsd:complexType name="arrayType">
            <!-- comment -->
            <xsd:sequence dfdl:separator="|">
              <!-- comment -->
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
              <!-- comment -->
            </xsd:sequence>
            <!-- comment -->
          </xsd:complexType>
          <!-- comment -->
        </tdml:defineSchema>
        <!-- comment -->
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="array" model="mySchema">
          <!-- comment -->
          <tdml:document>
            <!-- comment -->
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
            <!-- comment -->
          </tdml:document>
          <!-- comment -->
          <tdml:infoset>
            <!-- comment -->
            <tdml:dfdlInfoset>
              <!-- comment -->
              <tns:array>
                <!-- comment -->
                <data>1</data>
                <!-- comment -->
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
                <!-- comment -->
              </tns:array>
              <!-- comment -->
            </tdml:dfdlInfoset>
            <!-- comment -->
          </tdml:infoset>
          <!-- comment -->
        </tdml:parserTestCase>
        <!-- comment -->
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
    // if we don't throw/fail when running then pass this test.
  }

}
