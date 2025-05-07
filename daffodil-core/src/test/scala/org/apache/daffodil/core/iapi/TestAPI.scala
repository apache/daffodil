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

package org.apache.daffodil.core.iapi

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test; object INoWarn7 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.core.util.TestUtils

class TestDFDLParser_New {

  @Test def testParseOccursCountKindOfParsedDelimitedBySeparatorImplicitWithMaxOccurs()
    : Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence dfdl:separator=";" dfdl:terminator=";">
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="delimited" maxOccurs="4" minOccurs="0" dfdl:occursCountKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "5;6;7;8;.")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedBySeparatorImplicitWithMaxOccurs2()
    : Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence dfdl:separator=";" dfdl:terminator="!">
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="delimited" maxOccurs="100" minOccurs="0" dfdl:occursCountKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "5;6;7;8!.")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }
}
