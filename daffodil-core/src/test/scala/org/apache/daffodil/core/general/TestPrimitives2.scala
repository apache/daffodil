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

package org.apache.daffodil.core.general

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test

class TestPrimitives2 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testUnparseNilValueEntities(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" nillable="true" dfdl:nilKind="literalValue" dfdl:lengthKind="delimited" type="xs:string" dfdl:nilValue="%WSP;nil%NL; foobar" dfdl:outputNewLine="%LF;"/>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example} xmlns:xsi={
      XMLUtils.XSI_NAMESPACE.toString()
    } xsi:nil="true"/>
    TestUtils.testUnparsing(sch, infoset, " nil\u000a")
  }

  @Test def testUnparseNilValueEntities2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:nilValue="start%WSP;bla%%WSP;;;;foo%WSP*;bar%WSP+;baz%ES;quux%NL;boo%%baz%%NL;end" dfdl:outputNewLine="%LF;" nillable="true" dfdl:nilKind="literalValue" dfdl:lengthKind="delimited" type="xs:string"/>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example} xmlns:xsi={
      XMLUtils.XSI_NAMESPACE.toString()
    } xsi:nil="true"/>
    TestUtils.testUnparsing(sch, infoset, "start bla%WSP;;;;foobar bazquux\u000aboo%baz%NL;end")
  }

}
