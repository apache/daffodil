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

package org.apache.daffodil.lib.util

import scala.xml._

import org.junit.Assert._
import org.junit.Test

class TestSchemaUtils {

  /**
   * Just some random TDML-like DFDL fragments.
   */
  val test1 =
    <surround xmlns:ex="http://example.com" xmlns:ct="http://w3.ibm.com/xmlns/dfdl/ctInfoset" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData">
                <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                <dfdl:format ref="ex:GeneralFormat"/>
                <xs:element dfdl:lengthKind="delimited" name="intRestrict">
                  <xs:simpleType>
                    <xs:restriction base="xs:int">
                      <xs:totalDigits value="1"/>
                      <xs:minInclusive value="1"/>
                      <xs:maxInclusive value="10"/>
                    </xs:restriction>
                  </xs:simpleType>
                </xs:element>
                <xs:element dfdl:lengthKind="delimited" name="arrayCombo">
                  <xs:complexType>
                    <xs:sequence dfdl:separator=",">
                      <xs:element maxOccurs="unbounded" minOccurs="3" dfdl:lengthKind="delimited" name="e">
                        <xs:simpleType>
                          <xs:restriction base="xs:int">
                            <xs:maxExclusive value="3"/>
                            <xs:totalDigits value="1"/>
                          </xs:restriction>
                        </xs:simpleType>
                      </xs:element>
                    </xs:sequence>
                  </xs:complexType>
                </xs:element>
              </surround>

  def sameScopeEverywhere(x: Node, scope: NamespaceBinding): Boolean = {
    x match {
      case e: Elem =>
        e.scope == scope && e.child.forall {
          sameScopeEverywhere(_, scope)
        }
      case _ => true
    }
  }

  @Test def testDFDLTestSchema1(): Unit = {
    val incl = test1 \\ "include"
    val anns = test1 \\ "format"
    val elems = test1 \\ "element"
    val sch = SchemaUtils.dfdlTestSchema(incl, anns, elems, schemaScope = test1.scope)
    val scope = sch.scope
    assertTrue(sameScopeEverywhere(sch, scope))
  }

}
