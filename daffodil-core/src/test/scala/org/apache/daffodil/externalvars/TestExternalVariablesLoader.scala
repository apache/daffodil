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

package org.apache.daffodil.externalvars

import org.apache.daffodil.xml._
import org.apache.daffodil.util._
import scala.xml._
import junit.framework.Assert._
import org.junit.Test
import org.apache.daffodil.dsom.SchemaSet
import org.apache.daffodil.xml.NS
import org.apache.daffodil.Implicits._
import org.junit.Test
import org.apache.daffodil.dsom.SchemaDefinitionError
import scala.util.Success
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.api.DaffodilTunables

class TestExternalVariablesLoader extends Logging {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.
  
  val tunable = DaffodilTunables()

  def generateSD(topLevelAnnotations: Seq[Node] = <dfdl:format ref="tns:GeneralFormat"/>) = {
    lazy val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      topLevelAnnotations,
      <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>
      <xs:element name="fake2" type="tns:fakeCT"/>
      <xs:complexType name="fakeCT">
        <xs:sequence>
          <xs:group ref="tns:fakeGroup"/>
          <xs:element ref="tns:fake"/>
        </xs:sequence>
      </xs:complexType>
      <xs:group name="fakeGroup">
        <xs:choice>
          <xs:sequence/>
        </xs:choice>
      </xs:group>)
    lazy val xsd_sset = new SchemaSet(sch, "http://example.com", "fake")
    lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
    lazy val fakeSD = xsd_schema.schemaDocuments(0)
    (fakeSD, xsd_sset)
  }

  def FindValue(collection: Map[String, String], key: String, value: String): Boolean = {
    val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
      case Some(_) => true
      case None => false
    }
    found
  }

  @Test def test_override_success() = {
    val extVarFile1 = Misc.getRequiredResource("/test/external_vars_1.xml")

    val topLevelAnnotations = {
      <dfdl:format ref="tns:GeneralFormat"/>
      <dfdl:defineVariable name="v_no_default" type="xs:int" external="true"/>
      <dfdl:defineVariable name="v_with_default" type="xs:int" external="true" defaultValue="42"/>
    }

    val Success(v_no_default) =
      QName.refQNameFromExtendedSyntax("{http://example.com}v_no_default").map { _.toGlobalQName }
    val Success(v_with_default) =
      QName.refQNameFromExtendedSyntax("{http://example.com}v_with_default").map { _.toGlobalQName }

    val (sd, sset) = generateSD(topLevelAnnotations)
    val initialVMap = sset.variableMap

    val lst_v_no_default = initialVMap.getVariableBindings(v_no_default)
    val lst_v_with_default = initialVMap.getVariableBindings(v_with_default)

    val var_v_no_default = lst_v_no_default.head.head
    val var_v_with_default = lst_v_with_default.head.head

    val v_no_default_vrd = var_v_no_default.rd
    val v_with_default_vrd = var_v_with_default.rd

    // Verify that v_no_default isn't defined
    assertFalse(var_v_no_default.value.isDefined)

    // Verify that v_with_default is defined and is 42.
    assertTrue(var_v_with_default.value.isDefined)
    assertEquals("42", var_v_with_default.value.getAnyRef.toString())

    val bindings = ExternalVariablesLoader.uriToBindings(extVarFile1)
    val vmap = ExternalVariablesLoader.loadVariables(bindings, sd, initialVMap)

    // Verify that the external variables override the previous values
    // in the VariableMap
    val (value1, _) = vmap.readVariable(v_no_default_vrd, Fakes.fakeElem)
    assertEquals(1, value1.getAnyRef)
    val (value2, _) = vmap.readVariable(v_with_default_vrd, Fakes.fakeElem)
    assertEquals(2, value2.getAnyRef)
  }

  @Test def test_ext_var_not_match_defined_var() = {
    val extVarFile1 = Misc.getRequiredResource("/test/external_vars_1.xml")

    val e = intercept[SchemaDefinitionError] {
      // fakeSD does not contain any defineVariables
      // Because we are trying to load external variables and none are defined we should SDE.
      val bindings = ExternalVariablesLoader.uriToBindings((extVarFile1))
      ExternalVariablesLoader.loadVariables(bindings, Fakes.fakeSD, new VariableMap())
    }
    val err = e.getMessage()
    assertTrue(err.contains("unknown variable ex:v_no_default"))
  }

}
