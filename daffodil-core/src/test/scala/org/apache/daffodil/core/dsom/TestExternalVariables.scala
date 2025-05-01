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

package org.apache.daffodil.core.dsom

import java.nio.channels.Channels
import scala.xml.Node

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.Implicits.ns2String
import org.apache.daffodil.lib.iapi.UnitTestSchemaSource
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.QName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.externalvars.ExternalVariablesLoader
import org.apache.daffodil.runtime1.iapi.DFDL.DataProcessor
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.runtime1.processors.ExternalVariableException
import org.apache.daffodil.runtime1.processors.VariableMap

import org.junit.Assert._
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Tests for compiler-oriented XPath interface aka CompiledExpression
 */
class TestExternalVariables {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def generateSD(topLevelAnnotations: Seq[Node] = <dfdl:format ref="tns:GeneralFormat"/>) = {
    lazy val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:group>
    )
    lazy val xsd_sset = SchemaSet(sch, "http://example.com", "fake")
    lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
    lazy val fakeSD = xsd_schema.schemaDocuments(0)
    (fakeSD, xsd_sset)
  }

  def generateTestSchema(topLevelAnnotations: Seq[Node]) = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:group>
    )
    sch
  }

  def generateTestSchemaWithTarget(
    topLevelAnnotations: Seq[Node],
    theTargetNS: String,
    importSchemaLocation: String,
    hasDefaultNamespace: Boolean = true
  ) = {
    val sch = SchemaUtils.dfdlTestSchemaWithTarget(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      topLevelAnnotations,
      <xs:import schemaLocation={importSchemaLocation} namespace=""/>
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
      </xs:group>,
      theTargetNS,
      hasDefaultNamespace = hasDefaultNamespace
    )
    sch
  }

  def generateTestSchemaNoTarget(topLevelAnnotations: Seq[Node]) = {
    val sch = SchemaUtils.dfdlTestSchemaWithTarget(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      topLevelAnnotations,
      <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>
      <xs:element name="fake2" type="fakeCT"/>
      <xs:complexType name="fakeCT">
        <xs:sequence>
          <xs:group ref="fakeGroup"/>
          <xs:element ref="fake"/>
        </xs:sequence>
      </xs:complexType>
      <xs:group name="fakeGroup">
        <xs:choice>
          <xs:sequence/>
        </xs:choice>
      </xs:group>,
      ""
    )
    sch
  }

  def generateTestSchemaVmap(topLevelAnnotations: Seq[Node], theTargetNS: String) = {
    val sch = SchemaUtils.dfdlTestSchemaWithTarget(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      topLevelAnnotations,
      <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"
        dfdl:inputValueCalc="{ $ex:var1 }" />,
      theTargetNS
    )
    sch
  }

  def checkResult(vmap: VariableMap, keyToFind: String, expectedValue: String) = {
    import scala.util.{ Success, Failure }
    val tri = QName.refQNameFromExtendedSyntax(keyToFind).map { _.toGlobalQName }.map { qn =>
      vmap.find(qn)
    }
    tri match {
      case Failure(th) =>
        fail("The syntax '" + keyToFind + "' did not parse. Got " + Misc.getSomeMessage(th).get)
      case Success(None) => fail("Did not find " + keyToFind + " in the VariableMap.")
      case Success(Some(variab)) => {
        // Found var1 but is the value correct?
        // Variables that aren't externally defined will be undefined, but
        // expected value should be in the expression
        if (variab.toString.contains("VariableDefined"))
          assertTrue(variab.toString.contains("DataValue(" + expectedValue + ")"))
        else
          assertTrue(variab.toString.contains("CompiledExpression(" + expectedValue + ")"))
      }
    }
  }

  @Test def test_figures_out_namespace_success(): Unit = {
    // Here we want to test that if we do not give
    // a namespace that Daffodil is able to figure
    // out what it should be if there is no ambiguity.
    //
    val tla_no_ns = {
      <dfdl:format ref="GeneralFormat"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2"/>
    }
    val sch_no_ns = generateTestSchemaNoTarget(tla_no_ns)
    val source_no_ns =
      UnitTestSchemaSource(sch_no_ns, "test_figures_out_namespace_success_no_ns")

    val tla = {
      <dfdl:format ref="tns:GeneralFormat"/>
      <dfdl:defineVariable name="var1" type="xs:string" external="true" defaultValue="default1"/>
      <dfdl:defineVariable name="var3" type="xs:string" external="true" defaultValue="default3"/>
    }
    source_no_ns.uriForLoading.toString

    val sch = generateTestSchemaWithTarget(
      tla,
      XMLUtils.EXAMPLE_NAMESPACE,
      source_no_ns.uriForLoading.toString
    )
    val source = UnitTestSchemaSource(sch, "test_figures_out_namespace_success")

    source.uriForLoading.toString

    val vars = Map(
      ("{http://example.com}var1", "value1"), // Namespace defined
      ("{}var2", "value2"), // NoNamespace
      ("var3", "value3")
    ) // Figure out the namespace

    val variables = ExternalVariablesLoader.mapToBindings(vars)

    val c = Compiler(validateDFDLSchemas = false)
    val pf = c.compileSource(source)
    pf.isError
    pf.diagnostics.foreach { d => println(d) }
    assertFalse(pf.isError)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor].withExternalVariables(variables)

    val sset = pf.sset

    // var1's namespace was htp://example.com, so we expect to find it
    checkResult(dp.variableMap, "{http://example.com}var1", "value1")

    // var2's namespace was NoNamespace, so we expect to find it
    checkResult(dp.variableMap, "{}var2", "value2")

    // var3's namespace was not given so we needed to figure it out.
    // We need to determine if we successfully figured out the namespace
    checkResult(dp.variableMap, "{http://example.com}var3", "value3")

  }

  @Test def test_no_namespace_success(): Unit = {
    // Here we want to test that even when multiple var2's
    // are defined with different namespaces that we can
    // set the correct one.
    //
    val tla_no_ns = {
      <dfdl:format ref="GeneralFormat"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2.2"/>
    }
    val sch_no_ns = generateTestSchemaNoTarget(tla_no_ns)
    val source_no_ns = UnitTestSchemaSource(sch_no_ns, "test_no_namespace_success_no_ns")

    val tla = {
      <dfdl:format ref="tns:GeneralFormat"/>
      <dfdl:defineVariable name="var1" type="xs:string" external="true" defaultValue="default1"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2.1"/>
    }
    val sch = generateTestSchemaWithTarget(
      tla,
      XMLUtils.EXAMPLE_NAMESPACE,
      source_no_ns.uriForLoading.toString
    )
    val source = UnitTestSchemaSource(sch, "test_no_namespace_success")

    val vars = Map(
      ("{http://example.com}var1", "value1"), // Namespace defined
      ("{}var2", "value2")
    ) // NoNamespace

    val variables = ExternalVariablesLoader.mapToBindings(vars)

    val c = Compiler(validateDFDLSchemas = false)

    val pf = c.compileSource(source)
    val sset = pf.sset

    val dp = pf.onPath("/").asInstanceOf[DataProcessor].withExternalVariables(variables)

    // var1's namespace was htp://example.com, so we expect to find it
    checkResult(dp.variableMap, "{http://example.com}var1", "value1")

    // var2's namespace was NoNamespace, so we expect to find it
    checkResult(dp.variableMap, "{}var2", "value2")

    // The other var2's namespace was http://example.com, so we expect
    // it to be undefined and without value.
    checkResult(dp.variableMap, "{http://example.com}var2", "default2.1")

  }

  @Test def test_figures_out_namespace_failure(): Unit = {
    // We are purposefully defining multiple var3's but
    // in separate namespaces.  This test should fail
    // stating that var3 is ambiguous.
    //
    val tla_no_ns = {
      <dfdl:format ref="GeneralFormat"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2"/>
      <dfdl:defineVariable name="var3" type="xs:string" external="true" defaultValue="default3.2"/>
    }
    val sch_no_ns = generateTestSchemaNoTarget(tla_no_ns)
    val source_no_ns =
      UnitTestSchemaSource(sch_no_ns, "test_figures_out_namespace_failure_no_ns")

    val tla = {
      <dfdl:format ref="tns:GeneralFormat"/>
      <dfdl:defineVariable name="var1" type="xs:string" external="true" defaultValue="default1"/>
      <dfdl:defineVariable name="var3" type="xs:string" external="true" defaultValue="default3.1"/>
    }
    val sch = generateTestSchemaWithTarget(
      tla,
      XMLUtils.EXAMPLE_NAMESPACE,
      source_no_ns.uriForLoading.toString,
      hasDefaultNamespace = false
    )
    val source = UnitTestSchemaSource(sch, "test_figures_out_namespace_failure")

    val vars = Map(
      ("{http://example.com}var1", "value1"), // Namespace defined
      ("{}var2", "value2"), // NoNamespace
      ("var3", "value3")
    ) // Figure out the namespace

    val c = Compiler(validateDFDLSchemas = false)
    val pf = c.compileSource(source)
    val sset = pf.sset
    val variables = ExternalVariablesLoader.mapToBindings(vars)

    val exception = intercept[ExternalVariableException] {
      pf.onPath("/").asInstanceOf[DataProcessor].withExternalVariables(variables)
    }

    val msg = exception.getMessage()
    assertTrue(msg.contains("var3 is ambiguous"))
    assertTrue(msg.contains("tns:var3"))
    assertTrue(msg.contains("{}var3"))
  }

  @Test def test_data_processor_vmap_copy(): Unit = {
    // Here we want to test that even when multiple var2's
    // are defined with different namespaces that we can
    // set the correct one.
    //
    val tla = {
      <dfdl:format ref="tns:GeneralFormat"/>
      <dfdl:defineVariable name="var1" type="xs:string" external="true" defaultValue="default1"/>
    }
    val sch = generateTestSchemaVmap(tla, XMLUtils.EXAMPLE_NAMESPACE)
    val source = UnitTestSchemaSource(sch, "test_data_processor_vmap_copy")

    val vars = Map(("{http://example.com}var1", "value1"))

    val variables = ExternalVariablesLoader.mapToBindings(vars)

    val c = Compiler(validateDFDLSchemas = false)

    val pf = c.compileSource(source)
    val sset = pf.sset

    val dp1 = pf.onPath("/").asInstanceOf[DataProcessor]
    val dp2 = pf.onPath("/").asInstanceOf[DataProcessor].withExternalVariables(variables)

    val outputter = new ScalaXMLInfosetOutputter()
    val input = InputSourceDataInputStream(
      Channels.newInputStream(Misc.stringToReadableByteChannel(""))
    )

    val res1 = dp1.parse(input, outputter)
    assertTrue(outputter.getResult().toString.contains("default1"))

    val res2 = dp2.parse(input, outputter)
    assertTrue(outputter.getResult().toString.contains("value1"))

    val res3 = dp1.parse(input, outputter)
    assertTrue(outputter.getResult().toString.contains("default1"))

    checkResult(dp1.variableMap, "{http://example.com}var1", "default1")
    checkResult(dp2.variableMap, "{http://example.com}var1", "value1")
    checkResult(dp1.variableMap, "{http://example.com}var1", "default1")
  }

}
