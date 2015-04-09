/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dsom

import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits.ns2String
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertFalse
import junit.framework.Assert.assertTrue
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.xml.NS
import scala.xml.Node
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import scala.xml.NodeSeq
import edu.illinois.ncsa.daffodil.externalvars.Binding
import java.io.File
import edu.illinois.ncsa.daffodil.Implicits._
import org.xml.sax.InputSource
import edu.illinois.ncsa.daffodil.api.UnitTestSchemaSource
import edu.illinois.ncsa.daffodil.util.LogLevel

/**
 * Tests for compiler-oriented XPath interface aka CompiledExpression
 */
class TestExternalVariablesNew {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def generateSD(topLevelAnnotations: Seq[Node] = <dfdl:format ref="tns:daffodilTest1"/>) = {
    lazy val sch = SchemaUtils.dfdlTestSchema(
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

  def generateTestSchema(topLevelAnnotations: Seq[Node]) = {
    val sch = SchemaUtils.dfdlTestSchema(
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
    sch
  }

  def generateTestSchemaWithTarget(topLevelAnnotations: Seq[Node], theTargetNS: String, importSchemaLocation: String) = {
    val sch = SchemaUtils.dfdlTestSchemaWithTarget(
      topLevelAnnotations,
      <xs:import schemaLocation={ importSchemaLocation } namespace=""/>
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
      theTargetNS)
    sch
  }

  def generateTestSchemaNoTarget(topLevelAnnotations: Seq[Node]) = {
    val sch = SchemaUtils.dfdlTestSchemaWithTarget(
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
      "")
    sch
  }

  def checkResult(vmap: VariableMap, keyToFind: String, expectedValue: String) = {
    vmap.variables.find { case (qn, value) => qn.toString == keyToFind } match {
      case None => fail("Did not find " + keyToFind + " in the VariableMap.")
      case Some((key, value)) => {
        // Found var1 but is the value correct?
        assertTrue(value.toString.contains("Variable(VariableDefined,One(" + expectedValue + ")"))
      }
    }
  }

  @Test def test_figures_out_namespace_success() {
    // Here we want to test that if we do not give
    // a namespace that Daffodil is able to figure
    // out what it should be if there is no ambiguity.
    //
    val tla_no_ns = {
      <dfdl:format ref="daffodilTest1"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2"/>
    }
    val sch_no_ns = generateTestSchemaNoTarget(tla_no_ns)
    val source_no_ns = UnitTestSchemaSource(sch_no_ns, "test_figures_out_namespace_success_no_ns")

    val tla = {
      <dfdl:format ref="tns:daffodilTest1"/>
      <dfdl:defineVariable name="var1" type="xs:string" external="true" defaultValue="default1"/>
      <dfdl:defineVariable name="var3" type="xs:string" external="true" defaultValue="default3"/>
    }
    val s1 = source_no_ns.tempSchemaFile.toString

    val sch = generateTestSchemaWithTarget(tla, XMLUtils.EXAMPLE_NAMESPACE, source_no_ns.tempSchemaFile.toURI.toString)
    val source = UnitTestSchemaSource(sch, "test_figures_out_namespace_success")

    val s2 = source.tempSchemaFile.toString

    val vars = Map(
      ("{http://example.com}var1", "value1"), // Namespace defined
      ("{}var2", "value2"), // NoNamespace
      ("var3", "value3")) // Figure out the namespace

    val variables = ExternalVariablesLoader.getVariables(vars)

    val c = new Compiler()
    // c.setLoggingLevel(LogLevel.Debug)
    c.setExternalDFDLVariables(variables)
    c.setValidateDFDLSchemas(false)
    val pf = c.compileSource(source)
    pf.isError
    pf.diagnostics.foreach { d => println(d) }
    assertFalse(pf.isError)

    val sset = pf.sset

    val finalVars = sset.variableMap.variables

    // var1's namespace was htp://example.com, so we expect to find it
    checkResult(sset.variableMap, "{http://example.com}var1", "value1")

    // var2's namespace was NoNamespace, so we expect to find it
    checkResult(sset.variableMap, "{}var2", "value2")

    // var3's namespace was not given so we needed to figure it out.
    // We need to determine if we successfully figured out the namespace
    checkResult(sset.variableMap, "{http://example.com}var3", "value3")

  }

  @Test def test_no_namespace_success() {
    // Here we want to test that even when multiple var2's
    // are defined with different namespaces that we can
    // set the correct one.
    //
    val tla_no_ns = {
      <dfdl:format ref="daffodilTest1"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2.2"/>
    }
    val sch_no_ns = generateTestSchemaNoTarget(tla_no_ns)
    val source_no_ns = UnitTestSchemaSource(sch_no_ns, "test_no_namespace_success_no_ns")

    val tla = {
      <dfdl:format ref="tns:daffodilTest1"/>
      <dfdl:defineVariable name="var1" type="xs:string" external="true" defaultValue="default1"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2.1"/>
    }
    val sch = generateTestSchemaWithTarget(tla, XMLUtils.EXAMPLE_NAMESPACE, source_no_ns.tempSchemaFile.toURI.toString)
    val source = UnitTestSchemaSource(sch, "test_no_namespace_success")

    val vars = Map(
      ("{http://example.com}var1", "value1"), // Namespace defined
      ("{}var2", "value2")) // NoNamespace

    val variables = ExternalVariablesLoader.getVariables(vars)

    val c = new Compiler()
    c.setExternalDFDLVariables(variables)
    c.setValidateDFDLSchemas(false)

    val pf = c.compileSource(source)
    val sset = pf.sset

    val finalVars = sset.variableMap.variables

    // var1's namespace was htp://example.com, so we expect to find it
    checkResult(sset.variableMap, "{http://example.com}var1", "value1")

    // var2's namespace was NoNamespace, so we expect to find it
    checkResult(sset.variableMap, "{}var2", "value2")

    // The other var2's namespace was http://example.com, so we expect
    // it to be unchanged.
    checkResult(sset.variableMap, "{http://example.com}var2", "default2.1")

  }

  @Test def test_figures_out_namespace_failure() {
    // We are purposefully defining multiple var3's but
    // in separate namespaces.  This test should fail
    // stating that var3 is ambiguous.
    //
    val tla_no_ns = {
      <dfdl:format ref="daffodilTest1"/>
      <dfdl:defineVariable name="var2" type="xs:string" external="true" defaultValue="default2"/>
      <dfdl:defineVariable name="var3" type="xs:string" external="true" defaultValue="default3.2"/>
    }
    val sch_no_ns = generateTestSchemaNoTarget(tla_no_ns)
    val source_no_ns = UnitTestSchemaSource(sch_no_ns, "test_figures_out_namespace_failure_no_ns")

    val tla = {
      <dfdl:format ref="tns:daffodilTest1"/>
      <dfdl:defineVariable name="var1" type="xs:string" external="true" defaultValue="default1"/>
      <dfdl:defineVariable name="var3" type="xs:string" external="true" defaultValue="default3.1"/>
    }
    val sch = generateTestSchemaWithTarget(tla, XMLUtils.EXAMPLE_NAMESPACE, source_no_ns.tempSchemaFile.toURI.toString)
    val source = UnitTestSchemaSource(sch, "test_figures_out_namespace_failure")

    val vars = Map(
      ("{http://example.com}var1", "value1"), // Namespace defined
      ("{}var2", "value2"), // NoNamespace
      ("var3", "value3")) // Figure out the namespace

    val variables = ExternalVariablesLoader.getVariables(vars)

    val c = new Compiler()
    c.setExternalDFDLVariables(variables)
    c.setValidateDFDLSchemas(false)
    val pf = c.compileSource(source)
    val sset = pf.sset
    val msg = sset.getDiagnostics.mkString
    if (!msg.contains("var3 is ambiguous")) {
      println(msg)
      fail()
    }
  }

}
