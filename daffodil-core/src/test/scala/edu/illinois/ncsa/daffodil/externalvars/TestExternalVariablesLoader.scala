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

package edu.illinois.ncsa.daffodil.externalvars

import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util._
import scala.xml._
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.dsom.SchemaSet
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.Implicits._
import org.junit.Test
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import scala.util.Success
import edu.illinois.ncsa.daffodil.processors.VariableMap

class TestExternalVariablesLoader extends Logging {
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
      <dfdl:format ref="tns:daffodilTest1"/>
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
    assertEquals("42", var_v_with_default.value.get.toString())

    val vmap = ExternalVariablesLoader.loadVariables(extVarFile1, sd, initialVMap)

    // Verify that the external variables override the previous values
    // in the VariableMap
    val (value1, _) = vmap.readVariable(v_no_default_vrd, Fakes.fakeElem)
    assertEquals(1, value1)
    val (value2, _) = vmap.readVariable(v_with_default_vrd, Fakes.fakeElem)
    assertEquals(2, value2)
  }

  @Test def test_ext_var_not_match_defined_var() = {
    val extVarFile1 = Misc.getRequiredResource("/test/external_vars_1.xml")

    val e = intercept[SchemaDefinitionError] {
      // fakeSD does not contain any defineVariables
      // Because we are trying to load external variables and none are defined we should SDE.
      ExternalVariablesLoader.loadVariables(extVarFile1, Fakes.fakeSD, new VariableMap())
    }
    val err = e.getMessage()
    assertTrue(err.contains("unknown variable {http://example.com}v_no_default"))
  }

}
