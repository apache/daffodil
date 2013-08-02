package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.xpath._
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.util.SchemaUtils

/**
 * Tests for compiler-oriented XPath interface aka CompiledExpression
 */
class TestCompiledExpression2 extends WithParseErrorThrowing {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val dafint = XMLUtils.DAFFODIL_INTERNAL_NAMESPACE

  var context: SchemaComponent = null

  /**
   * Test the XPath evaluator, but provide namespace information on the XML
   */
  @Test def testCompiledAbsolutePathEvaluation2_withNamespace() {

    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format xmlns={ example } xmlns:tns={ example } ref="tns:daffodilTest1"/>,
      <xs:element name="root" type="xs:string"/>)

    val origInfoset = <tns:root xmlns={ example } xmlns:tns={ example } xmlns:dafint={ dafint }>19</tns:root>

    val c = new Compiler()

    val (sset, pf) = c.compileInternal(testSchema)
    val infoset = sset.getSCIDAugmentedInfoset(origInfoset)

    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(infoset) // root must have a document node
    val root = new InfosetElement(doc.getRootElement())

    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0, null)
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /tns:root/text() }"
    val compiled = ec.compile(ConvertToType.String, Found(xpathString, edecl)) // as a string
    val R(result, _) = compiled.evaluate(root, new VariableMap(), dummyState)

    assertEquals("19", result)

  }

  @Test def testCompiledRelativePathEvaluation5_WithNamespaces() {

    // Note Use of target namespace here.
    // Note that tns is bound to the same namespace as the target namespace
    // The relative path used below has a QName in it that uses this tns qualification.
    //
    // However, path evaluation seems to miss this, and the test fails because it doesn't find a node in the right namespace so the
    // expression returns null (no nodes in the nodeset that is the result of the XPath query)
    val tns = example

    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format xmlns={ example } xmlns:tns={ example } ref="tns:daffodilTest1"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    // Note that we specify the namespace of the unqualified elements here, and it matches
    // the target namespace.
    val origInfoset = <data xmlns={ example } xmlns:dafint={ dafint }><e1>42</e1><e2/></data>

    val c = new Compiler()

    val (sset, pf) = c.compileInternal(testSchema)
    val infoset = sset.getSCIDAugmentedInfoset(origInfoset)

    //
    // Note that we specify the namespace here as well.
    val edecl = sset.getGlobalElementDecl(tns, "data").get.forRoot()
    val ct = edecl.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]
    val Seq(e1: ElementBase, e2: ElementBase) = seq.groupMembers
    val ivcPrim = InputValueCalc(e2.asInstanceOf[LocalElementDecl])
    val parser = ivcPrim.parser.asInstanceOf[IVCParser]
    val d = Misc.stringToReadableByteChannel("xx") // it's not going to read from here.

    val initialState = PState.createInitialState(sset.schemaComponentRegistry, edecl, d, null)
    val doc = new org.jdom.Document(infoset) // root must have a document node
    val root = new InfosetElement(doc.getRootElement())
    val rootns = root.namespace
    val child2 = root.getChild("e2", rootns)
    val c2state = initialState.withParent(child2)
    val resState = parser.parse(c2state)
    val updatedChild2 = resState.parentElement
    val dataNode = updatedChild2.toXML
    // println(dataNode)
    val result = updatedChild2.dataValue

    assertEquals("42", result)

  }

  /**
   * Negative test. Insure we get a diagnostic that is useful.
   */
  @Test def testCompiledEvaluationError_DoesntExist() {

    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format xmlns={ example } xmlns:tns={ example } ref="tns:daffodilTest1"/>,
      <xs:element name="root" type="xs:string"/>)

    val origInfoset = <tns:root xmlns={ example } xmlns:tns={ example } xmlns:dafint={ dafint }>19</tns:root>

    val c = new Compiler()

    val (sset, pf) = c.compileInternal(testSchema)
    val infoset = sset.getSCIDAugmentedInfoset(origInfoset)

    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(infoset) // root must have a document node
    val root = new InfosetElement(doc.getRootElement())

    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0, null)
    context = edecl
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /tns:doesntExist/text() }"
    val compiled = ec.compile(ConvertToType.String, Found(xpathString, edecl)) // as a string
    val st = PState.createInitialState(sset.schemaComponentRegistry, edecl, "x", 0, null)
    withParseErrorThrowing(st) {
      val e = intercept[ParseError] {
        val R(res, _) = compiled.evaluate(root, new VariableMap(), dummyState)
        res
      }
      assertTrue(e.getMessage().contains("doesntExist"))
      assertTrue(e.getMessage().contains("XPathExpressionException"))
      st
    }

  }
}
