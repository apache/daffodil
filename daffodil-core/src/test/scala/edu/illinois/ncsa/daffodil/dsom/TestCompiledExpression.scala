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

import org.scalatest.junit.JUnitSuite
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors.xpath._
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.util.TestUtils
import edu.illinois.ncsa.daffodil.Implicits._

/**
 * Tests for compiler-oriented XPath interface aka CompiledExpression
 */
class TestCompiledExpression extends JUnitSuite {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  // dummy schema just so we can get a handle on a legit element declaration
  val testSchema = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                     <element name="root" type="xs:string"/>
                   </schema>

  @Test def testCompiledPathEvaluation() {

    val r = XMLUtils.elem2Element(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = new InfosetElement(doc.getRootElement())
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /root/child1/child2/child3 }"
    val compiled = ec.compile('String, xpathString) // as a string
    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0)

    val R(result, newVMap) = compiled.evaluate(root, new VariableMap(), dummyState)

    assertEquals("19", result)

    val compiled2 = ec.compile('Long, xpathString) // as a Long
    val R(result2, _) = compiled2.evaluate(root, new VariableMap(), dummyState)

    assertEquals(19L, result2)

    val compiled3 = ec.compile('Element, xpathString) // as a jdom element
    val R(result3, _) = compiled3.evaluate(root, new VariableMap(), dummyState)
    val r3string = result3.toString
    assertTrue(r3string.contains("<child3/>"))
    val r3value = result3.asInstanceOf[org.jdom.Element].getText()
    assertEquals("19", r3value)
  }

  @Test def testCompiledPathConstant() {

    val root = null // won't be used.
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0)

    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ 42 }"
    val compiled = ec.compile('String, xpathString) // as a string
    assertTrue(compiled.isConstant)

    val R(result, _) = compiled.evaluate(root, null, dummyState)

    assertEquals("42", result)

    val compiled2 = ec.compile('Long, xpathString) // as a Long
    assertTrue(compiled2.isConstant)
    val R(result2, _) = compiled2.evaluate(root, null, dummyState)

    assertEquals(42L, result2)

    val root2 = Infoset(<root/>)
    val compiled3 = ec.compile('Element, "{ /root }") // as a jdom Element
    assertFalse(compiled3.isConstant)
    val R(result3, _) = compiled3.evaluate(root2, null, dummyState)
    val r3string = result3.toString
    assertTrue(r3string.contains("<root/>"))
  }

  /**
   * Test XPath evaluator, with no namespace specified on the XML or on the paths
   */
  @Test def testCompiledAbsolutePathEvaluation1() {

    val root = Infoset(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /root/child1/child2/child3 }"
    val compiled = ec.compile('String, xpathString) // as a string
    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0)
    val R(result, _) = compiled.evaluate(root, new VariableMap(), dummyState)

    assertEquals("19", result)

  }

  @Test def testCompiledRelativePathEvaluation1() {

    val root = Infoset(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ child3 }"
    val compiled = ec.compile('String, xpathString) // as a string

    val child2 = root.getChild("child1").getChild("child2")
    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0)
    val R(result, _) = compiled.evaluate(child2, new VariableMap(), dummyState)

    assertEquals("19", result)

  }

  @Test def testCompiledRelativePathEvaluation2() {

    val root = Infoset(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ ../../../child1/child2/child3 }"
    val compiled = ec.compile('String, xpathString) // as a string
    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0)
    val child3 = root.getChild("child1").getChild("child2").getChild("child3")
    val R(result, _) = compiled.evaluate(child3, new VariableMap(), dummyState)

    assertEquals("19", result)

  }

  @Test def testCompiledRelativePathEvaluation3() {

    val root = Infoset(<data><e1>42</e1><e2/></data>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ ../e1 }"
    val compiled = ec.compile('String, xpathString) // as a string
    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0)
    val child2 = root.getChild("e2")
    val R(result, _) = compiled.evaluate(child2, new VariableMap(), dummyState)

    assertEquals("42", result)

  }

  @Test def testCompiledRelativePathEvaluation4() {

    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format xmlns:tns="http://example.com" ref="tns:daffodilTest1"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val r = XMLUtils.elem2Element(<data><e1>42</e1><e2/></data>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "data").get.forRoot()
    val dummyState = PState.createInitialState(sset.schemaComponentRegistry, edecl, "", 0)
    val ct = edecl.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]
    val Seq(e1, e2) = seq.groupMembers
    val ivcPrim = InputValueCalc(e2.asInstanceOf[LocalElementDecl])
    val parser = ivcPrim.parser.asInstanceOf[IVCParser]
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val child2 = root.getChild("e2")
    val R(result, _) = ivcPrim.expr.evaluate(new InfosetElement(child2), new VariableMap(), dummyState)

    assertEquals("42", result)

  }

  @Test def testCompiledRelativePathEvaluation5() {

    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format xmlns:tns="http://example.com" ref="tns:daffodilTest1"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val r = XMLUtils.elem2Element(<data><e1>42</e1><e2/></data>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "data").get.forRoot()
    val ct = edecl.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]
    val Seq(e1, e2) = seq.groupMembers
    val ivcPrim = InputValueCalc(e2.asInstanceOf[LocalElementDecl])
    val parser = ivcPrim.parser.asInstanceOf[IVCParser]
    val d = Compiler.stringToReadableByteChannel("xx") // it's not going to read from here.
    val initialState = PState.createInitialState(sset.schemaComponentRegistry, edecl, d)
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val child2 = root.getChild("e2")
    val c2state = initialState.withParent(new InfosetElement(child2))
    val resState = parser.parse(c2state)
    val updatedChild2 = resState.parentElement
    val dataNode = updatedChild2.parent.toXML
    // println(dataNode)
    val result = updatedChild2.dataValue

    assertEquals("42", result)

  }

  @Test def testCompiledRelativePathEvaluation6() {

    // Note: removed targetNamespace={ example }. 
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format xmlns:tns={ example } ref="tns:daffodilTest1"/>,
      <xs:element name="data" dfdl:lengthKind="implicit" dfdl:initiator="" dfdl:terminator="">
        <xs:complexType>
          <xs:sequence dfdl:separator="" dfdl:initiator="" dfdl:terminator="" dfdl:initiatedContent="no">
            <xs:element name="e1" type="xs:string" dfdl:encoding="ascii" dfdl:lengthUnits="bytes" dfdl:lengthKind="explicit" dfdl:length="2" dfdl:initiator="" dfdl:terminator=""/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "data").get.forRoot() // removed namespace example => ""
    val ct = edecl.typeDef.asInstanceOf[ComplexTypeBase]
    val d = Compiler.stringToReadableByteChannel("42")
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val dp = pf.onPath("/")
    val resState = dp.parse(d)
    val resNode = resState.result
    //println(resNode)
    val Seq(e2) = (resNode \\ "e2")
    val dataNode = e2.text
    //println(dataNode)

    assertEquals("42", dataNode)

  }
}
