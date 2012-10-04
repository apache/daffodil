package daffodil.dsom

import org.scalatest.junit.JUnitSuite
import daffodil.xml._
import daffodil.processors._
import daffodil.compiler._
import daffodil.processors.xpath._
import junit.framework.Assert._
import org.junit.Test
import daffodil.util.TestUtils



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
    val root = doc.getRootElement()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /root/child1/child2/child3 }"
    val compiled = ec.compile('String, xpathString) // as a string

    val R(result, newVMap) = compiled.evaluate(root, new VariableMap())

    assertEquals("19", result)
    
    val compiled2 = ec.compile('Long, xpathString) // as a Long
    val R(result2, _) = compiled2.evaluate(root, new VariableMap())
    
    assertEquals(19L, result2)
    
    val compiled3 = ec.compile('Element, xpathString) // as a jdom element
    val R(result3, _) = compiled3.evaluate(root, new VariableMap())
    val r3string = result3.toString
    assertTrue(r3string.contains("<child3/>"))
    val r3value = result3.asInstanceOf[org.jdom.Element].getText()
    assertEquals("19", r3value)
  }
 
  @Test def testCompiledPathConstant() { 
    
    val root = null // won't be used.
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ 42 }"
    val compiled = ec.compile('String, xpathString) // as a string
    assertTrue(compiled.isConstant)

    val R(result, _) = compiled.evaluate(root, null)

    assertEquals("42", result)
    
    val compiled2 = ec.compile('Long, xpathString) // as a Long
    assertTrue(compiled2.isConstant)
    val R(result2, _) = compiled2.evaluate(root, null)
    
    assertEquals(42L, result2)
    
    val root2 = XMLUtils.elem2Element(<root/>)
    val ignored = new org.jdom.Document(root2)
    val compiled3 = ec.compile('Element, "{ /root }") // as a jdom Element
    assertFalse(compiled3.isConstant)
    val R(result3, _) = compiled3.evaluate(root2, null)
    val r3string = result3.toString
    assertTrue(r3string.contains("<root/>"))
  }
  
  /**
   * Test XPath evaluator, with no namespace specified on the XML or on the paths
   */
  @Test def testCompiledAbsolutePathEvaluation1() { 
    
    val r = XMLUtils.elem2Element(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /root/child1/child2/child3 }"
    val compiled = ec.compile('String, xpathString) // as a string
    val R(result, _) = compiled.evaluate(root, new VariableMap())

    assertEquals("19", result)
    
  }
      
  @Test def testCompiledRelativePathEvaluation1() { 
    
    val r = XMLUtils.elem2Element(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ child3 }"
    val compiled = ec.compile('String, xpathString) // as a string

    val child2 = root.getChild("child1").getChild("child2")
    val R(result, _) = compiled.evaluate(child2, new VariableMap())

    assertEquals("19", result)
    
  }
  
   @Test def testCompiledRelativePathEvaluation2() { 
    
    val r = XMLUtils.elem2Element(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ ../../../child1/child2/child3 }"
    val compiled = ec.compile('String, xpathString) // as a string

    val child3 = root.getChild("child1").getChild("child2").getChild("child3")
    val R(result, _) = compiled.evaluate(child3, new VariableMap())

    assertEquals("19", result)
    
  }
   
  @Test def testCompiledRelativePathEvaluation3() { 
    
    val r = XMLUtils.elem2Element(<data><e1>42</e1><e2/></data>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ ../e1 }"
    val compiled = ec.compile('String, xpathString) // as a string

    val child2 = root.getChild("e2")
    val R(result, _) = compiled.evaluate(child2, new VariableMap())

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
    val ct = edecl.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]
    val Seq(e1, e2) = seq.groupMembers
    val ivcPrim = InputValueCalc(e2.asInstanceOf[LocalElementDecl])
    val parser = ivcPrim.parser.asInstanceOf[IVCParser]
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val child2 = root.getChild("e2")
    val R(result, _) = parser.testExpressionEvaluation(child2, new VariableMap())

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
    val initialState = PState.createInitialState(edecl, d)
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val child2 = root.getChild("e2")
    val c2state = initialState.withParent(child2)
    val resState = parser.parse(c2state)
    val updatedChild2 = resState.parentElement
    val dataNode = XMLUtils.element2Elem(updatedChild2.getParent().asInstanceOf[org.jdom.Element])
    // println(dataNode)
    val result = updatedChild2.getText()

    assertEquals("42", result)
    
  }
  
  
  @Test def testCompiledRelativePathEvaluation6() {

    // Note: removed targetNamespace={ example }. 
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format xmlns:tns="http://example.com" ref="tns:daffodilTest1" />,
                       <xs:element name="data" dfdl:lengthKind="implicit" dfdl:initiator="" dfdl:terminator="">
                         <xs:complexType>
                           <xs:sequence dfdl:separator="" dfdl:initiator="" dfdl:terminator="" dfdl:initiatedContent="no">
                             <xs:element name="e1" type="xs:string" dfdl:encoding="ascii" dfdl:lengthUnits="bytes" dfdl:lengthKind="explicit" dfdl:length="2" dfdl:initiator="" dfdl:terminator=""/>
                             <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../tns:e1 }"/>
                           </xs:sequence>
                         </xs:complexType>
                       </xs:element>)
        
                   

    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl("http://example.com", "data").get.forRoot() // removed namespace example => ""
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
