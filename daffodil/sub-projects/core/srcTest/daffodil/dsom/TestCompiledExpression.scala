package daffodil.dsom

import org.scalatest.junit.JUnit3Suite
import daffodil.xml._
import daffodil.processors._
import daffodil.processors.xpath._
import junit.framework.Assert._

/**
 * Tests for compiler-oriented XPath interface aka CompiledExpression
 */
class TestCompiledExpression extends JUnit3Suite {
  val xsd = XMLUtil.XSD_NAMESPACE
  val dfdl = XMLUtil.DFDL_NAMESPACE
  val xsi = XMLUtil.XSI_NAMESPACE
  val example = XMLUtil.EXAMPLE_NAMESPACE

  // dummy schema just so we can get a handle on a legit element declaration
  val testSchema = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                       <element name="root" type="xs:string"/>
                     </schema>
  
 def testCompiledPathEvaluation() { 
    
    val root = XMLUtil.elem2Element(<root><child1><child2><child3>19</child3></child2></child1></root>)
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val ignored = new org.jdom.Document(root) // root must have a document node
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /root/child1/child2/child3 }"
    val compiled = ec.compile('String, xpathString) // as a string

    val result = compiled.evaluate(root, new VariableMap())

    assertEquals("19", result)
    
    val compiled2 = ec.compile('Long, xpathString) // as a Long
    val result2 = compiled2.evaluate(root, new VariableMap())
    
    assertEquals(19L, result2)
    
    val compiled3 = ec.compile('Element, xpathString) // as a jdom element
    val result3 = compiled3.evaluate(root, new VariableMap())
    val r3string = result3.toString
    assertTrue(r3string.contains("<child3/>"))
    val r3value = result3.asInstanceOf[org.jdom.Element].getText()
    assertEquals("19", r3value)
  }
 
  def testCompiledPathConstant() { 
    
    val root = null // won't be used.
    val sset = new SchemaSet(testSchema)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ 42 }"
    val compiled = ec.compile('String, xpathString) // as a string
    assertTrue(compiled.isConstant)

    val result = compiled.evaluate(root, null)

    assertEquals("42", result)
    
    val compiled2 = ec.compile('Long, xpathString) // as a Long
    assertTrue(compiled2.isConstant)
    val result2 = compiled2.evaluate(root, null)
    
    assertEquals(42L, result2)
    
    val root2 = XMLUtil.elem2Element(<root/>)
    val ignored = new org.jdom.Document(root2)
    val compiled3 = ec.compile('Element, "{ /root }") // as a jdom Element
    assertFalse(compiled3.isConstant)
    val result3 = compiled3.evaluate(root2, null)
    val r3string = result3.toString
    assertTrue(r3string.contains("<root/>"))
  }
}