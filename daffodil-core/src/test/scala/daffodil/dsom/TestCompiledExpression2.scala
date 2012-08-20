package daffodil.dsom

import org.scalatest.junit.JUnit3Suite
import daffodil.xml._
import daffodil.processors._
import daffodil.processors.xpath._
import junit.framework.Assert._
import daffodil.processors._
import daffodil.compiler._
import javax.xml.xpath.XPathExpressionException

/**
 * Tests for compiler-oriented XPath interface aka CompiledExpression
 */
class TestCompiledExpression2 extends JUnit3Suite {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  // dummy schema just so we can get a handle on a legit element declaration
  val testSchema = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                     <element name="root" type="xs:string"/>
                   </schema>

  /**
   * Test the XPath evaluator, but provide namespace information on the XML
   */
  def testCompiledAbsolutePathEvaluation2_withNamespace() {

    val r = XMLUtils.elem2Element(
      <tns:root xmlns:tns={ example }>19</tns:root>)
    val sset = new SchemaSet(<schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                               <element name="root" type="xs:string"/>
                             </schema>)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /tns:root/text() }"
    val compiled = ec.compile('String, xpathString) // as a string
    val result = compiled.evaluate(root, new VariableMap())

    assertEquals("19", result)

  }

  def testCompiledRelativePathEvaluation5_WithNamespaces() {

    // Note Use of target namespace here.
    // Note that tns is bound to the same namespace as the target namespace
    // The relative path used below has a QName in it that uses this tns qualification.
    //
    // However, path evaluation seems to miss this, and the test fails because it doesn't find a node in the right namespace so the
    // expression returns null (no nodes in the nodeset that is the result of the XPath query)
    val testSchema = <xs:schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
                       <xs:element name="data">
                         <xs:complexType>
                           <xs:sequence>
                             <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="2"/>
                             <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../tns:e1 }"/>
                           </xs:sequence>
                         </xs:complexType>
                       </xs:element>
                     </xs:schema>

    val tns = example

    // Note that we specify the namespace of the unqualified elements here, and it matches
    // the target namespace.
    val r = XMLUtils.elem2Element(<data xmlns={ example }><e1>42</e1><e2/></data>)
    val sset = new SchemaSet(testSchema)
    //
    // Note that we specify the namespace here as well.
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
    val rootns = root.getNamespace()
    val child2 = root.getChild("e2", rootns)
    val c2state = initialState.withParent(child2)
    val resState = parser.parse(c2state)
    val updatedChild2 = resState.parentElement
    val dataNode = XMLUtils.element2Elem(updatedChild2.getParent().asInstanceOf[org.jdom.Element])
    // println(dataNode)
    val result = updatedChild2.getText()

    assertEquals("42", result)

  }

  /**
   * Negative test. Insure we get a diagnostic that is useful.
   */
  def testCompiledEvaluationError_DoesntExist() {

    val r = XMLUtils.elem2Element(
      <tns:root xmlns:tns={ example }>19</tns:root>)
    val sset = new SchemaSet(<schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                               <element name="root" type="xs:string"/>
                             </schema>)
    val edecl = sset.getGlobalElementDecl(example, "root").get.forRoot()
    val doc = new org.jdom.Document(r) // root must have a document node
    val root = doc.getRootElement()
    val ec = new ExpressionCompiler(edecl)
    val xpathString = "{ /tns:doesntExist/text() }"
    val compiled = ec.compile('String, xpathString) // as a string
    val e = intercept[XPathExpressionException] {
      val res = compiled.evaluate(root, new VariableMap())
      res
    }
    assertTrue(e.getMessage().contains("doesntExist"))
  }
}
