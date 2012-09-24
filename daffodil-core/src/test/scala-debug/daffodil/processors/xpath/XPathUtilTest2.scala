package daffodil.processors.xpath

import org.jdom.Document
import org.jdom.Element
import org.jdom.Text
import daffodil.processors.VariableMap
import daffodil.xml._
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test


class XPathUtilTest2 extends JUnitSuite {

 
  /**
   * Test that uses default namespace, not no-namespace.
   */
   @Test def testXPathWithDefaultNS1() { 
    
    // <root xmlns="urn:foobarNS><child1>19</child1></root>
    val text = new Text("19")
    val child1 = new Element("child1", "urn:foobarNS")
    val root = new Element("root", "urn:foobarNS")
    child1 addContent(text)
    root addContent(child1)
    val document = new Document(root)
       
    val ns = List(org.jdom.Namespace.getNamespace("", "urn:foobarNS"))
    val result = XPathUtil evalExpressionFromString("/root/child1",
        new VariableMap(),
        root,
        ns)
    
    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        println(result) 
        fail
      }
    }
  }
   
    @Test def testXPathWithNoNamespace() { 
    
    // <root xmlns="urn:foobarNS><child1>19</child1></root>
    val text = new Text("19")
    val child1 = new Element("child1")
    val root = new Element("root")
    child1 addContent(text)
    root addContent(child1)
    val document = new Document(root)
       
    val ns = Nil // List(org.jdom.Namespace.getNamespace("", "urn:foobarNS"))
    val result = XPathUtil evalExpressionFromString("/root/child1",
        new VariableMap(),
        root,
        ns)
    
    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        println(result) 
        fail
      }
    }
  }
   
   /**
    * Variation just gets the namespace by way of the root.getNamespace() call. 
    */
  @Test def testXPathWithDefaultNS2() { 
    
    // <root xmlns="urn:foobarNS><child1>19</child1></root>
    val text = new Text("19")
    val child1 = new Element("child1", "urn:foobarNS")
    val root = new Element("root", "urn:foobarNS")
    child1 addContent(text)
    root addContent(child1)
    val document = new Document(root)
       
    val ns = List(root.getNamespace()) // different way to get namespace info.
    val result = XPathUtil evalExpressionFromString("/root/child1",
        new VariableMap(),
        root,
        ns)
    
    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        println(result) 
        fail
      }
    }
  }
        
}
