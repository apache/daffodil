package daffodil.schema.test.integration

import java.io.ByteArrayInputStream

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.parser.RollbackStream
import daffodil.processors.VariableMap
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.enumerations.Text
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil._


class SimpleElementTest extends FunSuite with ShouldMatchers {
 
  test("reading a row of comma-separated values terminated with EOF, TEXT") 
  {
    val testValue = "1,2,3,4,5"
    val testResult = List(element("cell","1"),
			  element("cell","2"),
			  element("cell","3"),
			  element("cell","4"),
			  element("cell","5"))

    
    val annotation = new Annotation(null)
    annotation.format setEncoding ("ASCII")
    annotation.format setType ("string")
    annotation.format setSeparator (",")
    annotation.format setRepresentation ("text")
    
    val simpleElement = 
      new SimpleElement("cell",annotation,null,new Namespaces)
    
    simpleElement.setMaxOccurs(-1)
    
    val root = new org.jdom.Element("root")
    
    val result = simpleElement (new RollbackStream(
      new ByteArrayInputStream(testValue getBytes("ASCII"))),new Annotation(null),new VariableMap(),root,-1,Nil)

    compare(root,element("root",testResult)) should be (true)
  }
  
   test("reading a row of comma-separated values terminated with \\n, TEXT"){
    val testValue = "1,2,3,4,5\n"
    val testResult = List(element("cell","1"),
			  element("cell","2"),
			  element("cell","3"),
			  element("cell","4"),
			  element("cell","5"))

    
    
    val annotation = new Annotation(null)
    annotation.format setEncoding("ASCII")
    annotation.format setType("string")
    annotation.format setSeparator(",")
    annotation.format setTerminator("\\n")
    annotation.format setRepresentation(Text)
    val simpleElement = 
      new SimpleElement("cell",annotation,"example.com:example",new Namespaces)
    
    simpleElement.setMaxOccurs(-1)
    
    val root = new org.jdom.Element("root")
    
    val result = simpleElement (new RollbackStream(
      new ByteArrayInputStream(testValue getBytes("ASCII"))),new Annotation(null),new VariableMap(),root,-1,Nil)

    compare(root,element("root",testResult)) should be (true)
  }
}

