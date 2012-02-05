package daffodil.schema.test.integration

import java.io.ByteArrayInputStream

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.jdom.Element

import daffodil.parser.RollbackStream
import daffodil.processors.VariableMap
import daffodil.schema.SimpleElement
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.enumerations.Text
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil._
import daffodil.Implicits._

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class SimpleElementTest extends JUnit3Suite with ShouldMatchers {
 
  def testReadingRowOfCSVTermnatedWithEOFText() { // ("reading a row of comma-separated values terminated with EOF, TEXT") 
    val testValue = "1,2,3,4,5"
    val testResults = 
      List(element("cell", "1"),
        element("cell", "2"),
        element("cell", "3"),
        element("cell", "4"),
        element("cell", "5"))

    
    val annotation = new Annotation(null)
    annotation.format setEncoding("ASCII")
    annotation.format setTypeName("string")
    annotation.format setSeparator(",")
    annotation.format setRepresentation("text")
    annotation.format setLengthKind("delimited")
    
    val simpleElement = 
      new SimpleElement("cell",annotation,null,new Namespaces)
    
    simpleElement.setMaxOccurs(-1)
    val datastream = new RollbackStream(new ByteArrayInputStream(testValue getBytes("ASCII")))
    val annote = new Annotation(null)
    val vmap = new VariableMap()
    val root = new Element("root")
    val results = simpleElement (datastream,annote,vmap,root,-1,Nil)
    (results, testResults).zipped map { 
      (result : Element, testResult : Element) =>
        assertTrue(compare(result, testResult))
      }
  }
  
   def testReadingARowOfCSVTerminatedWithNewlineText() { //("reading a row of comma-separated values terminated with \\n, TEXT"){
    val testValue = "1#,2#,3#,4#,5#"
    val testResults = List(element("cell","1"),
			  element("cell","2"),
			  element("cell","3"),
			  element("cell","4"),
			  element("cell","5"))

    
    
    val annotation = new Annotation(null)
    annotation.format setEncoding("ASCII")
    annotation.format setTypeName("string")
    annotation.format setSeparator(",")
    annotation.format setTerminator("#") // applies to every element, not just the end of the whole group.
    annotation.format setRepresentation(Text)
    annotation.format setLengthKind("delimited")
    val simpleElement = 
      new SimpleElement("cell",annotation,null,new Namespaces)
    
    simpleElement.setMaxOccurs(-1)
    val datastream = new RollbackStream(new ByteArrayInputStream(testValue getBytes("ASCII")))
    val annote = new Annotation(null)
    val vmap = new VariableMap()
    val root = new Element("root")
    val results = simpleElement (datastream,annote,vmap,root,-1,Nil)
    (results, testResults).zipped map { 
      (result : Element, testResult : Element) =>
        assertTrue(compare(result, testResult))
      }
   }
}

