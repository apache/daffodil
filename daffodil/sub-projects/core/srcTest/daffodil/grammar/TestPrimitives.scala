package daffodil.grammar

import junit.framework.Assert._

import org.scalatest.junit.JUnit3Suite

import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler

object DFDLUtils {

  def dfdlTestSchema(topLevelAnnotations: Seq[Node], contentElements: Seq[Node]) = {
    val realSchema = <xs:schema xmlns:xs={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns={ targetNS } targetNamespace={ targetNS }>
                       <xs:annotation>
                         <xs:appinfo source={ dfdlURI }>
                           { topLevelAnnotations }
                         </xs:appinfo>
                       </xs:annotation>
                       { contentElements }
                     </xs:schema>
    val realSchemaText = realSchema.toString()
    val real = XML.loadString(realSchemaText)
    real
  }
}

class TestPrimitives extends JUnit3Suite {

  def assertEqualsXMLElements(expected : Node, actual : Node) = {
    val exp = XMLUtils.removeAttributes(expected)
    val act = XMLUtils.removeAttributes(actual)
    assertEquals(exp, act)
  }
  
  def testInitiator {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:initiator="abcd">
      </xs:element>)
    val actual = Compiler.testString(sch, "abcdefgh")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">efgh</e1>"))

    val expected: Node = <e1>efgh</e1>
    assertEqualsXMLElements(expected, actual)
  }
  
  def testTerminator {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:terminator="efgh">
      </xs:element>)
    val actual = Compiler.testString(sch, "abcdefgh")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">abcd</e1>"))

    val expected: Node = <e1>abcd</e1>
    assertEqualsXMLElements(expected, actual)
  }
  
  def testSeparator {
     val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
      		<xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
      		<xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "abcd,efgh")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>abcd</s1><s2>efgh</s2></e1>"))

    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    assertEqualsXMLElements(expected, actual)  
  }

}