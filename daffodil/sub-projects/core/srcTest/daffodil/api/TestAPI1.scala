package daffodil.api

import junit.framework.Assert._

import org.scalatest.junit.JUnit3Suite

import scala.xml._
import daffodil.xml.XMLUtil
import daffodil.xml.XMLUtil._
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

class TestDFDLParser extends JUnit3Suite {

  def testParseSimple1() {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" ignoreCase="no" textNumberRep="standard" emptyValueDelimiterPolicy="none" nilValueDelimiterPolicy="both" nilValue="" initiator="" terminator="" separator="" nilKind="logicalValue" occursCountKind="fixed" occursStopValue="-1"/>,
      <xs:element name="e1" type="xs:string" dfdl:initiator="" dfdl:terminator="" dfdl:lengthKind="explicit" dfdl:length="{ 1 }">
      </xs:element>)
    val actual = Compiler.testString(sch, "5")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">5</e1>"))

    val expected: NodeSeq = <e1>5</e1> \\ "e1" // This hack insures we get a nodeseq as our expected result, so comparison will work.
    assertEquals(expected, actual)
  }

  def testParseSequenceOfJustOneScalar() {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" ignoreCase="no" nilValueDelimiterPolicy="both" nilValue="" separatorPolicy="required" initiator="" terminator="" separator="" nilKind="logicalValue" occursCountKind="fixed" occursStopValue="-1"/>,
      <xs:element name="e1" dfdl:initiator="[" dfdl:terminator="]" dfdl:lengthKind="explicit" dfdl:length="{ 1 }">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:int" dfdl:representation="text" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[5]")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1></e1>"))
    val expected: NodeSeq = <e1><s1>5</s1></e1> \\ "e1" // This hack insures we get a nodeseq as our expected result, so comparison will work.
    assertEquals(expected, actual)
  }

  def testParseSequence1() {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format ignoreCase="no" nilValueDelimiterPolicy="both" nilValue="" separatorPolicy="required" initiator="" terminator="" separator="" nilKind="logicalValue" lengthKind="implicit" occursStopValue="-1"/>,
      <xs:element name="e1" dfdl:initiator="[" dfdl:terminator="]">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:int" dfdl:representation="text" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="2" maxOccurs="2"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[5,6]")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1><s1>6</s1></e1>"))
    val expected = <e1><s1>5</s1><s1>6</s1></e1>
    assertEquals(expected, actual)
  }
}