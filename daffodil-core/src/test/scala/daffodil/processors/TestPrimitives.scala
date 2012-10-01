package daffodil.processors

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util.TestUtils
import daffodil.dsom.SchemaSet

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
      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:initiator="abcd">
      </xs:element>)
    val actual = Compiler.testString(sch, "abcdefgh")
    assertTrue(actual.canProceed)
    val actualString = actual.result.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">efgh</e1>"))

    val expected: Node = <e1>efgh</e1>
    assertEqualsXMLElements(expected, actual.result)
  }
  
  def testTerminator {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:terminator="efgh">
      </xs:element>)
    val actual = Compiler.testString(sch, "abcdefgh")
    val actualString = actual.result.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">abcd</e1>"))

    val expected: Node = <e1>abcd</e1>
    assertEqualsXMLElements(expected, actual.result)
  }
  
  def testSeparator {
     val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
      		<xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
      		<xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "abcd,efgh")
    val actualString = actual.result.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>abcd</s1><s2>efgh</s2></e1>"))

    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    assertEqualsXMLElements(expected, actual.result)  
  }
  
  def testLengthKindDelimited {
     val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
      		<xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" />
      		<xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "abcd,efgh")
    val actualString = actual.result.toString
    // println(actualString)
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>abcd</s1><s2>efgh</s2></e1>"))

    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    assertEqualsXMLElements(expected, actual.result)  
  }
  
  def testLengthKindDelimited2 {
     val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="%WSP;%WSP*;\%NL;%WSP;%WSP*;" dfdl:separatorPosition="infix">
      		<xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" />
      		<xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "abcd  \\\n  efgh")
    val actualString = actual.result.toString
    // println(actualString)
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>abcd</s1><s2>efgh</s2></e1>"))

    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    assertEqualsXMLElements(expected, actual.result)  
  }
  
  def testLengthKindDelimited3 {
     val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="}}}" dfdl:separatorPosition="infix">
      		<xs:element name="s1" dfdl:lengthKind="delimited">
      			<xs:complexType>
      				<xs:sequence dfdl:separator="}" dfdl:separatorPosition="infix">
      					<xs:element name="ss1" type="xs:string" dfdl:lengthKind="delimited" />
      					<xs:element name="ss2" type="xs:string" dfdl:lengthKind="delimited" />
      				</xs:sequence>
      			</xs:complexType>
      		</xs:element>
      		<xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "abcd}efgh}}}ijkl")
    val actualString = actual.result.toString
    // println(actualString)
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1><ss1>abcd</ss1><ss2>efgh</ss2></s1><s2>ijkl</s2></e1>"))

    val expected: Node = <e1><s1><ss1>abcd</ss1><ss2>efgh</ss2></s1><s2>ijkl</s2></e1>
    assertEqualsXMLElements(expected, actual.result)  
  }
  
  def testDelimiterInheritance {
     val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:defineFormat name="config">
                           <dfdl:format initiator="" terminator="" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no"  textBidi="no" floating="no"   byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" 
                               occursCountKind="implicit" lengthUnits="bytes"
                                  initiatedContent="no" sequenceKind="ordered" ignoreCase="no"
                                  textPadKind="none" textTrimKind="none" textStandardBase="10"
                                  textNumberJustification="right" separatorPosition="infix"
                                  separatorPolicy="required" escapeSchemeRef="" lengthKind="delimited"
                                  documentFinalTerminatorCanBeMissing="no" outputNewLine="%LF;"
                                  textNumberRep="standard" nilValueDelimiterPolicy="both"
                                  textNumberRounding="pattern" />
                     </dfdl:defineFormat>
      <dfdl:defineFormat name="baseString">
                           <dfdl:format byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" lengthUnits="bytes" 
                    initiator="" terminator="" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no"  textBidi="no" floating="no" 
                                  ignoreCase="no" textPadKind="none" textTrimKind="none"
                                  textStandardBase="10" textStringJustification="right"
                                  escapeSchemeRef="" lengthKind="delimited" occursCountKind="implicit"/>
                     </dfdl:defineFormat>
                     <dfdl:defineFormat name="inheritance">
                           <dfdl:format byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" lengthUnits="bytes" 
                    initiator="" terminator="}" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no"  textBidi="no" floating="no" 
                                  ignoreCase="no" textPadKind="none" textTrimKind="none"
                                  textStandardBase="10" textStringJustification="right"
                                  escapeSchemeRef="" lengthKind="delimited" occursCountKind="implicit"/>
                     </dfdl:defineFormat>
         <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="root" dfdl:lengthKind="implicit" dfdl:ref="config" dfdl:initiator="{">
              <xs:complexType>
                     <xs:sequence dfdl:ref="config" dfdl:separator="," dfdl:terminator="::" >
                           <xs:element name="e1" type="xs:string" dfdl:ref="baseString"
                                  dfdl:representation="text" />
                           <xs:element name="e2" type="xs:string" dfdl:ref="baseString"
                                  dfdl:representation="text" />
                           <xs:element name="e3" dfdl:ref="baseString"
                                  dfdl:representation="text" >
                                  <xs:complexType>
                                         <xs:sequence dfdl:ref="config" dfdl:separator="/" dfdl:terminator="//">
                                                <xs:element name="e3_1" type="xs:string" dfdl:ref="baseString"
                                                       dfdl:representation="text" dfdl:terminator="."/>
                                                <xs:element name="e3_2" type="xs:string" dfdl:ref="inheritance"
                                                       dfdl:representation="text" />
                                         </xs:sequence>
                                  </xs:complexType>
                           </xs:element>
                     </xs:sequence>
              </xs:complexType>
       </xs:element>)
    val actual = Compiler.testString(sch, "{a,b,c./d}//::")
    val actualString = actual.result.toString
    // println(actualString)
    assertTrue(actualString.contains("<root")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><e1>a</e1><e2>b</e2><e3><e3_1>c</e3_1><e3_2>d</e3_2></e3></root>"))
    
    // <root><e1></e1><e2></e2><e3><e3_1></e3_1><e3_2></e3_2></e3></root>
    // a,b,c./d//::

    val expected: Node = <root><e1>a</e1><e2>b</e2><e3><e3_1>c</e3_1><e3_2>d</e3_2></e3></root>
    assertEqualsXMLElements(expected, actual.result)  
  }
  
  def testEntityReplacementSeparator {
     val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="%NUL;" dfdl:separatorPosition="infix">
      		<xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
      		<xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "abcd\u0000efgh")
    val actualString = actual.result.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>abcd</s1><s2>efgh</s2></e1>"))

    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    assertEqualsXMLElements(expected, actual.result)  
  }
  
  def testEntityReplacementInitiator {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:initiator="%NUL;">
      </xs:element>)
    val actual = Compiler.testString(sch, "\u0000efgh")
    assertTrue(actual.canProceed)
    val actualString = actual.result.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">efgh</e1>"))

    val expected: Node = <e1>efgh</e1>
    assertEqualsXMLElements(expected, actual.result)
  }
  
  def testEntityReplacementTerminator {
    val sch = DFDLUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:terminator="%NUL;">
      </xs:element>)
    val actual = Compiler.testString(sch, "abcd\u0000")
    val actualString = actual.result.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">abcd</e1>"))

    val expected: Node = <e1>abcd</e1>
    assertEqualsXMLElements(expected, actual.result)
  }

  def testNotByteAlignedCharactersErrorDetected() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val pf = new Compiler().compile(sch)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]

    val edecl = dp.rootElem
    val d = Compiler.stringToReadableByteChannel("42")
    val initialState = PState.createInitialState(edecl, d, bitOffset = 3)
    val resState = dp.parse(initialState)
    assertTrue(resState.isError)
    val Seq(err) = resState.getDiagnostics
    assertTrue(err.isInstanceOf[ParseError])
    assertTrue(err.toString.contains("byte"))
  }
  

}