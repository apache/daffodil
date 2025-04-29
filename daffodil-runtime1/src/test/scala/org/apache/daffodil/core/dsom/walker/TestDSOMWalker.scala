/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom.walker

import org.apache.daffodil.core.compiler.{ Compiler, ProcessorFactory }
import org.apache.daffodil.lib.util._
import org.apache.daffodil.runtime1.dsom.walker._

import org.junit.Assert._
import org.junit.Test

class TestDSOMWalker {

  @Test def testComplexTypesAndEndEvents(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="ex:GeneralFormat"
                   alignment="implicit" alignmentUnits="bits" occursCountKind="implicit"
                   lengthKind="delimited" encoding="ASCII"/>,
      <xs:element name="PersonData">
        <xs:complexType>
          <xs:sequence>
            <xs:choice>
              <xs:element name="age" type="xs:int" minOccurs="1" maxOccurs="1"/>
            </xs:choice>
            <xs:group ref="testGroup" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="testGroup">
        <xs:sequence />
      </xs:group>
    )
    val pf: ProcessorFactory = Compiler().compileNode(testSchema)
    assertEquals(
      s"This basic Schema: $testSchema should compile; here are some diagnostics: ${pf.getDiagnostics}",
      false,
      pf.isError
    )
    val walker: BasicWalker = new BasicWalker(ignoreEndEvents = false)
    walker.walkFromRoot(pf.rootView)
    val nodeStack: List[AnyRef] = walker.nodeArr.toList
    assertEquals(s"Node Stack $nodeStack should have 16 elements", 16, nodeStack.size)
    assertTrue(
      "Should have received a start event for the overall traversal",
      nodeStack.head.isInstanceOf[RootView]
    )
    assertTrue("The Root element was not of type RootView", nodeStack(1).isInstanceOf[RootView])
    assertEquals(
      "The root element should be named 'PersonData'",
      "PersonData",
      nodeStack(1).asInstanceOf[RootView].name
    )
    assertTrue(
      "The root element should contain a complexType wrapper child",
      nodeStack(2).isInstanceOf[ComplexTypeView]
    )
    assertTrue(
      "The complexType element should contain a Sequence child",
      nodeStack(3).isInstanceOf[SequenceView]
    )
    assertTrue(
      "The Sequence element should contain a Choice child",
      nodeStack(4).isInstanceOf[ChoiceView]
    )
    assertTrue(
      "The Choice element should contain an Element child",
      nodeStack(5).isInstanceOf[ElementBaseView]
    )
    assertEquals(
      "The Element child should be named 'age'",
      "age",
      nodeStack(5).asInstanceOf[ElementBaseView].name
    )
    assertEquals(
      "The Element child should be a simple type",
      true,
      nodeStack(5).asInstanceOf[ElementBaseView].isSimpleType
    )
    assertTrue(
      "The 'age' element should have a SimpleTypeView",
      nodeStack(6).isInstanceOf[SimpleTypeView]
    )
    assertTrue(
      "Should have received an end event for the Simple Type",
      nodeStack(7).isInstanceOf[SimpleTypeView]
    )
    assertTrue(
      "Should have received an end event for the 'age' element",
      nodeStack(8).isInstanceOf[ElementBaseView]
    )
    assertTrue(
      "Should have received an end event for the Choice element",
      nodeStack(9).isInstanceOf[ChoiceView]
    )
    assertTrue(
      "The Sequence element should contain a second child that is a GroupRef",
      nodeStack(10).isInstanceOf[GroupRefView]
    )
    assertTrue(
      "Should have received an end event for the GroupRef element",
      nodeStack(11).isInstanceOf[GroupRefView]
    )
    assertTrue(
      "Should have received an end event for the Sequence element",
      nodeStack(12).isInstanceOf[SequenceView]
    )
    assertTrue(
      "Should have received an end event for the complexType wrapper element",
      nodeStack(13).isInstanceOf[ComplexTypeView]
    )
    assertTrue(
      "Should have received an end event for the Root element",
      nodeStack(14).isInstanceOf[RootView]
    )
    assertTrue(
      "Should have received an end event for the overall traversal",
      nodeStack(15).isInstanceOf[RootView]
    )
  }

  private def getSuffix(ordinal: Int): String = {
    if ((ordinal % 100) / 10 == 1) "th"
    else if (ordinal % 10 == 1) "st"
    else if (ordinal % 10 == 2) "nd"
    else if (ordinal % 10 == 3) "rd"
    else "th"
  }

  @Test def testAllSimpleTypes(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="ex:GeneralFormat"
                   alignment="implicit" alignmentUnits="bits" occursCountKind="implicit"
                   lengthKind="implicit" encoding="ASCII"/>,
      <xs:element name="AllSimples">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="stringField" type="xs:string" minOccurs="1" maxOccurs="1" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="booleanField" type="xs:boolean" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:textBooleanTrueRep="1" dfdl:textBooleanFalseRep="0" />
            <xs:element name="byteField" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="shortField" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="intField" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="longField" type="xs:long" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="unsignedByteField" type="xs:unsignedByte" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="unsignedShortField" type="xs:unsignedShort" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="unsignedIntField" type="xs:unsignedInt" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="unsignedLongField" type="xs:unsignedLong" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="nonNegativeIntegerField" type="xs:nonNegativeInteger" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="integerField" type="xs:integer" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="floatField" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="doubleField" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="decimalField" type="xs:decimal" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="hexBinaryField" type="xs:hexBinary" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="anyURIField" type="xs:anyURI" dfdl:lengthKind="explicit" dfdl:length="1" dfdlx:objectKind="bytes"/>
            <xs:element name="dateTimeField" type="xs:dateTime" dfdl:calendarPattern="yyyy.MM.dd G 'at' HH:mm:ss ZZZZ"
                        dfdl:calendarPatternKind="explicit" dfdl:lengthKind="explicit" dfdl:length="35"/>
            <xs:element name="dateField" type="xs:date" dfdl:calendarPattern="EEEE, MMM d, ''yy" dfdl:calendarPatternKind="explicit"
                        dfdl:lengthKind="explicit" dfdl:length="23"/>
            <xs:element name="timeField" type="xs:time" dfdl:calendarPattern="h:mm a" dfdl:calendarPatternKind="explicit"
                        dfdl:lengthKind="explicit" dfdl:length="8"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val pf: ProcessorFactory = Compiler().compileNode(testSchema)
    assertEquals(
      s"This basic Schema $testSchema should compile; here are some diagnostics: ${pf.getDiagnostics}",
      false,
      pf.isError
    )
    val walker: BasicWalker = new BasicWalker(false, true)
    walker.walkFromRoot(pf.rootView)
    val simpleTypes: List[Class[_ <: PrimTypeView]] = List(
      classOf[StringView],
      classOf[BooleanView],
      classOf[ByteView],
      classOf[ShortView],
      classOf[IntView],
      classOf[LongView],
      classOf[UnsignedByteView],
      classOf[UnsignedShortView],
      classOf[UnsignedIntView],
      classOf[UnsignedLongView],
      classOf[NonNegativeIntegerView],
      classOf[IntegerView],
      classOf[FloatView],
      classOf[DoubleView],
      classOf[DecimalView],
      classOf[HexBinaryView],
      classOf[AnyURIView],
      classOf[DateTimeView],
      classOf[DateView],
      classOf[TimeView]
    )
    val nodeStack: List[AnyRef] = walker.nodeArr.toList
    assertEquals(
      s"Node Stack $nodeStack did not have the expected number of elements",
      2 * simpleTypes.size + 3,
      nodeStack.size
    )
    for (index <- 1 to simpleTypes.size) {
      val elementIndex = 2 * index + 1
      val simpleTypeIndex = 2 * index + 2
      assertTrue(
        s"The $elementIndex${getSuffix(elementIndex)} element in the stack should be an Element",
        nodeStack(elementIndex).isInstanceOf[ElementBaseView]
      )
      val className: String = simpleTypes(index - 1).getSimpleName
      val withoutView: String = className.substring(0, className.length - 4)
      val fieldName: String =
        withoutView.charAt(0).toLower +: (withoutView.substring(1) + "Field")
      assertEquals(
        s"The $elementIndex${getSuffix(elementIndex)} element in the stack should be named '$fieldName'",
        fieldName,
        nodeStack(elementIndex).asInstanceOf[ElementBaseView].name
      )
      assertTrue(
        s"The $simpleTypeIndex${getSuffix(simpleTypeIndex)} element in the stack should be a Simple type",
        nodeStack(simpleTypeIndex).isInstanceOf[SimpleTypeView]
      )
      assertTrue(
        s"The $simpleTypeIndex${getSuffix(simpleTypeIndex)} element in the stack should be of type '$className'",
        simpleTypes(index - 1).isInstance(
          nodeStack(simpleTypeIndex).asInstanceOf[SimpleTypeView].primType
        )
      )
    }
  }

  @Test def testOptionalField(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="ex:GeneralFormat"
                   alignment="implicit" alignmentUnits="bits" occursCountKind="implicit"
                   lengthKind="delimited" encoding="ASCII"/>,
      <xs:element name="DataList">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="data" type="xs:string" minOccurs="0" maxOccurs="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val pf: ProcessorFactory = Compiler().compileNode(testSchema)
    assertEquals(
      s"This basic Schema: $testSchema should compile; here are some diagnostics: ${pf.getDiagnostics}",
      false,
      pf.isError
    )
    val walker: BasicWalker = new BasicWalker(true, true)
    walker.walkFromRoot(pf.rootView)
    val nodeStack: List[AnyRef] = walker.nodeArr.toList
    assertEquals(
      s"Node Stack $nodeStack did not have the expected number of elements",
      3,
      nodeStack.size
    )
    assertTrue(
      "The 3rd element in the stack should be an Element",
      nodeStack(2).isInstanceOf[ElementBaseView]
    )
    assertEquals(
      "The 3rd element in the stack should be named 'data'",
      "data",
      nodeStack(2).asInstanceOf[ElementBaseView].name
    )
    assertEquals(
      "The 'data' element should be optional",
      true,
      nodeStack(2).asInstanceOf[ElementBaseView].isOptional
    )
  }

}
