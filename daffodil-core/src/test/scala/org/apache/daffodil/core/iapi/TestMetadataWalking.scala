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

package org.apache.daffodil.core.iapi

import scala.collection.mutable.ArrayBuffer
import scala.util.Using
import scala.xml.Elem

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.util._
import org.apache.daffodil.runtime1.iapi.ChoiceMetadata
import org.apache.daffodil.runtime1.iapi.ComplexElementMetadata
import org.apache.daffodil.runtime1.iapi.DFDL.ParseResult
import org.apache.daffodil.runtime1.iapi.ElementMetadata
import org.apache.daffodil.runtime1.iapi.InfosetArray
import org.apache.daffodil.runtime1.iapi.InfosetComplexElement
import org.apache.daffodil.runtime1.iapi.InfosetElement
import org.apache.daffodil.runtime1.iapi.InfosetItem
import org.apache.daffodil.runtime1.iapi.InfosetSimpleElement
import org.apache.daffodil.runtime1.iapi.Metadata
import org.apache.daffodil.runtime1.iapi.MetadataHandler
import org.apache.daffodil.runtime1.iapi.SequenceMetadata
import org.apache.daffodil.runtime1.iapi.SimpleElementMetadata
import org.apache.daffodil.runtime1.infoset.InfosetOutputter
import org.apache.daffodil.runtime1.processors.DataProcessor

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class TestMetadataWalking {

  def compileAndWalkMetadata(schema: Elem, mh: MetadataHandler): DataProcessor = {
    val dp = TestUtils.compileSchema(schema)
    assertTrue(!dp.isError)
    dp.walkMetadata(mh)
    dp
  }

  def parseAndWalkData(dp: DataProcessor, infosetOutputter: InfosetOutputter)(
    data: Array[Byte]
  ): ParseResult = {
    Using.resource(InputSourceDataInputStream(data)) { isdis =>
      val res = dp.parse(isdis, infosetOutputter)
      res
    }
  }

  class GatherMetadata extends MetadataHandler {

    private val buf = new ArrayBuffer[Metadata]();

    def getResult: Seq[Metadata] = {
      val res: Seq[Metadata] = buf.toVector // makes a copy
      buf.clear()
      res
    }

    override def simpleElementMetadata(m: SimpleElementMetadata): Unit = buf += m

    override def startComplexElementMetadata(m: ComplexElementMetadata): Unit = buf += m

    override def endComplexElementMetadata(m: ComplexElementMetadata): Unit = buf += m

    override def startSequenceMetadata(m: SequenceMetadata): Unit = buf += m

    override def endSequenceMetadata(m: SequenceMetadata): Unit = buf += m

    override def startChoiceMetadata(m: ChoiceMetadata): Unit = buf += m

    override def endChoiceMetadata(m: ChoiceMetadata): Unit = buf += m
  }

  class GatherData extends InfosetOutputter {

    private val buf = new ArrayBuffer[InfosetItem]

    def getResult: Seq[InfosetItem] = {
      val res = buf.toVector
      reset()
      res
    }

    override def reset(): Unit = { buf.clear() }

    override def startDocument(): Unit = {}

    override def endDocument(): Unit = {}

    override def startSimple(diSimple: InfosetSimpleElement): Unit = { buf += diSimple }

    override def endSimple(diSimple: InfosetSimpleElement): Unit = {}

    override def startComplex(complex: InfosetComplexElement): Unit = { buf += complex }

    override def endComplex(complex: InfosetComplexElement): Unit = { buf += complex }

    override def startArray(array: InfosetArray): Unit = { buf += array }

    override def endArray(array: InfosetArray): Unit = { buf += array }
  }

  @Test def testMetadataWalk_DataWalk_01(): Unit = {
    val gatherData = new GatherData
    val gatherMetadata = new GatherMetadata
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="ex:GeneralFormat" lengthKind="implicit"/>,
      <xs:element name="e1"  dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence dfdl:separator=";" dfdl:terminator=";">
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="delimited" maxOccurs="4" minOccurs="0" dfdl:occursCountKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      useTNS = false
    )
    val dp = compileAndWalkMetadata(sch, gatherMetadata)
    val md = gatherMetadata.getResult
    val mdQNames = md.map {
      case e: ElementMetadata => e.toQName
      case seq: SequenceMetadata => "seq"
      case cho: ChoiceMetadata => "cho"
    }
    assertEquals("Vector(e1, seq, s1, seq, e1)", mdQNames.toString)
    val parser: Array[Byte] => ParseResult = parseAndWalkData(dp, gatherData)
    val inputData = "5;6;7;8;.".getBytes("utf-8")
    val res = parser(inputData)
    val infosetItems = gatherData.getResult
    val itemQNames = infosetItems.flatMap {
      case e: InfosetElement => Seq(e.metadata.toQName)
      case e: InfosetArray => Seq(e.metadata.name + "_array")
      case _ => Nil
    }
    assertEquals("Vector(e1, s1_array, s1, s1, s1, s1, s1_array, e1)", itemQNames.toString)
    val itemValues = infosetItems.flatMap {
      case e: InfosetSimpleElement => Seq(e.getText)
      case _ => Nil
    }
    assertEquals("5678", itemValues.mkString)
  }

  /**
   * Shows that there are no hidden elements to deal with in
   * the metadata walk nor the data walk.
   */
  @Test def testMetadataWalk_DataWalk_NoHidden(): Unit = {
    val gatherData = new GatherData
    val gatherMetadata = new GatherMetadata
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="ex:GeneralFormat" lengthKind="delimited"/>,
      Seq(
        <xs:group name="len">
        <xs:sequence>
          <xs:element name="len" type="xs:unsignedInt"
            dfdl:outputValueCalc='{ dfdl:valueLength(../s1[1], "bytes") }'/>
        </xs:sequence>
      </xs:group>,
        <xs:element name="e1" dfdl:terminator=".">
        <xs:complexType>
          <xs:choice dfdl:choiceDispatchKey='{ "ints" }'>
            <xs:sequence dfdl:choiceBranchKey="strings"/>
            <xs:sequence dfdl:separator=";" dfdl:choiceBranchKey="ints">
              <xs:sequence dfdl:hiddenGroupRef="ex:len"/>
              <xs:element name="s1" type="xs:int"
                          dfdl:lengthKind="explicit" dfdl:length="{ ../len }"
                          maxOccurs="4" minOccurs="0" dfdl:occursCountKind="implicit"/>
            </xs:sequence>
           </xs:choice>
        </xs:complexType>
      </xs:element>
      ),
      useTNS = false,
      useDefaultNamespace = false,
      elementFormDefault = "unqualified"
    )
    val dp = compileAndWalkMetadata(sch, gatherMetadata)
    val md = gatherMetadata.getResult
    val mdQNames = md.map {
      case e: ElementMetadata => e.toQName
      case seq: SequenceMetadata => "seq"
      case cho: ChoiceMetadata => "cho"
    }
    assertEquals(
      "Vector(ex:e1, cho, seq, seq, seq, s1, seq, cho, ex:e1)",
      mdQNames.toString
    )
    val parser: Array[Byte] => ParseResult = parseAndWalkData(dp, gatherData)
    val inputData = "1;5;6;7;8.".getBytes("utf-8")
    val res = parser(inputData)
    val infosetItems = gatherData.getResult
    val itemQNames = infosetItems.flatMap {
      case e: InfosetElement => Seq(e.metadata.toQName)
      case e: InfosetArray => Seq(e.metadata.name + "_array")
      case _ => Nil
    }
    assertEquals(
      "Vector(ex:e1, s1_array, s1, s1, s1, s1, s1_array, ex:e1)",
      itemQNames.toString
    )
    val itemValues = infosetItems.flatMap {
      case e: InfosetSimpleElement => Seq(e.getText)
      case _ => Nil
    }
    assertEquals("5678", itemValues.mkString)
  }

}
