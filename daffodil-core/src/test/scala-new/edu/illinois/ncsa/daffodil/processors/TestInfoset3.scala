/* Copyright (c) 2014 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.util.Misc
import junit.framework.Assert._
import java.io.FileOutputStream
import java.nio.channels.WritableByteChannel
import java.io.FileWriter
import java.io.File
import java.nio.ByteBuffer
import org.junit.Test

class TestInfosetEvalCacheAndModelGroups {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val ex = XMLUtils.EXAMPLE_NAMESPACE

  def setup(testSchema: scala.xml.Node, xmlInfoset: scala.xml.Node) = {
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[DIComplex]
    infoset
  }

  @Test def testXMLToInfosetModelGroups1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w></ex:list>

    val infoset = setup(testSchema, xmlInfoset)
    val w_erd = infoset.erd.children(0)
    val srd = w_erd.immediateEnclosingTermRuntimeData.get.asInstanceOf[SequenceRuntimeData]
    val diSeq = infoset.modelGroup(srd).asInstanceOf[DISequence]
    val diSeqXML = diSeq.toPseudoXML()
    assertEquals("<dafint:sequence path=\"GlobalComplexTypeDef(example1)::sequence\">", diSeqXML)
    assertEquals("<list xmlns=\"http://example.com\"><w>4</w><?formatInfo <dafint:sequence path=\"GlobalComplexTypeDef(example1)::sequence\">?></list>",
      infoset.toXML(true, true).toString)
  }

  @Test def testXMLToInfosetModelGroupsNest1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:sequence>
            <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:sequence>
        </xs:sequence>
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w></ex:list>

    val infoset = setup(testSchema, xmlInfoset)
    val w_erd = infoset.erd.children(0)
    val srd = w_erd.immediateEnclosingTermRuntimeData.get.asInstanceOf[SequenceRuntimeData]
    val diSeq = infoset.modelGroup(srd).asInstanceOf[DISequence]
    val diSeqXML = diSeq.toPseudoXML()
    println(diSeqXML)
    val srd2 = srd.immediateEnclosingTermRuntimeData.get.asInstanceOf[ModelGroupRuntimeData]
    val diSeq2 = infoset.modelGroup(srd2).asInstanceOf[DISequence]
    val diSeq2XML = diSeq2.toPseudoXML()
    assertEquals("<dafint:sequence path=\"GlobalComplexTypeDef(example1)::sequence::sequence\">", diSeqXML)
    assertEquals("<dafint:sequence path=\"GlobalComplexTypeDef(example1)::sequence\">", diSeq2XML)
    val pi = infoset.toXML(true, true)(0).child(1).asInstanceOf[scala.xml.ProcInstr]
    assertEquals("<?formatInfo <dafint:sequence path=\"GlobalComplexTypeDef(example1)::sequence::sequence\">\n<dafint:sequence path=\"GlobalComplexTypeDef(example1)::sequence\">?>",
      pi.toString())
  }
}
