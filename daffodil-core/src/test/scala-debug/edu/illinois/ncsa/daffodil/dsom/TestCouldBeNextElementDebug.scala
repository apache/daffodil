/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler._
import junit.framework.Assert.assertEquals

class TestCouldBeNextElementDebug {

  @Test def testCouldBeNextElement_2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="array" minOccurs="0" maxOccurs="unbounded" dfdl:occursCountKind="implicit">
              <xs:complexType>
                <xs:choice>
                  <xs:element name="ch1" type="xs:string"/>
                  <xs:element name="ch2" type="xs:string"/>
                  <xs:sequence />
                </xs:choice>
              </xs:complexType>
            </xs:element>
            <xs:element name="after" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()

    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val rootSeq = rootCT.modelGroup.asInstanceOf[Sequence]
    val Seq(array: LocalElementBase, after: LocalElementBase) = rootSeq.groupMembers
    val arrayCT = array.elementComplexType
    val choice = arrayCT.modelGroup.asInstanceOf[Choice]
    val Seq(ch1: LocalElementBase, ch2: LocalElementBase, choiceSeq: Sequence) = choice.groupMembers

    assertEquals(0, root.couldBeNextElementInInfoset.length)
    assertEquals(2, root.couldBeFirstChildElementInInfoset.length)
    assertEquals("array", root.couldBeFirstChildElementInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("after", root.couldBeFirstChildElementInInfoset(1).asInstanceOf[LocalElementBase].name)

    assertEquals(2, array.couldBeFirstChildElementInInfoset.length)
    assertEquals("ch1", array.couldBeFirstChildElementInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("ch2", array.couldBeFirstChildElementInInfoset(1).asInstanceOf[LocalElementBase].name)

    assertEquals(2, array.couldBeNextElementInInfoset.length)
    assertEquals("array", array.couldBeNextElementInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("after", array.couldBeNextElementInInfoset(1).asInstanceOf[LocalElementBase].name)
   
    assertEquals(0, after.couldBeFirstChildElementInInfoset.length)
    assertEquals(0, after.couldBeNextElementInInfoset.length)

    assertEquals(0, ch1.couldBeFirstChildElementInInfoset.length)
    assertEquals(0, ch1.couldBeNextElementInInfoset.length)

    assertEquals(0, ch2.couldBeFirstChildElementInInfoset.length)
    assertEquals(0, ch2.couldBeNextElementInInfoset.length)

    assertEquals(3, choice.choiceBranchMap.size)
    assertEquals(ch1.runtimeData, choice.choiceBranchMap(ch1.namedQName))
    assertEquals(ch2.runtimeData, choice.choiceBranchMap(ch2.namedQName))
    assertEquals(choiceSeq.runtimeData, choice.choiceBranchMap(array.namedQName))
  }

}

