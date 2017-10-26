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

package edu.illinois.ncsa.daffodil.api

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import java.io.File
import org.junit.Test
import edu.illinois.ncsa.daffodil.dsom.DFDLElement

class TestDsomCompiler3 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testTmpDirProvided() {
    val sc = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="">
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val tmpDir = new File("./dfdl_tmp")
    if (tmpDir.exists) {
      tmpDir.listFiles.foreach(_.delete)
      tmpDir.delete
    }
    try {
      tmpDir.mkdirs
      val sset = Compiler().compileNode(sc, Some(tmpDir)).sset

      val list = tmpDir.list()
      assertEquals(1, list.length)

      val fileName = list(0)
      assertTrue(fileName.contains(".dfdl.xsd"))

      // Verify things still work using specified tmpDir
      //
      val Seq(schema) = sset.schemas
      val Seq(schemaDoc, _) = schema.schemaDocuments
      val Seq(declFactory) = schemaDoc.globalElementDecls
      val decl = declFactory.forRoot()
      val Seq(ct) = schemaDoc.globalComplexTypeDefs
      assertEquals("example1", ct.name)

      decl.formatAnnotation.asInstanceOf[DFDLElement]
      assertEquals(AlignmentUnits.Bytes, decl.alignmentUnits)
    } finally {
      if (tmpDir.exists) {
        tmpDir.listFiles.foreach(_.delete)
        tmpDir.delete
      }
    }
  }

}
