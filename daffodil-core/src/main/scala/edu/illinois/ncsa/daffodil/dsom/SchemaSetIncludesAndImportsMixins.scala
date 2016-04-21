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

import scala.collection.immutable.ListMap
import edu.illinois.ncsa.daffodil.util._
import IIUtils._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.Delay

/**
 * Mixin for SchemaSet
 */

trait SchemaSetIncludesAndImportsMixin { self: SchemaSet =>

  /**
   * Let's take the list of file names given, and make a fake schema
   * document with import statements for them. Then the algorithms
   * are all isolated to just the SchemaDocument class and the Include
   * and Import classes.
   */
  lazy val fakeXMLSchemaDocument = {
    val xsd = XMLUtils.XSD_NAMESPACE.toString

    // Any time we synthesize xml we have to grab the namespace definitions and
    // make sure we drag them along onto the new structures.
    val fakeImportStatementsXML = schemaSources.map { ssrc =>
      <xs:import schemaLocation={ ssrc.uriForLoading.toString } xmlns:xs={ xsd }/>
    }

    val fakeSchemaDocXML =
      <xs:schema xmlns:xs={ xsd }>{ fakeImportStatementsXML }</xs:schema>

    val initialEmptyIIMap: IIMap = Delay(ListMap.empty)

    val fakeSD = new XMLSchemaDocument(fakeSchemaDocXML, self, None, None, initialEmptyIIMap, isBootStrapSD = true)
    fakeSD
  }

  def allSchemaDocuments = LV('allSchemaDocuments) {
    allSchemaFiles.map { _.iiSchemaDocument }
  }.value

  def allSchemaFiles = LV('allSchemaFiles) {
    val fd = fakeXMLSchemaDocument //bootstrap
    val sa = fd.seenAfter
    val sfl = sa.value.flatMap {
      case (_, ii) => {
        val sf = ii.iiSchemaFileMaybe // maybe not if we've already seen this file for the same namespace.
        sf
      }
    }.toList
    sfl
  }.value

}
