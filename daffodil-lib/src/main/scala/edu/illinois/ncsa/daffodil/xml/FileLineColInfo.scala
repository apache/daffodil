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

package edu.illinois.ncsa.daffodil.xml

import scala.xml._
import scala.util.parsing.input.Position
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.net.URI

/**
 * Common logic for dealing with adding file/line/col attributes to XML
 * while loading.
 *
 * Shared by both Xerces (used for validation), and the ConstructingLoader
 * (used to actually construct the scala XML nodes).
 */
class FileLineColInfo(pre: String, label: String,
  scope: NamespaceBinding, attrs: MetaData,
  line: String,
  col: String,
  uriString: String) {

  // If we're the xs:schema node, then append the attribute for _file_ as well.

  private val nsURI = NS(scope.getURI(pre))
  private val isXSSchemaNode = (label == "schema" && nsURI == XMLUtils.XSD_NAMESPACE)
  private val isTDMLTestSuiteNode = (label == "testSuite" && nsURI == XMLUtils.TDML_NAMESPACE)
  private val isFileRootNode = isXSSchemaNode || isTDMLTestSuiteNode

  //
  // If this parser is at all sane in node construction, then it creates
  // nodes passing in the scope of the parent enclosing element. If that 
  // has the dafint binding in it already, we don't add it again.
  // 
  private lazy val scopeWithDafInt = {
    val intPre = scope.getPrefix(XMLUtils.INT_NS.toString)
    if (intPre == null)
      NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS.toString, scope)
    else {
      Assert.usage(intPre == XMLUtils.INT_PREFIX) // can't deal with some other binding for dafint.
      scope
    }
  }

  private val haveFileName = isFileRootNode && uriString != ""

  val lineAttr = attrs.get(XMLUtils.INT_NS.toString, scopeWithDafInt, XMLUtils.LINE_ATTRIBUTE_NAME).map { _.text }
  val colAttr = attrs.get(XMLUtils.INT_NS.toString, scopeWithDafInt, XMLUtils.COLUMN_ATTRIBUTE_NAME).map { _.text }
  val fileAttr = attrs.get(XMLUtils.INT_NS.toString, scopeWithDafInt, XMLUtils.FILE_ATTRIBUTE_NAME).map { _.text }

  val alreadyHasLocation = lineAttr.isDefined || colAttr.isDefined

  val newScope =
    if (alreadyHasLocation) scope
    else scopeWithDafInt

  private def makeLineAttr() =
    Attribute(XMLUtils.INT_PREFIX, XMLUtils.LINE_ATTRIBUTE_NAME, Text(line), Null)

  private def makeColAttr() =
    Attribute(XMLUtils.INT_PREFIX, XMLUtils.COLUMN_ATTRIBUTE_NAME, Text(col), Null)

  private def makeFileAttr() = {
    Attribute(XMLUtils.INT_PREFIX, XMLUtils.FILE_ATTRIBUTE_NAME, Text(uriString), Null)
  }

  val newLineAttr =
    if (!alreadyHasLocation) makeLineAttr() else Null

  val newColAttr =
    if (!alreadyHasLocation) makeColAttr() else Null

  val newFileAttr =
    if (!haveFileName) Null
    else if (!alreadyHasLocation) makeFileAttr() else Null
}