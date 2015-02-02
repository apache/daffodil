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

import scala.xml.parsing.ConstructingParser
import scala.xml._
import java.net.URI
import scala.io.Source
import org.apache.commons.io.input.XmlStreamReader
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.io.Position

/**
 * Loads XML using the Scala ConstructingParser for XML.
 *
 * Necessary as this xml loading technique handles the <![CDATA[...]]>
 * properly, creating PCData nodes for the contents of these, and not otherwise
 * messing with the contents.
 *
 * Xerces, unfortunately, messes with the contents of these CDATA regions,
 * normalizes whitespace inside them, and generally makes it impossible to do things
 * in XML that depend on line-structure of element content to being preserved.
 *
 * We have places where line structure matters. Specifically regular expressions
 * have a free-form syntax with comments that extend to end-of-line. If we always
 * wrap these with CDATA, and use this loader, not Xerces, then these will be
 * preserved properly.
 *
 * Also, enhanced to capture file/line/column info for every element and add it
 * as attributes onto each XML element.
 *
 * The way the constructing loader (aka ConstructingParser (for XML))
 * gets positions is different. It is given just an offset into the document file/stream,
 * and it therefore must synthesize line number/col number info itself.
 */
class DaffodilConstructingLoader(uri: URI, errorHandler: org.xml.sax.ErrorHandler)
  extends ConstructingParser({
    //
    // Note: we must open the XML carefully since it might be in some non
    // default encoding (we have tests that have UTF-16 for example)
    //
    val is = uri.toURL.openStream()
    val rdr = new XmlStreamReader(is) // apache has a good thing for determining XML charset
    val csName = rdr.getEncoding
    rdr.close()
    val source = Source.fromURL(uri.toURL, csName) // tbd: can one create a Source somehow directly from the rdr?
    source
  }, true) {

  override def reportSyntaxError(pos: Int, msg: String) {
    val line = Position.line(pos)
    val col = Position.column(pos)
    val exc = new org.xml.sax.SAXParseException(msg, null, uri.toString, line, col)
    errorHandler.error(exc)
  }
  /**
   * When elements are to be created, we append file/col/line number info
   * if it isn't already present.
   */
  override def elem(posLineAndCol: Int, pre: String, label: String, attrs: MetaData,
    scope: NamespaceBinding, empty: Boolean, nodes: NodeSeq): NodeSeq = {
    //
    // The posLineAndCol argument is not an offset into the file. It is a single integer 
    // into which both col and line info are packed.
    // see scala.io.Position.
    //

    val info = new FileLineColInfo(pre, label, scope, attrs,
      Position.line(posLineAndCol).toString, Position.column(posLineAndCol).toString, uri.toString)

    val e = super.elem(posLineAndCol, pre, label, attrs, info.newScope, empty, nodes).asInstanceOf[Elem]

    val res = e % info.newLineAttr % info.newColAttr % info.newFileAttr
    res
  }

  def load(): Node = {
    this.initialize
    val doc = this.document()
    val res =
      if (doc == null) null
      else doc.docElem
    res
  }
}