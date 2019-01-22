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

package org.apache.daffodil.xml

import scala.xml.parsing.ConstructingParser
import scala.xml._
import java.net.URI
import scala.io.Source
import org.apache.commons.io.input.XmlStreamReader
import org.apache.daffodil.exceptions.Assert

/**
 * Scala 2.11 deprecated the Position object so it is no longer public.
 * However, we still need a way to decode the integer positions that contain
 * both line/col information. Scala provided no other way to decode this, so
 * this copies the relevant bits from:
 *
 * https://github.com/scala/scala/blob/2.11.x/src/library/scala/io/Position.scala
 *
 * Note that if scala ever changes thes values, line/column numbers will be off
 */
object Position {
  /** Number of bits used to encode the line number */
  final val LINE_BITS = 20
  /** Number of bits used to encode the column number */
  final val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
  /** Mask to decode the line number */
  final val LINE_MASK = (1 << LINE_BITS) - 1
  /** Mask to decode the column number */
  final val COLUMN_MASK = (1 << COLUMN_BITS) - 1

  final def line(pos: Int): Int = (pos >> COLUMN_BITS) & LINE_MASK

  final def column(pos: Int): Int = pos & COLUMN_MASK
}

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

  // This one line is a bit of a hack to get consistent line numbers. The
  // scala-xml libary reads XML from a scala.io.Source which maintains private
  // line/col information about where in the Source we are reading from (i.e.
  // scala.io.Source.pos). The problem is that when CDATA or a processing
  // instruction is encountered, the library switches to a custom
  // "WithLookAhead" scala.io.Source that buffers the original Source. This
  // lookahead Source allows it to peek ahead a few characters, which is used
  // to find the end of CDATA and processing instructions. The problem is that
  // when it switches to this new Source, we lose position information since
  // that information is private to each Source. This causes line information
  // to reset to zero when the first CDATA or processing instruction is found.
  // And there is no good way to copy position information from one source to
  // another. So, what we can do is call this lookahead() function before any
  // XML is parsed. This causes the ConstructingLoader to immediately switch to
  // the buffering source. There may be some slight overhead for buffering, but
  // at last our line numbers are correct.
  lookahead()


  private def makeSAXParseException(pos: Int, msg: String) = {
    val line = Position.line(pos)
    val col = Position.column(pos)
    val exc = new org.xml.sax.SAXParseException(msg, null, uri.toString, line, col)
    exc
  }

  override def reportSyntaxError(pos: Int, msg: String) {
    val exc = makeSAXParseException(pos, msg)
    errorHandler.error(exc)
  }

  /*
   * Callback method invoked by MarkupParser after parsing an element, between
   * the elemStart and elemEnd callbacks. This adds daffodil file/line/column
   * information as attributes to the existing input attrs, modifying the scope
   * if necessary, then creates an element using the super def elem function.
   *
   *  @param pos      the position in the source file
   *  @param pre      the prefix
   *  @param local    the local name
   *  @param attrs    the attributes (metadata)
   *  @param scope    the namespace binding scope
   *  @param empty    `true` if the element was previously empty; `false` otherwise.
   *  @param args     the children of this element
   */
  override def elem(
    pos: Int,
    pre: String,
    local: String,
    attrs: MetaData,
    scope: NamespaceBinding,
    empty: Boolean,
    nodes: NodeSeq): NodeSeq = {

    val nsURI = NS(scope.getURI(pre))
    val isFileRootNode = (local.equalsIgnoreCase("schema") && nsURI == XMLUtils.XSD_NAMESPACE) ||
      (local.equalsIgnoreCase("testSuite") && nsURI == XMLUtils.TDML_NAMESPACE)
    val hasLineCol = attrs.exists {
      case PrefixedAttribute(XMLUtils.INT_PREFIX, attr, _, _) => {
        attr.equalsIgnoreCase(XMLUtils.COLUMN_ATTRIBUTE_NAME) ||
          attr.equalsIgnoreCase(XMLUtils.LINE_ATTRIBUTE_NAME)
      }
      case _ => false
    }

    val newAttrs: MetaData = {
      if (!hasLineCol) {
        val withFile: MetaData =
          if (isFileRootNode) {
            new PrefixedAttribute(XMLUtils.INT_PREFIX, XMLUtils.FILE_ATTRIBUTE_NAME, uri.toString, attrs)
          } else {
            attrs
          }
        val withCol: MetaData = new PrefixedAttribute(XMLUtils.INT_PREFIX, XMLUtils.COLUMN_ATTRIBUTE_NAME, Position.column(pos).toString, withFile)
        val withLine: MetaData = new PrefixedAttribute(XMLUtils.INT_PREFIX, XMLUtils.LINE_ATTRIBUTE_NAME, Position.line(pos).toString, withCol)
        withLine
      } else {
        attrs
      }
    }

    // add the dafint prefix if it doesn't already exist
    val intPrefix = scope.getPrefix(XMLUtils.INT_NS)
    val newScope = if (intPrefix == null) {
      NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)
    } else {
      Assert.usage(intPrefix == null || intPrefix == XMLUtils.INT_PREFIX) // can't deal with some other binding for dafint
      scope
    }

    super.elem(pos, pre, local, newAttrs, newScope, empty, nodes)
  }

  def load(): Node = {
    val res =
      try {
        this.initialize
        val doc = this.document()
        if (doc == null) null
        else doc.docElem
      } catch {
        case e: Exception => {
          val exc = makeSAXParseException(curInput.pos, e.toString)
          errorHandler.fatalError(exc)
          null
        }
      }
    res
  }
}
