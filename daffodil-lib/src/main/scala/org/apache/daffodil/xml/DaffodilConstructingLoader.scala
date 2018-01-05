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
  final val LINE_BITS   = 20
  /** Number of bits used to encode the column number */
  final val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
  /** Mask to decode the line number */
  final val LINE_MASK   = (1 << LINE_BITS) - 1
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

  override def reportSyntaxError(pos: Int, msg: String) {
    val line = Position.line(pos)
    val col = Position.column(pos)
    val exc = new org.xml.sax.SAXParseException(msg, null, uri.toString, line, col)
    errorHandler.error(exc)
  }


  /**
   * We probably aren't supposed to override mkAttributes(), however,
   * mkAttributes is a really really easy place to add our file/line/col
   * attributes and namespace, since this is the function in the
   * ConstructingParser that creates that stuff. However, the super mkAttribute
   * function, and functions that lead up to it being called, change the pos
   * member variable. So by the time our mkAttributes is called, the pos is
   * off, usually by a line or two, which is confusing. So override xTag, which
   * is called early enough before the position is changed, to capture the
   * position, and then use that position when mkAttributes is called.
   *
   * Note that this is potentially fragile and could break if scala-xml ever
   * makes significant changes. However, these functions, and the classes
   * containing these functions have not been modified for many years, so I
   * suspect they are fairly stable and it's safe to assume things won't change
   * significantly.
   */
  var capturedPos: Int = 0

  override protected def xTag(pscope: NamespaceType): (String, AttributesType) = {
    // save the position so we can use it in mkAttributes, super.xTag is going
    // to change it before our mkAttributes is called
    capturedPos = this.pos
    super.xTag(pscope)
  }

  override def mkAttributes(qname: String, pscope: NamespaceBinding): (scala.xml.MetaData, scala.xml.NamespaceBinding) = {
    val (attrs, scope) = super.mkAttributes(qname, pscope)

    val (pre, local) = Utility.prefix(qname) match {
      case Some(p) => (p, qname.drop(p.length + 1))
      case _       => (null, qname)
    }
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

    val newAttrs = if (!hasLineCol) {
      val next = if (isFileRootNode) {
        new PrefixedAttribute(XMLUtils.INT_PREFIX, XMLUtils.FILE_ATTRIBUTE_NAME, uri.toString, attrs)
      } else {
        attrs
      }
      val colAttr  = new PrefixedAttribute(XMLUtils.INT_PREFIX, XMLUtils.COLUMN_ATTRIBUTE_NAME, Position.column(capturedPos).toString, next)
      val lineAttr = new PrefixedAttribute(XMLUtils.INT_PREFIX, XMLUtils.LINE_ATTRIBUTE_NAME, Position.line(capturedPos).toString, colAttr)
      lineAttr
    } else {
      attrs
    }

    // add the dafint prefix if it doesn't already exist
    val intPrefix = scope.getPrefix(XMLUtils.INT_NS)
    val newScope = if (intPrefix == null) {
      NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)
    } else {
      Assert.usage(intPrefix == XMLUtils.INT_PREFIX) // can't deal with some other binding for dafint
      scope
    }

    (newAttrs, newScope)
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
