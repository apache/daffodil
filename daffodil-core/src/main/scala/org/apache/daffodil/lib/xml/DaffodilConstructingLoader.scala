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

package org.apache.daffodil.lib.xml

import java.io.BufferedInputStream
import java.net.URI
import scala.io.Source
import scala.xml._
import scala.xml.include.sax.EncodingHeuristics
import scala.xml.parsing.ConstructingParser

import org.apache.daffodil.lib.exceptions.Assert

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
 * This code is effectively our fork of the Scala ConstructingParser. This
 * works around some bugs in it.
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
 * Also, enhanced so that when addPositionAttributes is true, it will capture
 * file/line/column info for every element and add it
 * as attributes onto each XML element.
 *
 * The way the constructing loader (aka ConstructingParser (for XML))
 * gets positions is different. It is given just an offset into the document file/stream,
 * and it therefore must synthesize line number/col number info itself.
 *
 * This primary constructor is package private as the normalizeCRLFtoLF feature
 * is only for test/exploratory usage, or if a future need arises to preserve the
 * non-normalizaing behavior.
 *
 * @param uri URI for the XML to be loaded.
 * @param errorHandler Called back on load errors.
 * @param addPositionAttributes Use true if you want dafint:file,
 *                              dafint:col, and dafint:line attributes.
 *                              Defaults to false.
 * @param normalizeCRLFtoLF Use true to emulate the scala XML load
 *                          behavior of normalizing CRLF to LF, and solitary CR to LF.
 *                          Defaults to true. Should only be changed in special circumstances
 *                          as not normalizing CRLFs is non-standard for XML.
 *
 */
class DaffodilConstructingLoader private[xml] (
  uri: URI,
  errorHandler: org.xml.sax.ErrorHandler,
  addPositionAttributes: Boolean,
  normalizeCRLFtoLF: Boolean
) extends ConstructingParser(
    {
      // Note: we must open the XML carefully since it might be in some non
      // default encoding (we have tests that have UTF-16 for example)

      // must be buffered to support mark(), needed by heuristics
      val is = new BufferedInputStream(uri.toURL.openStream())
      val enc = EncodingHeuristics.readEncodingFromStream(is)
      Source.fromInputStream(is, enc)
    },
    true
  ) {

  /**
   * Public constructor insists on normalizingCRLFtoLF behavior.
   */
  def this(
    uri: URI,
    errorHandler: org.xml.sax.ErrorHandler,
    addPositionAttributes: Boolean = false
  ) =
    this(uri, errorHandler, addPositionAttributes, normalizeCRLFtoLF = true)

  /**
   * Ensures that DOCTYPES aka DTDs, if encountered, are rejected.
   *
   * Coverage is off, because this should never be hit, because
   * the loader always has loaded the data with xerces prior to
   * this loader (for validation purposes), and that will have caught
   * the doctype being in the XML.
   *
   * However, under code maintenance, suppose someone turned that off
   * or made that pass optional (for performance reasons perhaps). Then this
   * provides a last-gasp attempt to protect from DOCTYPE-related
   * insecurity.
   */
  // $COVERAGE-OFF$
  override def parseDTD(): Unit = {
    val e = makeSAXParseException(pos, "DOCTYPE is disallowed.")
    throw e
  }
  // $COVERAGE-ON$

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

  override def reportSyntaxError(pos: Int, msg: String): Unit = {
    val exc = makeSAXParseException(pos, msg)
    errorHandler.fatalError(exc)
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
    nodes: NodeSeq
  ): NodeSeq = {

    val nsURI = NS(scope.getURI(pre))
    val isFileRootNode =
      (local.equalsIgnoreCase("schema") && nsURI == XMLUtils.XSD_NAMESPACE) ||
        (local.equalsIgnoreCase("testSuite") && nsURI == XMLUtils.TDML_NAMESPACE)
    val alreadyHasLineCol = attrs.exists {
      case PrefixedAttribute(XMLUtils.INT_PREFIX, attr, _, _) => {
        attr.equalsIgnoreCase(XMLUtils.COLUMN_ATTRIBUTE_NAME) ||
        attr.equalsIgnoreCase(XMLUtils.LINE_ATTRIBUTE_NAME)
      }
      case _ => false
    }

    val newAttrs: MetaData = {
      if (addPositionAttributes && !alreadyHasLineCol) {
        val withFile: MetaData =
          if (isFileRootNode) {
            new PrefixedAttribute(
              XMLUtils.INT_PREFIX,
              XMLUtils.FILE_ATTRIBUTE_NAME,
              uri.toString,
              attrs
            )
          } else {
            attrs
          }
        val withCol: MetaData = new PrefixedAttribute(
          XMLUtils.INT_PREFIX,
          XMLUtils.COLUMN_ATTRIBUTE_NAME,
          Position.column(pos).toString,
          withFile
        )
        val withLine: MetaData = new PrefixedAttribute(
          XMLUtils.INT_PREFIX,
          XMLUtils.LINE_ATTRIBUTE_NAME,
          Position.line(pos).toString,
          withCol
        )
        withLine
      } else {
        attrs
      }
    }

    // add the dafint prefix if it doesn't already exist
    val intPrefix = scope.getPrefix(XMLUtils.INT_NS)
    val newScope = if (addPositionAttributes && intPrefix == null) {
      NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)
    } else {
      Assert.usage(
        intPrefix == null || intPrefix == XMLUtils.INT_PREFIX
      ) // can't deal with some other binding for dafint
      scope
    }

    super.elem(pos, pre, local, newAttrs, newScope, empty, nodes)
  }

  /**
   * To emulate the behavior of Xerces loader (standard scala loader)
   * we have to normalize CRLF to LF, and solitary CR to LF.
   *
   * This is optional controlled by a constructor parameter.
   */
  override def text(pos: Int, txt: String): Text = {
    val newText: String = {
      if (normalizeCRLFtoLF && txt.contains("\r")) {
        txt.replaceAll("\r\n", "\n").replaceAll("\r", "\n")
      } else {
        txt
      }
    }
    //
    // On MS-Windows the TDML Runner previously would load XML
    // files and due to git autoCRLF=true, they would
    // have CRLFs in them. The loader the TDML Runner WAS
    // using (not any more) was preserving these CRLFs
    // in the XML infoset data, and so tests could come
    // to depend on this and be non-portable between
    // unix (LF only) and windows (CRLF only).
    //
    // Furthermore, the TDML file itself used to be loaded with this
    // CRLF-preserving loader.
    //
    // The TDML Runner now always normalizes CRLF or
    // isolated CR to LF like regular XML loaders do,
    // for both the TDML file itself, and any files it
    // loads. So this is no longer an issue.
    //
    super.text(pos, newText)
  }

  /**
   * We override this to force the ConstrutingParser to process CDATA regions
   * specially with an override-able method named cdata.
   *
   * Strangely, if you look at the implementation of this in the MarkupParser
   * trait, it calls the handler for text, but then it ignores the result of that
   * and constructs a PCDATA node from the original text.
   *
   * It's possible this is a bug fix.
   */
  override def xCharData: NodeSeq = {
    xToken("[CDATA[")
    def mkResult(pos: Int, s: String): NodeSeq = {
      val s1 = cdata(pos, s).text
      PCData(s1)
    }
    xTakeUntil(mkResult, () => pos, "]]>")
  }

  /**
   * Same CRLF/CR => LF processing as text gets.
   */
  def cdata(pos: Int, s: String): NodeSeq = {
    text(pos, s)
  }

  /**
   * Drops comments
   */
  override def comment(pos: Int, s: String): Comment = {
    // returning null drops comments
    null
  }

  /**
   * Drops processing instructions
   */
  override def procInstr(pos: Int, target: String, txt: String) = {
    // returning null drops processing instructions
    null
  }

  private def parseXMLPrologAttributes(
    m: MetaData
  ): (Option[String], Option[String], Option[Boolean]) = {

    var info_ver: Option[String] = None
    var info_enc: Option[String] = None
    var info_stdl: Option[Boolean] = None

    var n = 0
    m("version") match {
      case null =>
      case Text("1.0") =>
        info_ver = Some("1.0"); n += 1
      case _ => reportSyntaxError("cannot deal with versions != 1.0")
    }

    m("encoding") match {
      case null =>
      case Text(enc) =>
        if (!isValidIANAEncoding(enc))
          reportSyntaxError("\"" + enc + "\" is not a valid encoding")
        else {
          info_enc = Some(enc)
          n += 1
        }
    }

    m("standalone") match {
      case null =>
      case Text("yes") =>
        info_stdl = Some(true); n += 1
      case Text("no") =>
        info_stdl = Some(false); n += 1
      case _ => reportSyntaxError("either 'yes' or 'no' expected")
    }

    if (m.length - n != 0) {
      reportSyntaxError(
        "only 'version', 'encoding', and 'standalone' attributes are expected in xml prolog. Found: " + m
      )
    }

    (info_ver, info_enc, info_stdl)
  }

  /**
   * Override of document to make it tolerant of the start of the file
   * being whitespace instead of a "<" character
   *
   * This does not handle DOCTYPEs (aka DTDs) at all. Hence, is not
   * a true replacement (bug fix) on the original ConstructingParser method
   * that it overrides.
   */
  override def document(): Document = {
    doc = new Document()
    this.dtd = null
    var children: NodeSeq = null

    if ('<' == ch) {
      nextch()
      if ('?' == ch) {
        nextch()
        // It's probably an XML prolog, but
        // there are cases where there is no XML Prolog, but a starting
        // PI of <?xml-model href="...."?>
        // So we have to recognize as a general PI, then look and see if
        // it is a prolog.
        val name = xName
        xSpace()
        val (md, scp) = xAttributes(TopScope)
        if (scp != TopScope)
          reportSyntaxError("no xmlns definitions allowed.")
        xToken('?')
        xToken('>')
        if (name == "xml") {
          val info_prolog = parseXMLPrologAttributes(md)
          doc.version = info_prolog._1
          doc.encoding = info_prolog._2
          doc.standAlone = info_prolog._3
        } else {
          // not an xml prolog. It's some other PI
          // do nothing. We're just skipping those PIs
        }
        children = content(TopScope)
      } else {
        val ts = new NodeBuffer()
        content1(TopScope, ts) // the 1 suffix means "without the first < character"
        ts &+ content(TopScope)
        children = NodeSeq.fromSeq(ts)
      }
    } else {
      children = content(TopScope)
    }

    var isErr = false
    var elemCount = 0
    var theNode: Node = null
    children.foreach { c =>
      c match {
        case _: ProcInstr => // skip
        case _: Comment => // skip
        // $COVERAGE-OFF$ // constructing parser never creates these - probably due to a bug
        case _: EntityRef => {
          reportSyntaxError("no entity references allowed here")
          isErr = true
        }
        // $COVERAGE-ON$
        case s: SpecialNode => {
          val txt = s.toString.trim()
          if (txt.length > 0) {
            reportSyntaxError("non-empty text nodes not allowed: '" + txt + "'.")
            isErr = true
          }
        }
        case m: Elem =>
          elemCount += 1
          theNode = m
      }
    }
    if (1 != elemCount) {
      reportSyntaxError("document must contain exactly one element")
      isErr = true
    }

    if (!isErr) {
      doc.children = children
      doc.docElem = theNode
      doc
    } else {
      null
    }
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
          errorHandler.fatalError(exc) // good place for a breakpoint
          null
        }
      }
    res
  }
}
