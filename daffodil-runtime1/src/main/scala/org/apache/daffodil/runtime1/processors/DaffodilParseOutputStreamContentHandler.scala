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

package org.apache.daffodil.runtime1.processors

import java.io.OutputStream
import java.io.OutputStreamWriter
import java.nio.charset.StandardCharsets
import scala.xml.NamespaceBinding

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Indentable
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.MStackOfBoolean
import org.apache.daffodil.lib.xml.XMLUtils

import org.xml.sax.Attributes
import org.xml.sax.ContentHandler
import org.xml.sax.Locator

/**
 * ContentHandler implementation that receives SAX events from DaffodilParseXMLReader to output
 * XML to the specified outputStream. Depending on the features set in the XMLReader, it uses either
 * prefixMappings or attributes to determine the prefix of the XML element. This means it will always
 * try to find and print a prefix if an element has a URI.
 *
 * @param out outputStream object to write generated XML to
 * @param pretty boolean to pretty print XML if true, or not if false
 */
class DaffodilParseOutputStreamContentHandler(out: OutputStream, pretty: Boolean = false)
  extends ContentHandler
  with Indentable {
  private val writer = new OutputStreamWriter(out, StandardCharsets.UTF_8)

  /**
   * represents the currently active prefix mappings (i.e all mappings include from parent element),
   * which is usefully for doing lookups
   */
  private var activePrefixMapping: NamespaceBinding = null

  /**
   * represents only the prefix mapping of the current element. We use this to generate the prefix mappings
   * when outputting the element tag
   */
  private var currentElementPrefixMapping: NamespaceBinding = null

  /**
   * used to maintain the correct scope of activePrefixMapping throughout processing. It is also used
   * to reset the activePrefixMapping after processing each element.
   */
  private lazy val activePrefixMappingContextStack = new MStackOf[NamespaceBinding]
  private val outputNewlineStack: MStackOfBoolean = {
    val s = MStackOfBoolean()
    s.push(false)
    s
  }

  // if the top of the stack is true, we have guessed we should output a newline
  private def outputNewline: Boolean = outputNewlineStack.top

  override def setDocumentLocator(locator: Locator): Unit = {
    // do nothing
  }

  override def startDocument(): Unit = {
    resetIndentation()
    activePrefixMapping = null
    currentElementPrefixMapping = null
    activePrefixMappingContextStack.clear()
    outputNewlineStack.clear()
    outputNewlineStack.push(false) // to match initialization state
    writer.write("""<?xml version="1.0" encoding="UTF-8"?>""")
  }

  override def endDocument(): Unit = {
    writer.write(System.lineSeparator())
    writer.flush()
    out.flush()
  }

  override def startPrefixMapping(prefix: String, uri: String): Unit = {
    val _prefix = if (prefix == "") null else prefix
    // only add this new prefix mapping to the currentElementMapping. The
    // mappings in this variable will be added to the active mapping later.
    // This is necessary because we essentially prepend NamespaceBindings when
    // adding new ones, which effectively reverses the order. When we add these
    // mappings to the activePrefixMapping, we'll undo that reversal so things
    // are in the correct order
    currentElementPrefixMapping = NamespaceBinding(_prefix, uri, currentElementPrefixMapping)
  }

  override def endPrefixMapping(prefix: String): Unit = {
    // do nothing
  }

  /**
   * Uses Attributes, which is passed in to the startElement callback, to
   * gather prefix mappings in the case where namespacePrefixes is true. New
   * prefix mappings are added to the currentElementPrefixMapping bindings
   */
  def processAttributePrefixMappings(atts: Attributes): Unit = {
    var i = 0
    while (i < atts.getLength) {
      val qName = atts.getQName(i)
      if (qName.nonEmpty) {
        // if qName is populated that implies namespacePrefixes == true, and we
        // might have prefix mappings if the qname is a special xmlns value
        if (qName == "xmlns") {
          val pre = null
          val uri = atts.getValue(i)
          currentElementPrefixMapping = NamespaceBinding(pre, uri, currentElementPrefixMapping)
        } else if (qName.startsWith("xmlns:")) {
          val pre = qName.substring(6)
          val uri = atts.getValue(i)
          currentElementPrefixMapping = NamespaceBinding(pre, uri, currentElementPrefixMapping)
        } else {
          // not a prefix mapping, ignore this attribute
        }
      } else {
        // no qname, so namespacePrefixes == false, which means we get no
        // prefix mappings in the Attributes, only regular attributes such as xsi:nil.
        // This can't be a prefix mapping, so ignore this attribute
      }
      i += 1
    }
  }

  /**
   * Uses Attributes, which is passed in to the startElement callback, to write
   * element attributes. Prefix mappings are ignored and assumed to be written
   * elsewhere. Uses activePrefixMappings to map uri's to prefixes, so prefix
   * mappings in the Attributes must have already been processed and added to
   * activePrefixMappings
   */
  def writeNonNamespaceAttributes(writer: OutputStreamWriter, atts: Attributes): Unit = {
    var i = 0
    while (i < atts.getLength) {
      val qName = atts.getQName(i)
      if (qName.nonEmpty) {
        if (qName.startsWith("xmlns:") || qName == "xmlns") {
          // namespace mapping, ignore
        } else {
          // regular attribute with qname such as xsi:nil
          val attrVal = atts.getValue(i)
          val attr = s""" ${qName}="${attrVal}""""
          writer.write(attr)
        }
      } else {
        // no qname, so namespacePrefixes == false, which means we get no
        // prefix mappings in the attributes, only regular attributes such as xsi:nil
        // though not in qname form
        val uri = atts.getURI(i)
        val localName = atts.getLocalName(i)
        // prefixed attribute, not prefix mapping, as they only show up as qnames
        if (uri.nonEmpty && localName.nonEmpty) {
          val maybePrefix = XMLUtils.maybePrefix(activePrefixMapping, uri)
          // found a prefix; add to attribute pairings
          if (maybePrefix.isDefined) {
            val prefix = maybePrefix.get
            val attrVal = atts.getValue(i)
            val attr = s""" $prefix:$localName="${attrVal}""""
            writer.write(attr)
          } else {
            // if an attribute has a URI, we must have a prefix, even if it is null
            Assert.invariantFailed("Cannot have URI with no prefix mapping")
          }
        } else {
          // non prefixed attribute don't exist in Daffodil
          Assert.invariantFailed("Cannot have an attribute with no qname, uri or localname")
        }
      }
      i += 1
    }
  }

  override def startElement(
    uri: String,
    localName: String,
    qName: String,
    atts: Attributes
  ): Unit = {
    // the pop/true removes whatever is on the stack which is our previous guess for whether we
    // would need a newline after the previous end tag. As we are currently at the start of a new
    // tag, we want to correct that assumption (in case it was false)
    outputNewlineStack.pop()
    outputNewlineStack.push(true)
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }

    // scan the attributes for any prefix mappings, which will update
    // currentElementPrefixMappings
    processAttributePrefixMappings(atts)

    // at this point, all prefix mappings specific to this element have been
    // added to currentElementPrefixMapping (either from startPrefixMapping or
    // processAttributePrefixMappings). Note that these new mappings are in the
    // reverse order than they should be written because they were prepended to
    // NamespaceBinding. We now add these new mappings to the activePrefixMapping,
    // and in doing so re-reverses the order so that they are in the correct
    // order in the activePrefixMaping. This ensures we get find the right
    // prefix during lookups and write the mappings in the correct order.
    val previousPrefixMapping = activePrefixMapping
    while (currentElementPrefixMapping != null) {
      val prefix = currentElementPrefixMapping.prefix
      val uri = currentElementPrefixMapping.uri
      activePrefixMapping = NamespaceBinding(prefix, uri, activePrefixMapping)
      currentElementPrefixMapping = currentElementPrefixMapping.parent
    }

    // we always push, but activePrefixMapping won't always be populated with new information
    // from startPrefixMapping or processAttributes
    activePrefixMappingContextStack.push(activePrefixMapping)

    // handle start of tag
    writer.write("<")
    outputTagName(uri, localName, qName)

    // write only the part of activePrefixMapping that is new for this element
    if (activePrefixMapping ne previousPrefixMapping) {
      val pm = activePrefixMapping.buildString(previousPrefixMapping)
      writer.write(pm)
    }

    // write the non-namespace attributes from the Attributes object. Example
    // attributes is xsi:nil
    writeNonNamespaceAttributes(writer, atts)

    // handle end of tag
    writer.write(">")
    incrementIndentation()
    // this push makes the assumption that we would not need to output a newline after this end
    // tag is complete
    outputNewlineStack.push(false)
  }

  private def outputTagName(uri: String, localName: String, qName: String): Unit = {
    val tagName = {
      if (qName.nonEmpty) {
        // namespacePrefixes == true, so qName is populated
        qName
      } else {
        // namespacePrefixes == false, so uri and localName are populated, but not qname
        // and attributes don't have prefix mapping information
        // if we have no qName, we need to use activePrefixMapping to get the prefix of the uri
        // to build the qname
        val sanitizedUri = if (uri.isEmpty) null else uri
        val maybePrefix = XMLUtils.maybePrefix(activePrefixMapping, sanitizedUri)
        if (maybePrefix.isDefined) {
          val pre = maybePrefix.get
          s"$pre:$localName"
        } else {
          localName
        }
      }
    }
    writer.write(tagName)
  }

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    decrementIndentation()
    if (outputNewline) {
      if (pretty) {
        writer.write(System.lineSeparator())
        outputIndentation(writer)
      }
    }
    writer.write("</")
    outputTagName(uri, localName, qName)
    writer.write(">")
    outputNewlineStack.pop()

    Assert.invariant(!activePrefixMappingContextStack.isEmpty)
    // throw out current prefix mapping context as we're done with it
    activePrefixMappingContextStack.pop

    // set the activePrefixMapping to the next mapping in the stack if the stack isn't empty
    if (activePrefixMappingContextStack.isEmpty) {
      activePrefixMapping = null
    } else {
      activePrefixMapping = activePrefixMappingContextStack.top
    }
  }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    val str = new String(ch, start, length)
    val escaped = scala.xml.Utility.escape(str)
    writer.write(escaped, 0, escaped.length)
  }

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    // do nothing
  }

  override def processingInstruction(target: String, data: String): Unit = {
    // do nothing
  }

  override def skippedEntity(name: String): Unit = {
    // do nothing
  }
}
