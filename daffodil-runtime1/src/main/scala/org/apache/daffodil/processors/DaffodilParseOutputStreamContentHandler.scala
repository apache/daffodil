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

package org.apache.daffodil.processors

import java.io.OutputStream
import java.io.OutputStreamWriter

import scala.xml.NamespaceBinding

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Indentable
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.util.MStackOfBoolean
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.xml.XMLUtils
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
  extends ContentHandler with Indentable {
  private val writer = new OutputStreamWriter(out)
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

  def reset(): Unit = {
    resetIndentation()
    writer.flush()
    activePrefixMapping = null
    currentElementPrefixMapping = null
    activePrefixMappingContextStack.clear()
    outputNewlineStack.clear()
    outputNewlineStack.push(false) //to match initialization state
    out.flush()
  }

  override def setDocumentLocator(locator: Locator): Unit = {
    // do nothing
  }

  override def startDocument(): Unit = {
    writer.write("""<?xml version="1.0" encoding="UTF-8"?>""")
  }

  override def endDocument(): Unit = {
    writer.write(System.lineSeparator())
    writer.flush()
  }

  override def startPrefixMapping(prefix: String, uri: String): Unit = {
    val _prefix = if (prefix == "") null else prefix
    activePrefixMapping = NamespaceBinding(_prefix, uri, activePrefixMapping)
    currentElementPrefixMapping = NamespaceBinding(_prefix, uri, currentElementPrefixMapping)
  }

  override def endPrefixMapping(prefix: String): Unit = {
    // do nothing
  }

  /**
   * Uses Attributes, which is passed in to the startElement callback, to gather element attributes
   * or in the case where namespacePrefixes is true, prefix mappings. Any new prefix mappings are used
   * to update the activePrefixMapping and currentElementPrefixMapping.
   *
   * @return sequence of string attribute=val pairings
   */
  def processAttributes(atts: Attributes): Seq[String] = {
    var attrPairings: Seq[String] = Seq()
    var i = 0
    var newMappingsList: Seq[(String, String)] = Seq()
    while (i < atts.getLength) {
      val qName = atts.getQName(i)
      val attrVal =  atts.getValue(i)
      if (qName.nonEmpty) {
        // if qName is populated; as in when namespacePrefixes == true
        // describing namespace mapping
        if (qName.startsWith("xmlns:") || qName == "xmlns") {
          // get prefix
          val pre = if (qName.startsWith("xmlns:")) {
            qName.substring(6)
          } else {
            null
          }
          // we make this call to check if the prefix already exists. If it doesn't exist, we get a
          // Nope, so we can add it to our list, but if it does exist, nothing happens and it doesn't
          // get re-added and we instead proceed to the next item in Attributes
          val maybeUri = XMLUtils.maybeURI(activePrefixMapping, pre)
          if (maybeUri.isEmpty || maybeUri.get != attrVal) { // not found yet, add it
            newMappingsList +:= (pre, attrVal)
          }
        } else {
          // regular attribute with qname such as xsi:nil
          attrPairings +:= s""" ${qName}="${attrVal}""""
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
            attrPairings +:= s""" $prefix:$localName="${attrVal}""""
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
    newMappingsList.foreach{ case (prefix, uri) =>
      activePrefixMapping = NamespaceBinding(prefix, uri, activePrefixMapping)
      currentElementPrefixMapping = NamespaceBinding(prefix, uri, currentElementPrefixMapping)
    }
    attrPairings.reverse
  }

  override def startElement(
    uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    // the pop/true removes whatever is on the stack which is our previous guess for whether we
    // would need a newline after the previous end tag. As we are currently at the start of a new
    // tag, we want to correct that assumption (in case it was false)
    outputNewlineStack.pop()
    outputNewlineStack.push(true)
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }

    /**
     * represents the attributes for the current element. We use this to generate the attributes list
     * within the start tag
     */
    val currentElementAttributes = processAttributes(atts)
    // we always push, but activePrefixMapping won't always be populated with new information
    // from startPrefixMapping or processAttributes
    activePrefixMappingContextStack.push(activePrefixMapping)

    // handle start of tag
    writer.write("<")
    outputTagName(uri, localName, qName, Some(atts))

    // this contains only xmlns prefixes and are populated via the start/endPrefixMappings
    // or Attributes via processAttributes()
    if (currentElementPrefixMapping != null) {
      val pm = currentElementPrefixMapping.toString()
      writer.write(pm)
      currentElementPrefixMapping = null
    }

    // handles attributes from the Attributes object. Example attributes is xsi:nil
    if (currentElementAttributes.nonEmpty) {
      val attrs = currentElementAttributes.mkString(" ")
      writer.write(attrs)
    }

    // handle end of tag
    writer.write(">")
    incrementIndentation()
    // this push makes the assumption that we would not need to output a newline after this end
    // tag is complete
    outputNewlineStack.push(false)
  }

  private def outputTagName(uri: String, localName: String, qName: String, atts: Maybe[Attributes] = Nope): Unit = {
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
    writer.write(ch, start, length)
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
