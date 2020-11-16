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

import org.apache.daffodil.infoset.XMLInfosetOutputter
import org.apache.daffodil.util.Indentable
import org.apache.daffodil.util.MStackOfBoolean
import org.xml.sax.Attributes
import org.xml.sax.ContentHandler
import org.xml.sax.Locator

class DaffodilParseOutputStreamContentHandler(out: OutputStream, pretty: Boolean = false)
  extends ContentHandler with Indentable with XMLInfosetOutputter {
  private val writer = new OutputStreamWriter(out)
  private var prefixMapping: NamespaceBinding = null
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
    prefixMapping = null
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
    prefixMapping = NamespaceBinding(_prefix, uri, prefixMapping)
  }

  override def endPrefixMapping(prefix: String): Unit = {
    // do nothing
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
    // handle start of tag
    writer.write("<")
    writer.write(qName)
    // handle attributes
    for (i <- 0 until atts.getLength) {
      val attsValue = atts.getValue(i)
      val attsQName = atts.getQName(i)
      writer.write(s""" $attsQName="$attsValue"""")
    }
    // handle namespaces
    if (prefixMapping != null) {
      val pm = prefixMapping.toString()
      writer.write(pm)
      prefixMapping = null
    }
    // handle end of tag
    writer.write(">")
    incrementIndentation()
    // this push makes the assumption that we would not need to output a newline after this end
    // tag is complete
    outputNewlineStack.push(false)
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
    writer.write(qName)
    writer.write(">")
    outputNewlineStack.pop()
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
