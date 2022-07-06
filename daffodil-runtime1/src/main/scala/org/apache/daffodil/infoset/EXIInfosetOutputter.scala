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

package org.apache.daffodil.infoset

import org.apache.daffodil.util.{ Indentable, Misc }
import org.apache.daffodil.dpath.NodeInfo
import com.siemens.ct.exi.core.helpers.DefaultEXIFactory
import com.siemens.ct.exi.main.api.sax.EXIResult
import com.siemens.ct.exi.grammars.GrammarFactory
import com.siemens.ct.exi.core.FidelityOptions
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.InputSource

/**
 * Writes the infoset to a java.io.Writer as XML text.
 *
 * @param os The OutputStream to write the XML text to
 * @param pretty Whether or to enable pretty printing. Set to true, XML
 *               elements are indented and newlines are inserted.
 */
class EXIInfosetOutputter (os: java.io.OutputStream, pretty: Boolean)
  extends InfosetOutputter with Indentable with XMLInfosetOutputter {

  def this(os: java.io.OutputStream) = {
    this(os, false)
  }

  private val sb = new StringBuilder()
  private val sw = new java.io.StringWriter()

  /**
   * Used to keep determine if the in-scope complex element has children, and
   * thus if we should output a newline or not when closing that complex type.
   * A complex type with no children should just be output like
   *
   *   <complex></complex>
   *
   * This value is initialized to false when a complex type is started, since
   * we don't know if it has children yet. This value is then set to true
   * either when a simple type is started (i.e. the current complex type must
   * have at least one child), or when a complex type is ended (i.e. the parent
   * and all subsequent parents of the ended complex must have at least one
   * child, which is the complex that just eneded).
   */
  private var inScopeComplexElementHasChildren = false

  override def reset(): Unit = {
    resetIndentation()
    inScopeComplexElementHasChildren = false
  }

  private def outputTagName(elem: DIElement): Unit = {
    val prefix = elem.erd.prefix
    if (prefix != null && prefix != "") {
      sw.write(prefix)
      sw.write(":")
    }
    sw.write(elem.erd.name)
  }

  private def outputStartTag(elem: DIElement): Unit = {
    sw.write("<")

    outputTagName(elem)

    val nsbStart = elem.erd.minimizedScope
    val nsbEnd = if (elem.isRoot) scala.xml.TopScope else elem.diParent.erd.minimizedScope
    if (nsbStart != nsbEnd) {
      sb.setLength(0) // reset the stringbuilder
      nsbStart.buildString(sb, nsbEnd)
      sw.write(sb.toString)
    }

    if (isNilled(elem)) {
      sw.write(" xsi:nil=\"true\"")
    }

    sw.write(">")
  }

  private def outputEndTag(elem: DIElement): Unit = {
    sw.write("</")
    outputTagName(elem)
    sw.write(">")
  }

  override def startSimple(simple: DISimple): Boolean = {
    if (pretty) {
      sw.write(System.lineSeparator())
      outputIndentation(sw)
    }
    outputStartTag(simple)

    if (!isNilled(simple) && simple.hasValue) {
      val text =
        if (simple.erd.optPrimType.get.isInstanceOf[NodeInfo.String.Kind]) {
          val s = remapped(simple.dataValueAsString)
          scala.xml.Utility.escape(s)
        } else {
          simple.dataValueAsString
        }

      sw.write(text)
    }

    outputEndTag(simple)
    inScopeComplexElementHasChildren = true

    true
  }
  
  override def endSimple(simple: DISimple): Boolean = {
    // do nothing, everything is done in startSimple
    true
  }

  override def startComplex(complex: DIComplex): Boolean = {
    if (pretty) {
      sw.write(System.lineSeparator())
      outputIndentation(sw)
    }
    outputStartTag(complex)
    incrementIndentation()
    inScopeComplexElementHasChildren = false

    true
  }

  override def endComplex(complex: DIComplex): Boolean = {
    decrementIndentation()
    if (pretty && inScopeComplexElementHasChildren) {
      // only output newline and indentation for non-empty complex types
      sw.write(System.lineSeparator())
      outputIndentation(sw)
    }
    outputEndTag(complex)
    inScopeComplexElementHasChildren = true

    true
  }

  override def startArray(array: DIArray): Boolean = {
    // do nothing
    true
  }

  override def endArray(array: DIArray): Boolean = {
    // do nothing
    true
  }

  override def startDocument(): Boolean = {
    sw.write("""<?xml version="1.0" encoding="UTF-8"?>""")
    true
  }

  override def endDocument(): Boolean = {
    sw.write(System.lineSeparator())
    sw.flush()

    val xsdLocation = Misc.getRequiredResource("org/apache/daffodil/xsd/XMLSchema.xsd")
    lazy val exiFactory = DefaultEXIFactory.newInstance()
		val grammarFactory = GrammarFactory.newInstance()
		val g = grammarFactory.createGrammars(xsdLocation.toString)
		exiFactory.setGrammars(g);
    exiFactory.getFidelityOptions().setFidelity(FidelityOptions.FEATURE_PREFIX,true)
    val exiResult = new EXIResult(exiFactory)
    exiResult.setOutputStream(os)
    val xmlReader = XMLReaderFactory.createXMLReader()
    xmlReader.setContentHandler( exiResult.getHandler() )
    val is = new InputSource(new java.io.StringReader(sw.toString))
    xmlReader.parse(is) // parse XML input
    true
  }
}
