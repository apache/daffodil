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

import java.io.IOException
import java.io.InputStream
import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.mutable

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.infoset.SAXInfosetOutputter
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.xml.XMLUtils
import org.xml.sax.ContentHandler
import org.xml.sax.DTDHandler
import org.xml.sax.EntityResolver
import org.xml.sax.ErrorHandler
import org.xml.sax.InputSource
import org.xml.sax.SAXNotRecognizedException
import org.xml.sax.SAXNotSupportedException
import org.xml.sax.SAXParseException

class DaffodilParseXMLReader(dp: DataProcessor) extends DFDL.DaffodilParseXMLReader {
  private var contentHandler: ContentHandler = _
  private var errorHandler: ErrorHandler = _
  private var dtdHandler: DTDHandler = _
  private var entityResolver: EntityResolver = _
  var saxParseResultPropertyValue: ParseResult = _
  var saxBlobDirectoryPropertyValue: Path = Paths.get(System.getProperty("java.io.tmpdir"))
  var saxBlobPrefixPropertyValue: String = "daffodil-sax-"
  var saxBlobSuffixPropertyValue: String = ".blob"

  private val featureMap = mutable.Map[String, Boolean](
    XMLUtils.SAX_NAMESPACES_FEATURE -> false,
    XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE -> false
  )

  override def getFeature(name: String): Boolean = {
    if (name == XMLUtils.SAX_NAMESPACES_FEATURE ||
      name == XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE) {
      featureMap(name)
    } else {
      throw new SAXNotRecognizedException("Feature unsupported: " + name + ".\n" +
        "Supported features are: " + featureMap.keys.mkString(", ")
      )
    }
  }

  override def setFeature(name: String, value: Boolean): Unit = {
    if (name == XMLUtils.SAX_NAMESPACES_FEATURE ||
      name == XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE) {
      featureMap(name) = value
    } else {
      throw new SAXNotRecognizedException("Feature unsupported: " + name + ".\n" +
        "Supported features are: " + featureMap.keys.mkString(", ")
      )
    }
  }

  override def getProperty(name: String): AnyRef = {
    val prop = name match {
      case XMLUtils.DAFFODIL_SAX_URN_PARSERESULT => saxParseResultPropertyValue
      case XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY => saxBlobDirectoryPropertyValue
      case XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX => saxBlobPrefixPropertyValue
      case XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX => saxBlobSuffixPropertyValue
      case _ =>
        throw new SAXNotRecognizedException("Property unsupported: " + name + ".")
    }
    prop
  }

  override def setProperty(name: String, value: AnyRef): Unit = {
    try {
      name match {
        case XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY => saxBlobDirectoryPropertyValue =
          value.asInstanceOf[Path]
        case XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX => saxBlobPrefixPropertyValue = value
          .asInstanceOf[String]
        case XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX => saxBlobSuffixPropertyValue = value
          .asInstanceOf[String]
        case _ =>
          throw new SAXNotRecognizedException("Property unsupported: " + name + ".")
      }
    } catch {
      case _: ClassCastException =>
        throw new SAXNotSupportedException("Unsupported value for property: " + name + ".")
    }
  }

  override def setEntityResolver(resolver: EntityResolver): Unit = {
    entityResolver = resolver
  }

  override def getEntityResolver: EntityResolver = entityResolver

  override def setDTDHandler(handler: DTDHandler): Unit = {
    dtdHandler = handler
  }

  override def getDTDHandler: DTDHandler = dtdHandler

  override def setContentHandler(handler: ContentHandler): Unit = {
    contentHandler = handler;
  }

  override def getContentHandler: ContentHandler = contentHandler

  override def setErrorHandler(handler: ErrorHandler): Unit = {
    errorHandler = handler;
  }

  override def getErrorHandler: ErrorHandler = errorHandler

  override def parse(input: InputSource): Unit = {
    val is = input.getByteStream
    if (is != null) {
      val isdis = InputSourceDataInputStream(is)
      parse(isdis)
    } else {
      throw new IOException("InputSource must be backed by InputStream")
    }
  }

  override def parse(systemId: String): Unit = {
    throw new IOException("SAX parsing of systemId is unsupported")
  }

  def parse(isdis: InputSourceDataInputStream): Unit = {
    val sio = createSAXInfosetOutputter(this)
    val pr = dp.parse(isdis, sio)
    saxParseResultPropertyValue = pr.asInstanceOf[ParseResult]
    handleDiagnostics(pr)
  }

  def parse(stream: InputStream): Unit = {
    val isdis = InputSourceDataInputStream(stream)
    parse(isdis)
  }

  def parse(arr: Array[Byte]): Unit = {
    val isdis = InputSourceDataInputStream(arr)
    parse(isdis)
  }

  private def handleDiagnostics(pr: DFDL.ParseResult): Unit = {
    val diagnostics = pr.getDiagnostics
    val eh = this.getErrorHandler
    if (diagnostics.nonEmpty && eh != null) {
      diagnostics.foreach { d =>
        val spe = {
          val msg = d.getMessage()
          val (lineNo, colNo, systemId) = d.getLocationsInSchemaFiles.headOption.map { s =>
            val sl = s.asInstanceOf[SchemaFileLocation]
            val ln = sl.lineNumber.getOrElse("0").toInt
            val cn = sl.columnNumber.getOrElse("0").toInt
            val sId = sl.uriString
            (ln, cn, sId)
          }.getOrElse((0, 0, null))

          val spe = new SAXParseException(msg, null, systemId, lineNo, colNo, d)
          spe
        }

        if (d.isError) {
          eh.error(spe)
        } else {
          eh.warning(spe)
        }
      }
    }
  }

  /**
   * Creates SAXInfosetOutputter object and attempts to setBlobAttributes on it if
   * it has at least the blobDirectory property set
   *
   * @return SAXInfosetOutputter object with or without blob Attributes set
   */
  private def createSAXInfosetOutputter(xmlReader: DaffodilParseXMLReader): SAXInfosetOutputter = {
    val sioo = new SAXInfosetOutputter(xmlReader)
    val siof = try {
      sioo.setBlobAttributes(saxBlobDirectoryPropertyValue, saxBlobPrefixPropertyValue,
        saxBlobSuffixPropertyValue
      )
      sioo
    } catch {
      case e: SAXNotSupportedException => sioo
    }
    siof
  }
}
