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

import java.io.IOException
import java.io.InputStream
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Using

import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.infoset.SAXInfosetOutputter

import org.xml.sax.ContentHandler
import org.xml.sax.DTDHandler
import org.xml.sax.EntityResolver
import org.xml.sax.ErrorHandler
import org.xml.sax.InputSource
import org.xml.sax.SAXException
import org.xml.sax.SAXNotRecognizedException
import org.xml.sax.SAXNotSupportedException
import org.xml.sax.SAXParseException

/**
 * XMLReader implementation that interfaces with the DataProcessor and SAXInfosetOutputter to parse
 * data into an XML infoset
 *
 * This is inherently a secure reader - it isn't actually reading any XML. So one need not (and cannot)
 * call things like setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true).
 *
 * @param dp dataprocessor object that will be used to call the parse
 */
class DaffodilParseXMLReader(dp: DataProcessor) extends DFDL.DaffodilParseXMLReader {
  private var contentHandler: ContentHandler = _
  private var errorHandler: ErrorHandler = _
  private var dtdHandler: DTDHandler = _
  private var entityResolver: EntityResolver = _
  private var saxParseResultPropertyValue: ParseResult = _
  private var saxBlobDirectoryPropertyValue: Path =
    Paths.get(System.getProperty("java.io.tmpdir"))
  private var saxBlobPrefixPropertyValue: String = "daffodil-sax-"
  private var saxBlobSuffixPropertyValue: String = ".blob"
  private var saxNamespaceFeatureValue: Boolean = true
  private var saxNamespacePrefixesFeatureValue: Boolean = false

  override def getFeature(name: String): Boolean = {
    name match {
      case XMLUtils.SAX_NAMESPACES_FEATURE => saxNamespaceFeatureValue
      case XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE => saxNamespacePrefixesFeatureValue
      case _ =>
        throwFeatureSAXNotRecogizedException(name, "Feature unsupported")
    }
  }

  override def setFeature(name: String, value: Boolean): Unit = {
    name match {
      case XMLUtils.SAX_NAMESPACES_FEATURE => saxNamespaceFeatureValue = value
      case XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE => saxNamespacePrefixesFeatureValue = value
      case _ =>
        throwFeatureSAXNotRecogizedException(name, "Feature unsupported")
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
        case XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY =>
          saxBlobDirectoryPropertyValue = value.asInstanceOf[Path]
        case XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX =>
          saxBlobPrefixPropertyValue = value
            .asInstanceOf[String]
        case XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX =>
          saxBlobSuffixPropertyValue = value
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
      Using.resource(InputSourceDataInputStream(is)) { isdis =>
        parse(isdis)
      }
    } else {
      throw new IOException("InputSource must be backed by InputStream")
    }
  }

  override def parse(systemId: String): Unit = {
    throw new IOException("SAX parsing of systemId is unsupported")
  }

  def parse(isdis: InputSourceDataInputStream): Unit = {
    // validate that the features are not false/false
    if (!saxNamespaceFeatureValue && !saxNamespacePrefixesFeatureValue) {
      throw new SAXException(
        "Illegal State: Namespaces and NamespacePrefixes features cannot both be false"
      )
    }
    // creates SAXInfosetOutputter object and calls setBlobAttributes on it
    val sio =
      new SAXInfosetOutputter(this, saxNamespaceFeatureValue, saxNamespacePrefixesFeatureValue)
    sio.setBlobAttributes(
      saxBlobDirectoryPropertyValue,
      saxBlobPrefixPropertyValue,
      saxBlobSuffixPropertyValue
    )
    val pr = dp.parse(isdis, sio)
    saxParseResultPropertyValue = pr.asInstanceOf[ParseResult]
    handleDiagnostics(pr)
  }

  def parse(stream: InputStream): Unit = {
    Using.resource(InputSourceDataInputStream(stream)) { isdis =>
      parse(isdis)
    }
  }

  def parse(arr: Array[Byte]): Unit = {
    Using.resource(InputSourceDataInputStream(arr)) { isdis =>
      parse(isdis)
    }
  }

  private def handleDiagnostics(pr: DFDL.ParseResult): Unit = {
    val diagnostics = pr.getDiagnostics
    val eh = this.getErrorHandler
    if (diagnostics.nonEmpty && eh != null) {
      diagnostics.foreach { d =>
        val spe = {
          val msg = d.getMessage()
          val (lineNo, colNo, systemId) = d.getLocationsInSchemaFiles.headOption
            .map { s =>
              val sl = s.asInstanceOf[SchemaFileLocation]
              val ln = sl.lineNumber.getOrElse("0").toInt
              val cn = sl.columnNumber.getOrElse("0").toInt
              val sId = sl.diagnosticFile.toString
              (ln, cn, sId)
            }
            .getOrElse((0, 0, null))

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

  def throwFeatureSAXNotRecogizedException(name: String, message: String): Boolean = {
    throw new SAXNotRecognizedException(
      message + ": " + name + ".\n" +
        "Supported features are: " +
        Seq(XMLUtils.SAX_NAMESPACES_FEATURE, XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE).mkString(
          ", "
        )
    )
  }
}
