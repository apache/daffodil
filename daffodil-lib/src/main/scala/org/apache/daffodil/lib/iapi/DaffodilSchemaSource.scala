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

package org.apache.daffodil.lib.iapi
import java.io.File
import java.io.FileInputStream
import java.net.URI
import java.nio.file.FileSystemNotFoundException
import java.nio.file.Paths
import scala.xml.Node

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.xml.XMLUtils

import org.apache.commons.io.input.XmlStreamReader
import org.xml.sax.InputSource

/**
 * Our abstraction of the source of a schema.
 *
 * Provides the support needed to enable import/include schemaLocation
 * resolution using relative paths.
 *
 * Because we make multiple passes over the schema (for validation)
 * we have to cache the schema if it is given to us in any sort of transient form
 * like as an input stream (stdin for example).
 *
 * Can also be used for other XML artifacts such as TDML files.
 */
sealed trait DaffodilSchemaSource {

  /**
   * Use to get a org.xml.sax.InputSource for use by Xerces
   */
  def newInputSource(): InputSource

  /**
   * Use to get the URI that can be used to load the xml.
   */
  def uriForLoading: URI

  /**
   * @return True if the schema source is for an XSD source file (extension ".xsd")
   * False otherwise.
   */
  def isXSD: Boolean

  /**
   * Diagnostic file path associated with this schema source
   */
  def diagnosticFile: File
}

object URISchemaSource {
  def apply(diagnosticFile: File, uri: URI): URISchemaSource = {
    new URISchemaSource(diagnosticFile, uri)
  }
}

class URISchemaSource protected (diagnosticFilepath: File, val uri: URI)
  extends DaffodilSchemaSource {

  override def equals(other: Any) = other match {
    case oth: URISchemaSource => this.uri == oth.uri
    case _ => false
  }

  override def hashCode() = uri.hashCode()

  private lazy val url = uri.toURL

  final def isXSD = isXSD_

  /**
   * Must be lazy so that it captures the file mod time when it is opened
   * and the content used.
   */
  lazy val (isFile, isXSD_, file, fileModTime) = {
    val uriString = uriForLoading.toString
    //
    // doing this the manual way using split
    // on string because FilenameUtils.getExtension
    // does something complicated for the nested
    // jar URIs we encounter here. It seems to not
    // move past the nesting, and sees ".jar" not
    // ".xsd" for files that are clearly XSD.
    //
    val ext = uriString.split("\\.").last
    val isXSDExt = (ext == "xsd")
    try {
      val path = Paths.get(uri)
      val f = path.toFile()
      (true, isXSDExt, f, f.lastModified())
    } catch {
      case e: FileSystemNotFoundException => (false, isXSDExt, null, 0L)
      case e: UnsupportedOperationException => (false, isXSDExt, null, 0L)
    }
  }

  override def newInputSource() = {
    fileModTime // demand this so we have it recorded
    val is = new InputSource(url.openStream())
    is.setSystemId(uri.toString)
    is
  }

  override def uriForLoading = uri

  override def diagnosticFile: File = diagnosticFilepath

  /**
   * @return True if this URI is for a file, other URI is for a file
   * but the modification date of this is newer than the modification
   * date of the other.
   */
  def isNewerThan(other: URISchemaSource): Boolean = {
    if (this.isFile && other.isFile) {
      val thisTime = fileModTime
      val otherTime = other.fileModTime
      if (thisTime > otherTime) true
      else false
    } else false
  }
}

/**
 * For stdin, or other anonymous pipe-like source of schema.
 */
class InputStreamSchemaSource(
  is: java.io.InputStream,
  tmpDir: Option[File],
  blameName: String,
  extension: String
) extends DaffodilSchemaSource {
  lazy val tempSchemaFile =
    XMLUtils.convertInputStreamToTempFile(is, tmpDir.getOrElse(null), blameName, extension)
  lazy val tempURI = tempSchemaFile.toURI
  lazy val csName = {
    val xmlStream = XmlStreamReader.builder().setFile(tempSchemaFile).get()
    val csName = xmlStream.getEncoding()
    xmlStream.close()
    csName
  }

  def isXSD = Assert.usageError("isXSD not supported for InputStreamInputSource")

  override def newInputSource() = {
    val is = new FileInputStream(tempSchemaFile)
    val inSrc = new InputSource(is)
    inSrc.setEncoding(csName)
    inSrc.setSystemId(blameName)
    inSrc
  }
  override def uriForLoading = tempURI

  override def diagnosticFile: File = tempSchemaFile
}

protected sealed abstract class NodeSchemaSourceBase(
  node: Node,
  nameHint: String,
  tmpDir: Option[File],
  tempSchemaFileFromNode: File
) extends URISchemaSource(
    tempSchemaFileFromNode,
    tempSchemaFileFromNode.toURI
  ) {

  def blameName: String

  override def newInputSource() = {
    val inSrc = new InputSource(this.uriForLoading.toURL().openStream())
    inSrc.setSystemId(blameName)
    inSrc
  }
}

case class UnitTestSchemaSource(node: Node, nameHint: String, optTmpDir: Option[File] = None)
  extends NodeSchemaSourceBase(
    node,
    nameHint,
    optTmpDir,
    XMLUtils.convertNodeToTempFile(node, optTmpDir.orNull, nameHint)
  ) {
  override val blameName =
    if (nameHint != "") "unittest:" + nameHint
    else uriForLoading.toString
}

/**
 * Used by TDML runner for embedded schemas- the schema node is constructed out of the TDML file
 * which in order to be able to validate repeatedly and such, is written to a temp file.
 */
case class EmbeddedSchemaSource(node: Node, nameHint: String, optTmpDir: Option[File] = None)
  extends NodeSchemaSourceBase(
    node,
    nameHint,
    optTmpDir,
    XMLUtils.convertNodeToTempFile(node, optTmpDir.orNull, nameHint)
  ) {
  override val blameName = nameHint

  override val diagnosticFile =
    XMLUtils
      .getOptTDMLFileFromNode(node)
      .getOrElse(super.diagnosticFile)
}
