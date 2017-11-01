/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.api
import org.xml.sax.InputSource
import java.net.URI
import scala.xml.Node
import java.io.FileInputStream
import org.apache.daffodil.xml.XMLUtils
import org.apache.commons.io.input.XmlStreamReader
import java.io.File
import java.nio.file.Paths
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.equality._
import java.nio.file.FileSystemNotFoundException

/**
 * Our abstraction of the source of a schema.
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
}

case class URISchemaSource(fileOrResource: URI) extends DaffodilSchemaSource {

  override def equals(other: Any) = other match {
    case oth: URISchemaSource => this.fileOrResource == oth.fileOrResource
    case _ => false
  }

  override def hashCode() = fileOrResource.hashCode()

  private lazy val url = fileOrResource.toURL

  /**
   * Must be lazy so that it captures the file mod time when it is opened
   * and the content used.
   */
  lazy val (isFile, file, fileModTime) = try {
    val path = Paths.get(fileOrResource)
    val f = path.toFile()
    (true, f, f.lastModified())
  } catch {
    case e: FileSystemNotFoundException => (false, null, 0L)
    case e: UnsupportedOperationException => (false, null, 0L)
  }

  override def newInputSource() = {
    fileModTime // demand this so we have it recorded
    val is = new InputSource(url.openStream())
    is.setSystemId(fileOrResource.toString)
    is
  }

  override def uriForLoading = fileOrResource

  /**
   * True if this URI is for a file, other URI is for a file
   * (it is required that they're both the same URI. Usage error otherwise),
   * but the modification date
   * of the two is such that this is newer than the other at the time the
   * other was accessed via newInputSource()
   *
   * Otherwise false.
   */
  def isNewerThan(other: URISchemaSource): Boolean = {
    Assert.usage(fileOrResource =:= other.fileOrResource)
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
case class InputStreamSchemaSource(is: java.io.InputStream, tmpDir: Option[File], blameName: String, extension: String) extends DaffodilSchemaSource {
  lazy val tempSchemaFile = XMLUtils.convertInputStreamToTempFile(is, tmpDir.getOrElse(null), blameName, extension)
  lazy val tempURI = tempSchemaFile.toURI
  lazy val csName = {
    val xmlStream = new XmlStreamReader(tempSchemaFile)
    val csName = xmlStream.getEncoding()
    xmlStream.close()
    csName
  }
  override def newInputSource() = {
    val is = new FileInputStream(tempSchemaFile)
    val inSrc = new InputSource(is)
    inSrc.setEncoding(csName)
    inSrc.setSystemId(blameName)
    inSrc
  }
  override def uriForLoading = tempURI
}

protected sealed abstract class NodeSchemaSourceBase(node: Node, nameHint: String, tmpDir: Option[File]) extends DaffodilSchemaSource {
  lazy val tempSchemaFile = XMLUtils.convertNodeToTempFile(node, tmpDir.getOrElse(null), nameHint)
  lazy val tempURI = tempSchemaFile.toURI
  def blameName: String
  override def newInputSource() = {
    val is = new FileInputStream(tempSchemaFile)
    val inSrc = new InputSource(is)
    inSrc.setSystemId(blameName)
    inSrc
  }
  override def uriForLoading = tempURI
}

case class UnitTestSchemaSource(node: Node, nameHint: String, optTmpDir: Option[File] = None)
  extends NodeSchemaSourceBase(node, nameHint, optTmpDir) {
  override val blameName =
    if (nameHint != "") "unittest:" + nameHint
    else tempURI.toString
}

/**
 * Used by TDML runner for embedded schemas- the schema node is constructed out of the TDML file
 * which in order to be able to validate repeatedly and such, is written to a temp file.
 */
case class EmbeddedSchemaSource(node: Node, nameHint: String, optTmpDir: Option[File] = None)
  extends NodeSchemaSourceBase(node, nameHint, optTmpDir) {
  override val blameName = tempURI.toString
}
