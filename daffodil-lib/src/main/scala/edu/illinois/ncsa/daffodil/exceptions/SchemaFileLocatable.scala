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

package edu.illinois.ncsa.daffodil.exceptions

import java.net.URLDecoder
import scala.xml.Node
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.dsom.LookupLocation
import edu.illinois.ncsa.daffodil.util.TransientParam

object NoSchemaFileLocation extends SchemaFileLocation(NotLocatable)

trait HasSchemaFileLocation extends LookupLocation {
  def schemaFileLocation: SchemaFileLocation
  override def lineDescription: String = schemaFileLocation.lineDescription

  override def columnDescription: String = schemaFileLocation.columnDescription

  override def fileDescription: String = schemaFileLocation.fileDescription

  override def locationDescription: String = schemaFileLocation.locationDescription
}

class SchemaFileLocation(@TransientParam context: SchemaFileLocatable) extends LocationInSchemaFile with Serializable {

  val lineNumber = context.lineNumber
  val columnNumber = context.columnNumber
  val uriString: String = context.uriString

  override def lineDescription = lineNumber match {
    case Some(num) => " line " + num
    case None => ""
  }

  override def columnDescription = columnNumber match {
    case Some(num) => " column " + num
    case None => ""
  }

  override val toString = context.toString

  override def fileDescription = " in " + URLDecoder.decode(uriString, "UTF-8")

  override def locationDescription = {
    val showInfo = lineDescription != "" || fileDescription != ""
    val info = lineDescription + columnDescription + fileDescription
    val txt = if (showInfo) "Location" + info else ""
    txt
  }
}

object NotLocatable extends SchemaFileLocatable {
  def lineAttribute: Option[String] = None
  def columnAttribute: Option[String] = None
  def fileAttribute: Option[String] = None
  def uriString: String = "NotLocatable"
  def namespaces: scala.xml.NamespaceBinding = scala.xml.TopScope
  def SDE(id: String, args: Any*): Nothing = ???
}

trait SchemaFileLocatable extends LocationInSchemaFile
  with HasSchemaFileLocation {
  def lineAttribute: Option[String]
  def columnAttribute: Option[String]
  def fileAttribute: Option[String]

  lazy val lineNumber: Option[String] = lineAttribute match {
    case Some(seqNodes) => Some(seqNodes.toString)
    case None => None
  }

  override lazy val lineDescription = lineNumber match {
    case Some(num) => " line " + num
    case None => ""
  }

  lazy val columnNumber = columnAttribute match {
    case Some(seqNodes) => Some(seqNodes.toString)
    case None => None
  }

  override lazy val columnDescription = columnNumber match {
    case Some(num) => " column " + num
    case None => ""
  }

  // URLDecoder removes %20, etc from the file name.
  override lazy val fileDescription = " in " + URLDecoder.decode(uriString, "UTF-8")

  override lazy val locationDescription = {
    val showInfo = lineDescription != "" || fileDescription != ""
    val info = lineDescription + columnDescription + fileDescription
    val txt = if (showInfo) "Location" + info else ""
    txt
  }

  /**
   * It would appear that this is only used for informational purposes
   * and as such, doesn't need to be a URL.  Can just be String.
   *
   * override if you don't have a fileName attribute appended
   * but are in a context where some enclosing construct does
   * normally only a root node would have a file attribute.
   *
   * implement as
   * @example {{{
   *     lazy val uriString = uriStringFromAttribute().getOrElse("unknown")
   * }}}
   * or delegate like
   * @example {{{
   *     lazy val uriString = schemaDocument.uriString
   * }}}
   */
  def uriString: String

  lazy val uriStringFromAttribute = {
    fileAttribute match {
      case Some(seqNodes) => Some(seqNodes.toString)
      case None => None
    }

  }

  override lazy val schemaFileLocation = new SchemaFileLocation(this)
}

