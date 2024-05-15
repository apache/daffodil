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

package org.apache.daffodil.lib.exceptions

import java.io.File
import scala.xml.SAXParseException

import org.apache.daffodil.lib.api.LocationInSchemaFile
import org.apache.daffodil.lib.schema.annotation.props.LookupLocation

trait HasSchemaFileLocation extends LookupLocation {
  override def schemaFileLocation: SchemaFileLocation
  override def lineDescription: String = schemaFileLocation.lineDescription

  override def columnDescription: String = schemaFileLocation.columnDescription

  override def fileDescription: String = schemaFileLocation.fileDescription

  override def locationDescription: String = schemaFileLocation.locationDescription
}

object SchemaFileLocation {
  def apply(context: SchemaFileLocatable) =
    new SchemaFileLocation(
      context.lineNumber,
      context.columnNumber,
      context.uriString,
      context.diagnosticFile,
      context.toString,
      context.diagnosticDebugName,
    )
}

class SchemaFileLocation protected (
  val lineNumber: Option[String],
  val columnNumber: Option[String],
  val uriString: String,
  val diagnosticFile: File,
  contextToString: String,
  val diagnosticDebugName: String,
) extends LocationInSchemaFile
  with Serializable {

  override def lineDescription = lineNumber match {
    case Some(num) => " line " + num
    case None => ""
  }

  override def columnDescription = columnNumber match {
    case Some(num) => " column " + num
    case None => ""
  }

  override val toString = contextToString

  override def fileDescription = " in " + diagnosticFile

  override def locationDescription = {
    val showInfo = lineDescription != "" || fileDescription != ""
    val info = lineDescription + columnDescription + fileDescription
    val txt = if (showInfo) "Location" + info else ""
    txt
  }
}

trait SchemaFileLocatable extends LocationInSchemaFile with HasSchemaFileLocation {
  def lineAttribute: Option[String]
  def columnAttribute: Option[String]
  def fileAttribute: Option[String]

  def diagnosticDebugName: String

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

  override lazy val fileDescription = {
    " in " + diagnosticFile
  }

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

  def diagnosticFile: File

  lazy val uriStringFromAttribute = {
    fileAttribute match {
      case Some(seqNodes) => Some(seqNodes.toString)
      case None => None
    }

  }

  override lazy val schemaFileLocation = SchemaFileLocation(this)
}

class XercesSchemaFileLocation(
  val xercesError: SAXParseException,
  val schemaFileLocation: SchemaFileLocation,
) extends SchemaFileLocation(
    Option(xercesError.getLineNumber.toString),
    Option(xercesError.getColumnNumber.toString),
    xercesError.getSystemId,
    schemaFileLocation.diagnosticFile,
    schemaFileLocation.toString,
    schemaFileLocation.diagnosticDebugName,
  ) {
  // we set this to blank string instead of "Schema File" since we don't have access to the element
  // that causes this error from Xerces and "Schema File" doesn't really add much more info compared
  // to the blank string
  override val diagnosticDebugName = ""

  // we have to override equals and hashCode because the OOlag error checks for duplicates in its error list
  override def equals(obj: Any): Boolean = {
    val xsflObj = obj.asInstanceOf[XercesSchemaFileLocation]
    xsflObj.xercesError.getLineNumber == this.xercesError.getLineNumber &&
    xsflObj.xercesError.getColumnNumber == this.xercesError.getColumnNumber &&
    xsflObj.xercesError.getSystemId == this.xercesError.getSystemId &&
    xsflObj.schemaFileLocation == this.schemaFileLocation
  }

  override def hashCode: Int = {
    this.xercesError.hashCode() +
      this.schemaFileLocation.hashCode()
  }
}
