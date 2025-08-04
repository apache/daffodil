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

import scala.jdk.CollectionConverters.*

import org.apache.daffodil.api
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Misc

/**
 * Base class for all "thin" (no stack trace) diagnostic objects.
 * Such as processing errors.
 */
abstract class ThinDiagnostic(
  schemaContext: Maybe[SchemaFileLocation],
  dataContext: Maybe[api.DataLocation],
  maybeCause: Maybe[Throwable],
  maybeFormatString: Maybe[String],
  args: Any*
) extends Diagnostic(
    false, // thin, i.e., not isThick
    schemaContext,
    dataContext,
    maybeCause,
    maybeFormatString,
    args*
  ) {
  Assert.invariant(maybeCause.isDefined || maybeFormatString.isDefined)
}

/**
 * Base class for all thick or thin error, warning, info, and other sorts of objects
 * that capture diagnostic information. Such as Schema Definition Errors/Warnings.
 *
 * Allows for lazy message creation, internationalization, etc.
 */
abstract class Diagnostic protected (
  isThick: Boolean,
  private val schemaContext: Maybe[SchemaFileLocation],
  private val dataContext: Maybe[api.DataLocation],
  private val maybeCause: Maybe[Throwable],
  private val maybeFormatString: Maybe[String],
  private val args: Any*
) extends api.Diagnostic(
    maybeFormatString.orNull,
    maybeCause.orNull,
    isThick,
    isThick
  ) {

  /**
   * Constructor for Thick diagnostics (with stack trace).
   * Use for fatal errors.
   */
  def this(
    schemaContext: Maybe[SchemaFileLocation],
    dataContext: Maybe[api.DataLocation],
    maybeCause: Maybe[Throwable],
    maybeFormatString: Maybe[String],
    args: Any*
  ) = this(
    true, // isThick
    schemaContext,
    dataContext,
    maybeCause,
    maybeFormatString,
    args*
  )

  final def toss =
    throw this // good place for a breakpoint.

  def isValidation = false

  /**
   * These are put into a collection to remove duplicates so equals and hash
   * matter or we'll get duplicates we don't want.
   */
  override def equals(b: Any): Boolean = {
    b match {
      case other: Diagnostic =>
        (this eq other) || {
          schemaContext == other.schemaContext &&
          dataContext == other.dataContext &&
          maybeCause == other.maybeCause &&
          maybeFormatString == other.maybeFormatString &&
          args == other.args &&
          isError == other.isError &&
          modeName == other.modeName
        }
      case _ => false
    }
  }

  override def hashCode = {
    schemaContext.hashCode +
      dataContext.hashCode +
      maybeCause.hashCode +
      maybeFormatString.hashCode +
      args.hashCode +
      isError.hashCode +
      modeName.hashCode
  }

  /**
   * Turns the diagnostic object into a string.
   *
   * Should utilize locale information to properly internationalize. But if that is
   * unavailable, will still construct an English-language string.
   */
  override def getMessage(): String = message

  override def getMessageOnly: String = message

  override def toString() =
    getModeName() + ": " + getMessage + schemaContextString + dataLocationString

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   */
  def isError: Boolean

  /**
   * Define as "Parse", "Unparse", "Schema Definition", "Configuration".
   *
   * This is combined with the word "Error" or "Warning"
   */
  protected def modeName: String

  def getModeName(): String = {
    val errorOrWarning = if (isError) "Error" else "Warning"
    modeName + " " + errorOrWarning
  }

  /**
   * Get data location information relevant to this diagnostic object.
   *
   * For example, this might be a file name, and position within the file.
   *
   * @return [[org.apache.daffodil.api.DataLocation]]
   */
  def getDataLocations: java.util.List[api.DataLocation] =
    dataContext.toSeq.asJava

  /**
   * Get schema location information relevant to this diagnostic object.
   *
   * For example, this might be a file name of a schema, and position within the schema file.
   *
   * @return [[org.apache.daffodil.lib.iapi.LocationInSchemaFile]]
   */
  def getLocationsInSchemaFiles: java.util.List[? <: api.LocationInSchemaFile] =
    schemaContext.toSeq.asJava

  private def schemaLocationsString = {
    val strings = getLocationsInSchemaFiles.asScala.map { _.locationDescription }
    val res =
      if (strings.nonEmpty)
        " " + strings.mkString(", ")
      else
        " (no schema file location)"
    res
  }

  protected def schemaContextString =
    if (schemaContext.isEmpty) ""
    else {
      val ddn = schemaContext.get.diagnosticDebugName
      val pn = if (ddn.nonEmpty) " " + ddn else ""
      "\nSchema context:%s%s".format(pn, schemaLocationsString)
    }

  private def dataLocationString =
    if (dataContext.isEmpty) ""
    else
      "\nData location was preceding %s".format(dataContext.value)

  private lazy val message = {
    if (getCause != null) {
      getCause match {
        case d: Diagnostic => d.getModeName() + ": " + d.getMessage()
        case th => Misc.getSomeMessage(th).get
      }
    } else {
      Assert.invariant(maybeFormatString.isDefined)
      val m =
        if (args.size > 0) {
          try {
            maybeFormatString.get.format(args*)
          } catch {
            case e: IllegalArgumentException =>
              Assert.abort(
                e.getMessage() + """\nFormat string "%s" did not accept these arguments: %s."""
                  .format(maybeFormatString.get, args.mkString(", "))
              )
          }
        } else maybeFormatString.get
      m
    }
  }
}

/**
 * Relevant schema location for a diagnostic message. E.g., file and line number.
 */
trait LocationInSchemaFile extends api.LocationInSchemaFile {
  override def toString: String = {
    asString()
  }
}

/**
 * Mix into classes that can carry diagnostic information as part of their structure.
 */
trait WithDiagnostics extends api.WithDiagnostics {

  /**
   * Helper method to check that isError is false, if not it throws
   * a usage error caused by illegal state caused by a compilation error
   */
  def checkNotError(): Unit = {
    Assert.usageWithCause(
      !isError,
      new IllegalStateException(
        "Must call isError() to ensure there are no errors",
        getDiagnostics.asScala.find(_.isError).get
      )
    )
  }
}
