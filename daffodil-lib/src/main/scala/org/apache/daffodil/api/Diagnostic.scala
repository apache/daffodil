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

package org.apache.daffodil.api

import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.exceptions.SchemaFileLocation

/**
 * Base class for all "thin" (no stack trace) diagnostic objects.
 * Such as processing errors.
 */
abstract class ThinDiagnostic(
  schemaContext: Maybe[SchemaFileLocation],
  dataContext: Maybe[DataLocation],
  maybeCause: Maybe[Throwable],
  maybeFormatString: Maybe[String],
  args: Any*)
  extends Diagnostic(
    false, // thin, i.e., not isThick
    schemaContext,
    dataContext,
    maybeCause,
    maybeFormatString,
    args: _*) {
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
  private val dataContext: Maybe[DataLocation],
  private val maybeCause: Maybe[Throwable],
  private val maybeFormatString: Maybe[String],
  private val args: Any*)
  extends Exception(
    null,
    (if (maybeCause.isDefined) maybeCause.get else null),
    isThick,
    isThick) {

  /**
   * Constructor for Thick diagnostics (with stack trace).
   * Use for fatal errors.
   */
  def this(
    schemaContext: Maybe[SchemaFileLocation],
    dataContext: Maybe[DataLocation],
    maybeCause: Maybe[Throwable],
    maybeFormatString: Maybe[String],
    args: Any*) = this(
    true, // isThick
    schemaContext, dataContext, maybeCause, maybeFormatString, args: _*)

  final def toss =
    throw this // good place for a breakpoint.

  def isValidation = false

  /**
   * These are put into a collection to remove duplicates so equals and hash
   * matter or we'll get duplicates we don't want.
   */
  override def equals(b: Any): Boolean = {
    b match {
      case other: Diagnostic => (this eq other) || {
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

  override def toString() = getMessage

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   */
  def isError: Boolean

  protected def componentText: String = ""

  /**
   * Define as "Parse", "Unparse", "Schema Definition", "Configuration".
   *
   * This is combined with the word "Error" or "Warning"
   */
  protected def modeName: String

  private def errorOrWarning =
    if (isError) "Error" else "Warning"

  /**
   * Get data location information relevant to this diagnostic object.
   *
   * For example, this might be a file name, and position within the file.
   */
  def getDataLocations: Seq[DataLocation] = dataContext.toSeq

  /**
   * Get schema location information relevant to this diagnostic object.
   *
   * For example, this might be a file name of a schema, and position within the schema file.
   */
  def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = schemaContext.toSeq

  /**
   * Positively get these things. No returning 'null' and making caller figure out
   * whether to look for cause object.
   */
  final def getSomeCause: Some[Throwable] = Misc.getSomeCause(this)
  final def getSomeMessage: Some[String] = Misc.getSomeMessage(this)

  private def init: Unit = {
    Assert.invariant(maybeCause.isDefined ^ maybeFormatString.isDefined)
    Assert.invariant(maybeCause.isEmpty || args.length == 0) // if there is a cause, there can't be args.
  }

  private def schemaLocationsString = {
    val strings = getLocationsInSchemaFiles.map { _.locationDescription }
    val res = if (strings.length > 0)
      " " + strings.mkString(", ")
    else
      " (no schema file location)"
    res
  }

  protected def schemaContextString =
    if (schemaContext.isEmpty) ""
    else {
      val pn = schemaContext.get.diagnosticDebugName
      "\nSchema context: %s%s".format(pn, schemaLocationsString)
    }

  private def dataLocationString =
    if (dataContext.isEmpty) ""
    else
      "\nData location was preceding %s".format(dataContext.value)
  //
  // Right here is where we would lookup the symbolic error kind id, and
  // choose a locale-based message string.
  //
  // For now, we'll just do an automatic English message.
  //
  private def msgString: String = {
    Assert.invariant(maybeFormatString.isDefined)
    val m =
      if (args.size > 0) {
        try {
          maybeFormatString.get.format(args: _*)
        } catch {
          case e: IllegalArgumentException =>
            Assert.abort(e.getMessage() + """\nFormat string "%s" did not accept these arguments: %s.""".format(maybeFormatString.get, args.mkString(", ")))
        }
      } else maybeFormatString.get
    m
  }

  private def msgCausedBy: String = {
    Assert.invariant(maybeCause.isDefined)
    maybeCause.get match {
      case d: Diagnostic => d.getSomeMessage.get
      case th => Misc.getSomeMessage(th).get
    }
  }

  private lazy val message = {
    init
    val res = modeName + " " + errorOrWarning + ": " +
      (if (maybeCause.isDefined) msgCausedBy else msgString) +
      componentText +
      schemaContextString +
      dataLocationString
    res
  }
}

/**
 * Relevant data location for a diagnostic message. E.g., file and line number.
 */
trait DataLocation {
  def toString: String
  def isAtEnd: Boolean
  def bitPos1b: Long
  def bytePos1b: Long
}

/**
 * Relevant schema location for a diagnostic message. E.g., file and line number.
 */
trait LocationInSchemaFile {

  def lineDescription: String

  def columnDescription: String

  def fileDescription: String

  def locationDescription: String

}

/**
 * Mix into classes that can carry diagnostic information as part of their structure.
 */
trait WithDiagnostics {

  /**
   * If multiple diagnostic messages can be created by an action, then this
   * returns a sequence of multiple diagnostic objects. If the message is
   * a fatal runtime issue, then this might be a singleton list, or it could be
   * a bunch of warnings followed by a fatal runtime error.
   *
   * The order of the sequence is important. When the diagnostics are about
   * a file of text, then diagnostics that are about lines earlier in the file
   * are earlier in the list.
   */
  def getDiagnostics: Seq[Diagnostic]

  /**
   * This predicate indicates whether the object in question succeeded or failed
   * at whatever some action was trying to do. That is to say,
   * do the diagnostics contain a hard error, or do the diagnostics
   * only contain warnings and/or advisory content. If false then only warnings
   * and other non-fatal diagnostics have appeared, so subsequent actions can
   * proceed.
   *
   * The classic example of this is compilation. If only warnings were produced
   * then one can proceed to run the compiled entity.
   */
  def isError: Boolean
}
