package edu.illinois.ncsa.daffodil.api
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.HasIsError
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

/**
 * Base trait for all error, warning, info, and other sorts of objects
 * that capture diagnostic information.
 *
 * Allows for lazy message creation, internationalization, etc.
 */
trait Diagnostic extends Throwable with HasIsError {

  /**
   * Turns the diagnostic object into a string.
   *
   * Should utilize locale information to properly internationalize. But if that is
   * unavailable, will still construct an English-language string.
   */
  def getMessage: String

  override def toString() = getMessage
  /**
   * Get data location information relevant to this diagnostic object.
   *
   * For example, this might be a file name, and position within the file.
   */
  def getDataLocations: Seq[DataLocation]

  /**
   * Get schema location information relevant to this diagnostic object.
   *
   * For example, this might be a file name of a schema, and position within the schema file.
   */
  def getLocationsInSchemaFiles: Seq[LocationInSchemaFile]

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   */
  def isError: Boolean

  /**
   * Positively get these things. No returning 'null' and making caller figure out
   * whether to look for cause object.
   */
  def getSomeCause: Some[Throwable]
  def getSomeMessage: Some[String]

}

/**
 * Relevant data location for a diagnostic message. E.g., file and line number.
 */
trait DataLocation {
  def toString: String
  def isAtEnd: Boolean
  def bitPos: Long
  def bytePos: Long
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
   * only contain warnings and/or advisory content. If true then only warnings
   * and other non-fatal diagnostics have appeared, so subsequent actions can
   * proceed.
   *
   * The classic example of this is compilation. If only warnings were produced
   * then one can proceed to run the compiled entity.
   *
   * This list is lazily constructed, so asking a compiler for its diagnostics forces
   * the completion of all compilation so that any diagnostics will have been
   * created. That is, this isn't for polling for diagnostics or anything like that.
   */
  final lazy val canProceed: Boolean = !isError
  def isError: Boolean
  /**
   * Indicates whether there are any diagnostic objects available.
   */
  // def hasDiagnostics : Boolean
}
