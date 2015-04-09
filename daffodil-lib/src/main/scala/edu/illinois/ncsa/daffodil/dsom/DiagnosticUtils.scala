/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.util.Misc

object DiagnosticUtils {
  /**
   * Java throwable/exception objects may or may not have a message. They are supposed to have a cause if they
   * don't have a message of their own, but might have neither, or might have both.
   *
   * This is too painful to deal with in code when you want to be generic about converting throws/exceptions
   * into diagnostic information.
   *
   * So we have a more uniform behavior. Never returns null. Always gets a message.
   * If the argument has none, but has a cause object, then it
   * gets the message from that, if that has no message, it chases further.
   * Ultimately, if there's no message, it just uses the innermost cause object's class name.
   */

  def getSomeMessage(th: Throwable): Some[String] = {
    val m = th.getMessage()
    val c = th.getCause()
    val res = (m, c) match {
      case (null, null) => Misc.getNameFromClass(th)
      case ("", null) => Misc.getNameFromClass(th)
      case (m, null) => m
      case (null, c) => getSomeMessage(c).get
      case (m, c) => {
        val Some(cmsg) = getSomeMessage(c)
        cmsg + " (within " + m + ")"
      }
    }
    Some(res)
  }

  def getSomeCause(th: Throwable): Some[Throwable] = {
    val c = th.getCause()
    val res = c match {
      case null => th
      case _ => getSomeCause(c).get
    }
    Some(res)
  }
}

trait DiagnosticImplMixin
  extends Diagnostic {
  def getSomeCause() = DiagnosticUtils.getSomeCause(this)
  def getSomeMessage() = DiagnosticUtils.getSomeMessage(this)
  def getLocationsInSchemaFiles(): Seq[LocationInSchemaFile] = Nil
  def getDataLocations(): Seq[DataLocation] = Nil
  def isError = true
}