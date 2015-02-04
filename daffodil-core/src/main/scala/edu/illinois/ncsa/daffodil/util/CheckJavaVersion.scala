/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

object CheckJavaVersion {

  def minMajorJVersion = 1
  def minMinorJVersion = 7
  def checkJavaVersion(context: ThrowsSDE) = {
    val jVersion = {
      try { System.getProperty("java.version") }
      catch {
        case se: SecurityException => context.SDE("Attempted to read property 'java.version' failed due to a SecurityException: \n%s".format(se.getMessage()))
        case _: Throwable => context.SDE("An invalid 'key' was passed to System.getProperty.")
      }
    }
    val javaVersion = """([0-9])\.([0-9])\.(.*)""".r
    jVersion match {
      case javaVersion(major, minor, x) => {

        if (major.toInt < minMajorJVersion) {
          context.SDE("You must run Java 7 (1.7) or higher. You are currently running %s".format(jVersion))
        }
        if (minor.toInt < minMinorJVersion) {
          context.SDE("You must run Java 7 (1.7) or higher. You are currently running %s".format(jVersion))
        }
      }
      case _ => {
        context.SDE("Failed to obtain the Java version.  You must run Java 7 (1.7) or higher.")
      }
    }
  }

}