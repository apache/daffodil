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

package org.apache.daffodil.example

import org.apache.daffodil.sapi.logger._

import scala.collection.mutable.ListBuffer

class LogWriterForSAPITest2 extends LogWriter {
  val errors = new ListBuffer[String]()
  val warnings = new ListBuffer[String]()
  val infos = new ListBuffer[String]()
  val others = new ListBuffer[String]()

  def write(level: LogLevel.Value, logID: String, msg: String) {
    level match {
      case LogLevel.Error => errors.append(msg)
      case LogLevel.Warning => warnings.append(msg)
      case LogLevel.Info => infos.append(msg)
      case _ => others.append(msg)
    }
  }

  override def prefix(level: LogLevel.Value, logID: String): String = {
    val prefix = level match {
      case LogLevel.Error => "[error] "
      case LogLevel.Warning => "[warning] "
      case LogLevel.Info => "[info] "
      case LogLevel.Compile => "[compile] "
      case LogLevel.Debug => "[debug] "
      case LogLevel.DelimDebug => "[delimdebug] "
      case LogLevel.OOLAGDebug => "[oolagdebug] "
      case _ => "[unknown] "
    }
    "[SAPI LOG] " + prefix
  }

  override def suffix(level: LogLevel.Value, logID: String): String = {
    " [END]"
  }

}
