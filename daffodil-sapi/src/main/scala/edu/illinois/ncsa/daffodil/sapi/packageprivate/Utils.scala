/* Copyright (c) 2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.sapi.packageprivate
// The compiler plugin that converts Scala code to Java code for Javadoc
// generation has no concept of package private. So if there is anything that
// should be package private and thus not part of the public API (e.g.
// utilities for converting from Scala to Java), there isn't a way to exclude
// them from Javadocs. So, when we generate Javadocs, we manually exclude
// anything in the sapi.packageprivate package. So anything that should be package
// private should go in this package.

import edu.illinois.ncsa.daffodil.sapi._
import edu.illinois.ncsa.daffodil.sapi.logger._
import edu.illinois.ncsa.daffodil.sapi.debugger._
import edu.illinois.ncsa.daffodil.sapi.infoset._
import edu.illinois.ncsa.daffodil.api.{ ValidationMode => SValidationMode }
import edu.illinois.ncsa.daffodil.util.{ LogLevel => SLogLevel }
import edu.illinois.ncsa.daffodil.util.{ LogWriter => SLogWriter }
import edu.illinois.ncsa.daffodil.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import edu.illinois.ncsa.daffodil.debugger.{ InteractiveDebuggerRunner => SInteractiveDebuggerRunner }
import edu.illinois.ncsa.daffodil.infoset.{ InfosetOutputter => SInfosetOutputter }
// TODO: Not sure about this DISimple stuff. Should API users have this deep access to our internal infoset?
import edu.illinois.ncsa.daffodil.infoset.DISimple
import edu.illinois.ncsa.daffodil.infoset.DIComplex
import edu.illinois.ncsa.daffodil.infoset.DIArray

private[sapi] object LoggingConversions {

  def levelToScala(lvl: LogLevel.Value): SLogLevel.Type = {
    val slvl: SLogLevel.Type = lvl match {
      case LogLevel.Error => SLogLevel.Error
      case LogLevel.Warning => SLogLevel.Warning
      case LogLevel.Info => SLogLevel.Info
      case LogLevel.Compile => SLogLevel.Compile
      case LogLevel.Resolver => SLogLevel.Resolver
      case LogLevel.Debug => SLogLevel.Debug
      case LogLevel.OOLAGDebug => SLogLevel.OOLAGDebug
      case LogLevel.DelimDebug => SLogLevel.DelimDebug
    }
    slvl
  }

  def levelFromScala(slvl: SLogLevel.Type): LogLevel.Value = {
    val lvl: LogLevel.Value = slvl match {
      case SLogLevel.Error => LogLevel.Error
      case SLogLevel.Warning => LogLevel.Warning
      case SLogLevel.Info => LogLevel.Info
      case SLogLevel.Resolver => LogLevel.Resolver
      case SLogLevel.Compile => LogLevel.Compile
      case SLogLevel.Debug => LogLevel.Debug
      case SLogLevel.OOLAGDebug => LogLevel.OOLAGDebug
      case SLogLevel.DelimDebug => LogLevel.DelimDebug
    }
    lvl
  }
}

private[sapi] object ValidationConversions {

  def modeToScala(mode: ValidationMode.Value): SValidationMode.Type = {
    val smode: SValidationMode.Type = mode match {
      case ValidationMode.Off => SValidationMode.Off
      case ValidationMode.Limited => SValidationMode.Limited
      case ValidationMode.Full => SValidationMode.Full
    }
    smode
  }

  def modeFromScala(smode: SValidationMode.Type): ValidationMode.Value = {
    val mode: ValidationMode.Value = smode match {
      case SValidationMode.Off => ValidationMode.Off
      case SValidationMode.Limited => ValidationMode.Limited
      case SValidationMode.Full => ValidationMode.Full
    }
    mode
  }
}

/* A wrapper log writer that scala logging can talk to, which is then forwarded
 * onto the java LogWriter, if a user implements their own log writer in java.
 */
private[sapi] class JavaLogWriter(logWriter: LogWriter)
    extends SLogWriter {

  protected def write(msg: String): Unit = {}

  override def log(lvl: SLogLevel.Type, logID: String, msg: String, args: Seq[Any]) {
    if (logWriter != null) {
      logWriter.log(LoggingConversions.levelFromScala(lvl), logID, msg, args)
    }
  }
}

/* A wrapper interctive debugger that scala debugging can talk to, which is
 * then forwarded onto the java interactive debugger, if a user implements
 * their own debugger in java.
 */
private[sapi] class JavaInteractiveDebuggerRunner(dr: DebuggerRunner)
    extends SInteractiveDebuggerRunner {
  def init(id: SInteractiveDebugger): Unit = dr.init
  def getCommand: String = dr.getCommand
  def lineOutput(line: String): Unit = dr.lineOutput(line)
  def fini(): Unit = dr.fini
}

/* A wrapper for existing infoset outputters */
private[sapi] abstract class InfosetOutputterWrapper extends InfosetOutputter {

  protected val infosetOutputter: SInfosetOutputter

  def reset(): Unit = infosetOutputter.reset()

  def startDocument(): Boolean = infosetOutputter.startDocument()
  def endDocument(): Boolean = infosetOutputter.endDocument()

  def startSimple(diSimple: DISimple): Boolean = infosetOutputter.startSimple(diSimple)
  def endSimple(diSimple: DISimple): Boolean = infosetOutputter.endSimple(diSimple)

  def startComplex(diComplex: DIComplex): Boolean = infosetOutputter.startComplex(diComplex)
  def endComplex(diComplex: DIComplex): Boolean = infosetOutputter.endComplex(diComplex)

  def startArray(diArray: DIArray): Boolean = infosetOutputter.startArray(diArray)
  def endArray(diArray: DIArray): Boolean = infosetOutputter.endArray(diArray)
}
